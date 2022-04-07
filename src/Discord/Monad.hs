{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}

module Discord.Monad
  ( MonadDiscord (..),
    EnvRunDiscordOpts (..),
    RunDiscordOpts,
    runDiscord,
    runDiscordM,
  )
where

import Control.Monad.Identity (IdentityT)
import Control.Monad.Reader
import Data.Aeson
import Data.Default
import Data.IORef (writeIORef)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Discord.Handle
import Discord.Internal.Gateway
import Discord.Internal.Rest
import Discord.Requests
import UnliftIO (IOException, MonadUnliftIO, SomeException, finally, race, try)
import UnliftIO.Concurrent
import Prelude hiding (log)

class MonadUnliftIO m => MonadDiscord d m | m -> d where
  restCall :: (FromJSON a, Request (r a)) => r a -> m (Either RestCallErrorCode a)
  sendCommand :: GatewaySendable -> m ()
  readCache :: m (Cache d)
  stopDiscord :: m ()
  modifyCacheData :: (Maybe d -> Maybe d) -> m ()

instance {-# OVERLAPPING #-} MonadDiscord d (EnvDiscordHandler d) where
  restCall = restCall'
  sendCommand = sendCommand'
  readCache = readCache'
  stopDiscord = stopDiscord'
  modifyCacheData = modifyCacheData'

instance MonadDiscord d m => MonadDiscord d (IdentityT m) where
  restCall = lift . restCall
  sendCommand = lift . sendCommand
  readCache = lift readCache
  stopDiscord = lift stopDiscord
  modifyCacheData = lift . modifyCacheData

instance MonadDiscord d m => MonadDiscord d (ReaderT r m) where
  restCall = lift . restCall
  sendCommand = lift . sendCommand
  readCache = lift readCache
  stopDiscord = lift stopDiscord
  modifyCacheData = lift . modifyCacheData

-- | Execute one http request and get a response
restCall' :: (FromJSON a, Request (r a)) => r a -> EnvDiscordHandler d (Either RestCallErrorCode a)
restCall' r = do
  h <- ask
  empty <- isEmptyMVar (discordHandleLibraryError h)
  if not empty
    then pure (Left (RestCallErrorCode 400 "Library Stopped Working" ""))
    else do
      resp <- liftIO $ writeRestCall (discordHandleRestChan h) r
      case resp of
        Right x -> pure (Right x)
        Left (RestCallInternalErrorCode c e1 e2) -> do
          pure (Left (RestCallErrorCode c (TE.decodeUtf8 e1) (TE.decodeUtf8 e2)))
        Left (RestCallInternalHttpException _) ->
          threadDelay (10 * 10 ^ (6 :: Int)) >> restCall' r
        Left (RestCallInternalNoParse err dat) -> do
          let formaterr =
                T.pack
                  ( "restcall - parse exception [" <> err <> "]"
                      <> " while handling"
                      <> show dat
                  )
          writeChan (discordHandleLog h) formaterr
          pure (Left (RestCallErrorCode 400 "Library Parse Exception" formaterr))

-- | Send a user GatewaySendable
sendCommand' :: GatewaySendable -> EnvDiscordHandler d ()
sendCommand' e = do
  dhg <- asks discordHandleGateway
  writeChan (gatewayHandleUserSendables dhg) e
  case e of
    UpdateStatus opts -> liftIO $ writeIORef (gatewayHandleLastStatus dhg) (Just opts)
    _ -> pure ()

-- | Access the current state of the gateway cache
readCache' :: EnvDiscordHandler d (Cache d)
readCache' = do
  cache <- asks discordHandleCache
  merr <- readMVar (cacheHandleCache cache)
  case merr of
    Left (c, _) -> pure c
    Right c -> pure c

-- | Modify the user cache.
modifyCacheData' :: forall d. (Maybe d -> Maybe d) -> EnvDiscordHandler d ()
modifyCacheData' f = do
  cache <- asks discordHandleCache
  modifyMVar_ (cacheHandleCache cache) (pure . modifyCache)
  where
    modifyCache :: Either (Cache d, GatewayException) (Cache d) -> Either (Cache d, GatewayException) (Cache d)
    modifyCache = \case
      Left (c,ge) -> Left (c {cacheData = f (cacheData c)}, ge)
      Right c -> Right (c {cacheData = f (cacheData c)})

-- | Stop all the background threads
stopDiscord' :: EnvDiscordHandler d ()
stopDiscord' = do
  h <- ask
  _ <- tryPutMVar (discordHandleLibraryError h) "Library has closed"
  threadDelay (10 ^ (6 :: Int) `div` 10)
  mapM_ (killThread . toId) (discordHandleThreads h)
  where
    toId t = case t of
      HandleThreadIdRest a -> a
      HandleThreadIdGateway a -> a
      HandleThreadIdCache a -> a
      HandleThreadIdLogger a -> a

-- | Run a discord bot from the given options.
runDiscord :: RunDiscordOpts -> IO T.Text
runDiscord = runDiscordM id

-- | Run a discord bot from the given options, as well as a way to unlift the
-- MonadDiscord given.
--
-- Gets the handle for `runDiscordLoopM`.
runDiscordM :: MonadDiscord d m => (forall a. m a -> EnvDiscordHandler d a) -> EnvRunDiscordOpts d m -> IO T.Text
runDiscordM unlift opts = do
  log <- newChan
  logId <- liftIO $ startLogger (discordOnLog opts) log
  (cache, cacheId) <- liftIO $ startCacheThread log
  (rest, restId) <- liftIO $ startRestThread (Auth (discordToken opts)) log
  (gate, gateId) <- liftIO $ startGatewayThread (Auth (discordToken opts)) (discordGatewayIntent opts) cache log

  libE <- newEmptyMVar

  let handle =
        DiscordHandle
          { discordHandleRestChan = rest,
            discordHandleGateway = gate,
            discordHandleCache = cache,
            discordHandleLog = log,
            discordHandleLibraryError = libE,
            discordHandleThreads =
              [ HandleThreadIdLogger logId,
                HandleThreadIdRest restId,
                HandleThreadIdCache cacheId,
                HandleThreadIdGateway gateId
              ]
          }

  finally
    (runDiscordLoopM unlift handle opts)
    (discordOnEnd opts >> runReaderT stopDiscord handle)

-- | Run a discord bot from the given options, as well as a way to unlift the
-- MonadDiscord given.
runDiscordLoopM :: forall m d. MonadDiscord d m => (forall a. m a -> EnvDiscordHandler d a) -> EnvDiscordHandle d -> EnvRunDiscordOpts d m -> IO T.Text
runDiscordLoopM unlift handle opts = do
  resp <- liftIO $ writeRestCall (discordHandleRestChan handle) GetCurrentUser
  case resp of
    Left (RestCallInternalErrorCode c e1 e2) ->
      libError $
        "HTTP Error Code " <> T.pack (show c) <> " " <> TE.decodeUtf8 e1
          <> " "
          <> TE.decodeUtf8 e2
    Left (RestCallInternalHttpException e) -> libError ("HTTP Exception -  " <> T.pack (show e))
    Left (RestCallInternalNoParse _ _) -> libError "Couldn't parse GetCurrentUser"
    _ -> flip runReaderT handle . unlift $ do
      me <- try $ discordOnStart opts
      case me of
        Left (e :: SomeException) -> libError ("discordOnStart handler stopped on an exception:\n\n" <> T.pack (show e))
        Right _ -> loop
  where
    libError :: MonadIO n => T.Text -> n T.Text
    libError msg = tryPutMVar (discordHandleLibraryError handle) msg >> pure msg

    loop :: m T.Text
    loop = do
      next <-
        race
          (readMVar (discordHandleLibraryError handle))
          (readChan (gatewayHandleEvents (discordHandleGateway handle)))
      case next of
        Left err -> libError err
        Right (Left err) -> libError (T.pack (show err))
        Right (Right event) -> do
          let userEvent = userFacingEvent event
          let action =
                if discordForkThreadForEvents opts
                  then void . forkIO
                  else id
          action $ do
            me <- try $ discordOnEvent opts userEvent
            case me of
              Left (e :: SomeException) ->
                writeChan
                  (discordHandleLog handle)
                  ( "eventhandler - crashed on [" <> T.pack (show userEvent) <> "] "
                      <> "          with error: "
                      <> T.pack (show e)
                  )
              Right _ -> pure ()
          loop

data EnvRunDiscordOpts d m = RunDiscordOpts
  { discordToken :: T.Text,
    discordOnStart :: MonadDiscord d m => m (),
    discordOnEnd :: IO (),
    discordOnEvent :: MonadDiscord d m => Event -> m (),
    discordOnLog :: T.Text -> IO (),
    discordForkThreadForEvents :: Bool,
    discordGatewayIntent :: GatewayIntent
  }

type RunDiscordOpts = EnvRunDiscordOpts () DiscordHandler

instance Default (EnvRunDiscordOpts d m) where
  def =
    RunDiscordOpts
      { discordToken = "",
        discordOnStart = pure (),
        discordOnEnd = pure (),
        discordOnEvent = \_ -> pure (),
        discordOnLog = \_ -> pure (),
        discordForkThreadForEvents = True,
        discordGatewayIntent = def
      }

startLogger :: (T.Text -> IO ()) -> Chan T.Text -> IO ThreadId
startLogger handle logC = forkIO $
  forever $
    do
      me <- try $ readChan logC >>= handle
      case me of
        Right _ -> pure ()
        Left (_ :: IOException) ->
          -- writeChan logC "Log handler failed"
          pure ()
