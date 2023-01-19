{-# LANGUAGE OverloadedStrings #-}

-- | Provides logic code for interacting with the Discord websocket
--   gateway. Realistically, this is probably lower level than most
--   people will need
module Discord.Internal.Gateway.EventLoop where

import Prelude hiding (log)

import Control.Monad (forever, void)
import Control.Monad.Random (getRandomR)
import UnliftIO
import UnliftIO.Concurrent
import Data.Aeson (eitherDecode, encode)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL

import Wuss (runSecureClient)
import Network.Socket (HostName)
import Network.WebSockets (ConnectionException(..), Connection,
                           receiveData, sendTextData, sendClose)

import Discord.Internal.Types
import Discord.Internal.Rest.Prelude (apiVersion)

writeTChanIO :: MonadUnliftIO m => TChan a -> a -> m ()
writeTChanIO c = atomically . writeTChan c

writeToLog :: MonadUnliftIO m => LoggingChannel -> T.Text -> m ()
writeToLog c = atomically . writeTQueue c

writeTVarIO :: MonadUnliftIO m => TVar a -> a -> m ()
writeTVarIO t = atomically . writeTVar t

readTChanIO :: MonadUnliftIO m => TChan a -> m a
readTChanIO = atomically . readTChan

-- | Info the event processing loop needs to
data GatewayHandle = GatewayHandle
  { -- | Realtime events from discord
    gatewayHandleEvents         :: EventChannel,
    -- | Events the user sends to discord
    gatewayHandleUserSendables  :: TChan GatewaySendable,
    -- | Recent set status (resent to discord on reconnect)
    gatewayHandleLastStatus     :: TVar (Maybe UpdateStatusOpts),
    -- | Recent sent event sequence (used to reconnect)
    gatewayHandleLastSequenceId :: TVar Integer,
    -- | Which discord server session (used to reconnect)
    gatewayHandleSessionId      :: TVar T.Text,
    -- | Which discord gateway to connect to. This should contain a default value
    -- ("gateway.discord.gg") on first connect, but on subsequent Resumes this
    -- may contain a different value. This should never contain trailing slashes,
    -- or any "wss://" prefixes, since HostNames of this kind are not supported
    -- by the websockets library.
    gatewayHandleHostname       :: TVar HostName
  }

-- | Ways the gateway connection can fail with no possibility of recovery.
newtype GatewayException = GatewayExceptionIntent T.Text
  deriving (Show)

type EventChannel = TChan (Either GatewayException EventInternalParse)

type LoggingChannel = TQueue T.Text

-- | State of the eventloop
data LoopState = LoopStart
               | LoopClosed
               | LoopReconnect
  deriving Show

-- | Info the sendableLoop reads when it writes to the websocket
data SendablesData = SendablesData
  { sendableConnection :: Connection
  , librarySendables :: TChan GatewaySendableInternal
  , startsendingUsers :: TVar Bool
  , heartbeatInterval :: Integer
  }


-- | Gateway connection infinite loop. Get events from websocket and send them to the library user
--
-- @
--  Auth                                                         needed to connect
--  GatewayIntent                                                needed to connect
--  GatewayHandle (eventsGives,status,usersends,seq,sesh)        needed all over
--  log :: Chan (T.Text)                                         needed all over
--
--  sendableConnection                                 set by setup,  need sendableLoop
--  librarySendables :: Chan (GatewaySendableInternal) set by setup,  need heartbeat
--  heartbeatInterval :: Int                           set by Hello,  need heartbeat
--
--  sequenceId :: Int id of last event received        set by Resume, need heartbeat and reconnect
--  sessionId :: Text                                  set by Ready,  need reconnect
-- @
connectionLoop :: Auth -> GatewayIntent -> GatewayHandle -> LoggingChannel -> IO ()
connectionLoop auth intent gatewayHandle log = outerloop LoopStart
    where

    -- | Main connection loop. Catch exceptions and reconnect.
    outerloop :: LoopState -> IO ()
    outerloop state = do
        gatewayHost <- readTVarIO (gatewayHandleHostname gatewayHandle)
        mfirst <- firstmessage state -- construct first message
        case mfirst of
          Nothing -> pure () -- close

          Just message -> do
              nextstate <- try (startOneConnection gatewayHost message)  -- connection
              case nextstate :: Either SomeException LoopState of
                Left _ -> do t <- getRandomR (3,20)
                             threadDelay (t * (10^(6 :: Int)))
                             writeToLog log "gateway - trying to reconnect after failure(s)"
                             outerloop LoopReconnect
                Right n -> outerloop n

    -- | Construct the initial websocket message to send based on which state of the loop.
    --   Fresh start is Identify and a reconnect is Resume
    firstmessage :: LoopState -> IO (Maybe GatewaySendableInternal)
    firstmessage state =
      case state of
        LoopStart -> pure $ Just $ Identify auth intent (0, 1)
        LoopReconnect -> do seqId  <- readTVarIO (gatewayHandleLastSequenceId gatewayHandle)
                            seshId <- readTVarIO (gatewayHandleSessionId gatewayHandle)
                            if seshId == ""
                            then do writeToLog log "gateway - WARNING seshID was not set by READY?"
                                    pure $ Just $ Identify auth intent (0, 1)
                            else pure $ Just $ Resume auth seshId seqId
        LoopClosed -> pure Nothing

    startOneConnection
      :: HostName
      -- ^ The gateway address to connect to. Should be "gateway.discord.gg" on first try, but
      -- all Resumes should go to the resume_gateway_url specified in the Ready event
      -- https://discord.com/developers/docs/change-log#sessionspecific-gateway-resume-urls
      -> GatewaySendableInternal
      -- ^ The first message to send. Either an Identify or Resume.
      -> IO LoopState
    startOneConnection gatewayAddr message = runSecureClient gatewayAddr 443 ("/?v=" <> T.unpack apiVersion <>"&encoding=json") $ \conn -> do
                        msg <- getPayload conn log
                        case msg of
                            Right (Hello interval) -> do
                                -- setup sendables data
                                internal <- newTChanIO :: IO (TChan GatewaySendableInternal)
                                sendingUser <- newTVarIO False
                                let sending = SendablesData { sendableConnection = conn
                                                            , librarySendables = internal
                                                            , startsendingUsers = sendingUser
                                                            , heartbeatInterval = interval
                                                            }
                                -- start websocket sending loop
                                sendsId <- forkIO $ sendableLoop conn gatewayHandle sending log
                                heart <- forkIO $ heartbeat sending (gatewayHandleLastSequenceId gatewayHandle)
                                writeTChanIO internal message

                                -- run connection eventloop
                                finally (runEventLoop gatewayHandle sending log)
                                        (killThread heart >> killThread sendsId)

                            _ -> do
                                writeToLog log "gateway - WARNING could not connect. Expected hello"
                                sendClose conn ("expected hello" :: BL.ByteString)
                                void $ forever $ void (receiveData conn :: IO BL.ByteString)
                                -- > after sendClose you should call receiveDataMessage until
                                -- > it throws an exception
                                -- haskell websockets documentation
                                threadDelay (3 * (10^(6 :: Int)))
                                pure LoopStart


-- | Process events from discord and write them to the onDiscordEvent Channel
runEventLoop :: GatewayHandle -> SendablesData -> LoggingChannel -> IO LoopState
runEventLoop thehandle sendablesData log = do loop
  where
  eventChan :: TChan (Either GatewayException EventInternalParse)
  eventChan = gatewayHandleEvents thehandle

  -- | Keep receiving Dispatch events until a reconnect or a restart
  loop = do
    eitherPayload <- getPayloadTimeout sendablesData log
    case eitherPayload :: Either ConnectionException GatewayReceivable of

      Right (Dispatch event sq) -> do -- GOT AN EVENT:
                                      writeTVarIO (gatewayHandleLastSequenceId thehandle) sq
                                      writeTChanIO eventChan (Right event) -- send the event to user
                                      case event of
                                        (InternalReady _ _ _ seshID resumeHost _ _) -> do
                                            writeTVarIO (gatewayHandleSessionId thehandle) seshID
                                            writeTVarIO (gatewayHandleHostname thehandle) resumeHost
                                        _ -> writeTVarIO (startsendingUsers sendablesData) True
                                      loop
      Right (Hello _interval) -> do writeToLog log "eventloop - unexpected hello"
                                    loop
      Right (HeartbeatRequest sq) -> do writeTVarIO (gatewayHandleLastSequenceId thehandle) sq
                                        writeTChanIO (librarySendables sendablesData) (Heartbeat sq)
                                        loop
      Right (InvalidSession retry) -> pure $ if retry then LoopReconnect else LoopStart
      Right Reconnect        -> pure LoopReconnect
      Right HeartbeatAck     -> loop
      Right (ParseError _)   -> loop  -- getPayload logs the parse error. nothing to do here

      Left (CloseRequest code str) -> case code of
          -- see Discord and MDN documentation on gateway close event codes
          -- https://discord.com/developers/docs/topics/opcodes-and-status-codes#gateway-gateway-close-event-codes
          -- https://developer.mozilla.org/en-US/docs/Web/API/CloseEvent#properties
          1000 -> pure LoopReconnect
          1001 -> pure LoopReconnect
          4000 -> pure LoopReconnect
          4006 -> pure LoopStart
          4007 -> pure LoopStart
          4014 -> do writeTChanIO eventChan (Left (GatewayExceptionIntent $
                           "Tried to declare an unauthorized GatewayIntent. " <>
                           "Use the discord app manager to authorize by following: " <>
                           "https://github.com/discord-haskell/discord-haskell/blob/master/docs/intents.md"))
                     pure LoopClosed
          _ -> do writeToLog log ("gateway - unknown websocket close code " <> T.pack (show code)
                                  <> " [" <> TE.decodeUtf8 (BL.toStrict str) <> "]. Consider opening an issue "
                                  <> "https://github.com/discord-haskell/discord-haskell/issues")
                  pure LoopStart
      Left _ -> pure LoopReconnect


-- | Blocking wait for next payload from the websocket (returns "Reconnect" after 1.5*heartbeatInterval seconds)
getPayloadTimeout :: SendablesData -> LoggingChannel -> IO (Either ConnectionException GatewayReceivable)
getPayloadTimeout sendablesData log = do
  let interval = heartbeatInterval sendablesData
  res <- race (threadDelay (fromInteger ((interval * 1000 * 3) `div` 2)))
              (getPayload (sendableConnection sendablesData) log)
  case res of
    Left () -> pure (Right Reconnect)
    Right other -> pure other

-- | Blocking wait for next payload from the websocket
getPayload :: Connection -> LoggingChannel -> IO (Either ConnectionException GatewayReceivable)
getPayload conn log = try $ do
  msg' <- receiveData conn
  case eitherDecode msg' of
    Right msg -> pure msg
    Left  err -> do writeToLog log ("gateway - received exception [" <> T.pack err <> "]"
                                      <> " while decoding " <> TE.decodeUtf8 (BL.toStrict msg'))
                    pure (ParseError (T.pack err))

-- | Infinite loop to send heartbeats to the chan
heartbeat :: SendablesData -> TVar Integer -> IO ()
heartbeat sendablesData seqKey = do
  threadDelay (3 * 10^(6 :: Int))
  forever $ do
    num <- readTVarIO seqKey
    writeTChanIO (librarySendables sendablesData) (Heartbeat num)
    threadDelay (fromInteger (heartbeatInterval sendablesData * 1000))


-- | Infinite loop to send library/user events to discord with the actual websocket connection
sendableLoop :: Connection -> GatewayHandle -> SendablesData -> LoggingChannel -> IO ()
sendableLoop conn ghandle sendablesData _log = sendLoop
  where
  sendLoop = do
   -- send a ~120 events a min by delaying
      threadDelay $ round ((10^(6 :: Int)) * (62 / 120) :: Double)
   -- payload :: Either GatewaySendableInternal GatewaySendable
      payload <- race nextLibrary nextUser
   -- writeChan _log ("gateway - sending " <> TE.decodeUtf8 (BL.toStrict (either encode encode payload)))
      sendTextData conn (either encode encode payload)
      sendLoop

  -- | next event sent by library
  nextLibrary :: IO GatewaySendableInternal
  nextLibrary = readTChanIO (librarySendables sendablesData)

  -- | next event sent by user (once startsendingUsers is set)
  nextUser :: IO GatewaySendable
  nextUser = do usersending <- readTVarIO (startsendingUsers sendablesData)
                if usersending
                then readTChanIO (gatewayHandleUserSendables ghandle)
                else threadDelay (4 * (10^(6::Int))) >> nextUser


