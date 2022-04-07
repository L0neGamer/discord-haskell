{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | Query info about connected Guilds and Channels
module Discord.Internal.Gateway.Cache where

import Prelude hiding (log)
import Control.Monad (forever)
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import System.Timeout

import Discord.Internal.Types
import Discord.Internal.Gateway.EventLoop

data Cache d = Cache
     { cacheCurrentUser :: User
     , cacheDMChannels :: M.Map ChannelId Channel
     , cacheGuilds :: M.Map GuildId Guild
     , cacheChannels :: M.Map ChannelId Channel
     , cacheApplication :: PartialApplication
     , cacheData :: Maybe d
     } deriving (Show)

data CacheHandle d = CacheHandle
  { cacheHandleEvents :: Chan (Either GatewayException EventInternalParse)
  , cacheHandleCache  :: TVar (Either (Cache d, GatewayException) (Cache d))
  }

cacheLoop :: forall d. Maybe d -> Chan (Either GatewayException EventInternalParse) -> Chan T.Text -> IO (CacheHandle d, ThreadId)
cacheLoop d eventChan log = do
      maybeready <- timeout 5000000 $ readChan eventChan -- timeout is 5 seconds
      case maybeready of
        Nothing -> fail "timed out on getting ready cache loop"
        Just ready ->
          case ready of
            Right (InternalReady _ user dmChannels _unavailableGuilds _ _ pApp) -> do
              let dmChans = M.fromList (zip (map channelId dmChannels) dmChannels)
              chc <- newTVarIO (Right (Cache user dmChans M.empty M.empty pApp d))
              let ch = CacheHandle eventChan chc
              lti <- forkIO (loop chc)
              return (ch, lti)
            Right r -> do
              writeChan log (T.pack $ expectedReady r)
              fail $ expectedReady r
            Left e -> do
              writeChan log (T.pack $ gatewayException e)
              fail $ gatewayException e
  where
  expectedReady r = "cache - stopping cache - expected Ready event, but got " <> show r
  gatewayException e = "cache - stopping cache - gateway exception " <> show e

  loop :: TVar (Either (Cache d, GatewayException) (Cache d)) -> IO ()
  loop cache = forever $ do
    eventOrExcept <- readChan eventChan
    minfo <- readTVarIO cache
    case minfo of
      Left nope -> atomically $ writeTVar cache (Left nope)
      Right info -> case eventOrExcept of
                      Left e -> atomically $ writeTVar cache (Left (info, e))
                      Right event -> atomically $ writeTVar cache (Right (adjustCache info event))

adjustCache :: Cache d -> EventInternalParse -> Cache d
adjustCache minfo event = case event of
  InternalGuildCreate guild ->
    let newChans = maybe [] (map (setChanGuildID (guildId guild))) (guildChannels guild)
        g = M.insert (guildId guild) (guild { guildChannels = Just newChans }) (cacheGuilds minfo)
        c = M.unionWith const
                        (M.fromList [ (channelId ch, ch) | ch <- newChans ])
                        (cacheChannels minfo)
    in minfo { cacheGuilds = g, cacheChannels = c }
  --InternalGuildUpdate guild -> do
  --  let g = M.insert (guildId guild) guild (cacheGuilds minfo)
  --      m2 = minfo { cacheGuilds = g }
  --  putMVar cache m2
  --InternalGuildDelete guild -> do
  --  let g = M.delete (guildId guild) (cacheGuilds minfo)
  --      c = M.filterWithKey (\(keyGuildId,_) _ -> keyGuildId /= guildId guild) (cacheChannels minfo)
  --      m2 = minfo { cacheGuilds = g, cacheChannels = c }
  --  putMVar cache m2
  InternalReady _ _ _ _ _ _ pa -> minfo { cacheApplication = pa }
  _ -> minfo

setChanGuildID :: GuildId -> Channel -> Channel
setChanGuildID s c = if channelIsInGuild c
                     then c { channelGuild = s }
                     else c
