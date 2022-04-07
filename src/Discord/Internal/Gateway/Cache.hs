{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | Query info about connected Guilds and Channels
module Discord.Internal.Gateway.Cache where

import Prelude hiding (log)
import Control.Monad (forever)
import Control.Concurrent (Chan, MVar, readChan, writeChan, putMVar, takeMVar)
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Discord.Internal.Types
import Discord.Internal.Gateway.EventLoop (GatewayException)

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
  , cacheHandleCache  :: MVar (Either (Cache d, GatewayException) (Cache d))
  }

cacheLoop :: forall d. Maybe d ->  CacheHandle d -> Chan T.Text -> IO ()
cacheLoop d cacheHandle log = do
      ready <- readChan eventChan
      case ready of
        Right (InternalReady _ user dmChannels _unavailableGuilds _ _ pApp) -> do
          let dmChans = M.fromList (zip (map channelId dmChannels) dmChannels)
          putMVar cache (Right (Cache user dmChans M.empty M.empty pApp d))
          loop
        Right r ->
          writeChan log ("cache - stopping cache - expected Ready event, but got " <> T.pack (show r))
        Left e ->
          writeChan log ("cache - stopping cache - gateway exception " <> T.pack (show e))
  where
  cache     = cacheHandleCache cacheHandle
  eventChan = cacheHandleEvents cacheHandle

  loop :: IO ()
  loop = forever $ do
    eventOrExcept <- readChan eventChan
    minfo <- takeMVar cache
    case minfo of
      Left nope -> putMVar cache (Left nope)
      Right info -> case eventOrExcept of
                      Left e -> putMVar cache (Left (info, e))
                      Right event -> putMVar cache (Right (adjustCache info event))

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
