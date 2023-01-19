{-# LANGUAGE OverloadedStrings #-}

-- | Provides a rather raw interface to the websocket events
--   through a real-time Chan
module Discord.Internal.Gateway
  ( GatewayHandle(..)
  , CacheHandle(..)
  , GatewayException(..)
  , Cache(..)
  , startCacheThread
  , startGatewayThread
  , module Discord.Internal.Types
  ) where

import Prelude hiding (log)
import UnliftIO
import UnliftIO.Concurrent

import Discord.Internal.Types (Auth, EventInternalParse, GatewayIntent)
import Discord.Internal.Gateway.EventLoop
import Discord.Internal.Gateway.Cache (cacheLoop, Cache(..), CacheHandle(..))

-- | Starts a thread for the cache
startCacheThread :: Bool -> EventChannel -> LoggingChannel -> IO (MVar CacheHandle, ThreadId)
startCacheThread isEnabled eventChannel log = do
  events <- atomically $ dupTChan eventChannel
  cacheMVar <- newEmptyMVar
  tid <- forkIO $ cacheLoop isEnabled events log cacheMVar
  pure (cacheMVar, tid)

-- | Create a Chan for websockets. This creates a thread that
--   writes all the received EventsInternalParse to the Chan
startGatewayThread :: Auth -> GatewayIntent -> EventChannel -> LoggingChannel -> IO (GatewayHandle, ThreadId)
startGatewayThread auth intent eventChannel log = do
  events <- atomically $ dupTChan eventChannel
  sends <- newTChanIO
  status <- newTVarIO Nothing
  seqid <- newTVarIO 0
  seshid <- newTVarIO ""
  host <- newTVarIO "gateway.discord.gg"
  let gatewayHandle = GatewayHandle events sends status seqid seshid host
  tid <- forkIO $ connectionLoop auth intent gatewayHandle log
  pure (gatewayHandle, tid)



