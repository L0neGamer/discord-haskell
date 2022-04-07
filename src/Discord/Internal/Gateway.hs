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
import Control.Concurrent (Chan, ThreadId, dupChan, newChan, forkIO, newEmptyMVar)
import Data.IORef (newIORef)
import qualified Data.Text as T

import Discord.Internal.Types (Auth, EventInternalParse, GatewayIntent)
import Discord.Internal.Gateway.EventLoop (connectionLoop, GatewayHandle(..), GatewayException(..))
import Discord.Internal.Gateway.Cache (cacheLoop, Cache(..), CacheHandle(..))

startCacheThread :: Maybe d -> Chan T.Text -> IO (CacheHandle d, ThreadId)
startCacheThread d log = do
  events <- newChan
  cache <- newEmptyMVar
  let cacheHandle = CacheHandle events cache
  tid <- forkIO $ cacheLoop d cacheHandle log
  pure (cacheHandle, tid)

-- | Create a Chan for websockets. This creates a thread that
--   writes all the received EventsInternalParse to the Chan
startGatewayThread :: Auth -> GatewayIntent -> CacheHandle d-> Chan T.Text -> IO (GatewayHandle, ThreadId)
startGatewayThread auth intent cacheHandle log = do
  events <- dupChan (cacheHandleEvents cacheHandle)
  sends <- newChan
  status <- newIORef Nothing
  seqid <- newIORef 0
  seshid <- newIORef ""
  let gatewayHandle = GatewayHandle events sends status seqid seshid
  tid <- forkIO $ connectionLoop auth intent gatewayHandle log
  pure (gatewayHandle, tid)



