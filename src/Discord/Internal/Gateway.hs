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

import Prelude
import UnliftIO
import UnliftIO.Concurrent
import Control.Monad.Random (MonadRandom)
import qualified Data.Text as T

import Discord.Internal.Types
import Discord.Internal.Gateway.EventLoop (connectionLoop, GatewayHandle(..), GatewayException(..))
import Discord.Internal.Gateway.Cache (cacheLoop, Cache(..), CacheHandle(..))

-- | Starts a thread for the cache
startCacheThread :: forall m. MonadUnIOLog m => m (CacheHandle, ThreadId)
startCacheThread = do
  events <- newChan :: m (Chan (Either GatewayException EventInternalParse))
  cache <- newEmptyMVar :: m (MVar (Either (Cache, GatewayException) Cache))
  let cacheHandle = CacheHandle events cache
  tid <- forkIO $ cacheLoop cacheHandle
  pure (cacheHandle, tid)

-- | Create a Chan for websockets. This creates a thread that
--   writes all the received EventsInternalParse to the Chan
startGatewayThread :: (MonadUnIOLog m, MonadRandom m) => Auth -> GatewayIntent -> CacheHandle -> m (GatewayHandle, ThreadId)
startGatewayThread auth intent cacheHandle = do
  events <- dupChan (cacheHandleEvents cacheHandle)
  sends <- newChan
  status <- newIORef Nothing
  seqid <- newIORef 0
  seshid <- newIORef ""
  host <- newIORef $ "gateway.discord.gg"
  let gatewayHandle = GatewayHandle events sends status seqid seshid host
  tid <- forkIO $ connectionLoop auth intent gatewayHandle
  pure (gatewayHandle, tid)



