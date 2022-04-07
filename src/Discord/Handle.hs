module Discord.Handle
  ( DiscordHandle
  , EnvDiscordHandle(..)
  , DiscordHandler
  , EnvDiscordHandler
  , HandleThreadId(..)
  ) where

import Control.Concurrent (ThreadId, Chan, MVar)
import Control.Monad.Reader (ReaderT)
import qualified Data.Text as T

import Discord.Internal.Rest (RestChanHandle(..))
import Discord.Internal.Gateway (GatewayHandle(..), CacheHandle(..))

-- | Thread Ids marked by what type they are
data HandleThreadId = HandleThreadIdRest ThreadId
                      | HandleThreadIdCache ThreadId
                      | HandleThreadIdLogger ThreadId
                      | HandleThreadIdGateway ThreadId

data EnvDiscordHandle d = DiscordHandle
  { discordHandleRestChan :: RestChanHandle
  , discordHandleGateway :: GatewayHandle
  , discordHandleCache :: CacheHandle d
  , discordHandleThreads :: [HandleThreadId]
  , discordHandleLog :: Chan T.Text
  , discordHandleLibraryError :: MVar T.Text
  }

type DiscordHandle = EnvDiscordHandle ()

type DiscordHandler = ReaderT DiscordHandle IO

type EnvDiscordHandler d = ReaderT (EnvDiscordHandle d) IO
