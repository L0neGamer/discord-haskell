module Discord.Handle
  ( DiscordHandle(..)
  , HandleThreadId(..)
  ) where

import           Control.Concurrent             ( Chan
                                                , MVar
                                                , ThreadId
                                                )
import qualified Data.Text                     as T

import           Discord.Internal.Gateway       ( CacheHandle(..)
                                                , GatewayHandle(..)
                                                )
import           Discord.Internal.Rest          ( RestChanHandle(..) )

-- | Thread Ids marked by what type they are
data HandleThreadId = HandleThreadIdRest ThreadId
                      | HandleThreadIdCache ThreadId
                      | HandleThreadIdLogger ThreadId
                      | HandleThreadIdGateway ThreadId

data DiscordHandle = DiscordHandle
  { discordHandleRestChan     :: RestChanHandle
  , discordHandleGateway      :: GatewayHandle
  , discordHandleCache        :: CacheHandle
  , discordHandleThreads      :: [HandleThreadId]
  , discordHandleLog          :: Chan T.Text
  , discordHandleLibraryError :: MVar T.Text
  }
