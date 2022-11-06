{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}

module Discord.Internal.Types.Channel.SubTypes where

import Data.Aeson
import Data.Text (Text)
import Discord.Internal.Types.Checks
import Discord.Internal.Types.Prelude
import GHC.TypeNats
import Data.Data (Data)
import Data.Time (UTCTime)
import Data.Functor

class IsChannel c where
    channelId :: c -> ChannelId -- ^ The id of the channel
    channelType :: proxy c -> ChannelType -- ^ What type of channel is it?

class (InGuild c, IsChannel c) => IsGuildChannel c where
  -- | The name of the channel (1 - 100 characters).
  channelName :: c -> ChannelName
  -- | The storing position of the channel.
  channelPosition :: c -> Nat
  -- | An array of permission 'Overwrite's
  channelPermissions :: c -> [Overwrite]
  -- | Is not-safe-for-work
  channelNSFW :: c -> Bool
  -- | The parent channel (category for most channels, text channel for threads)
  channelParent :: c -> Maybe ChannelId

class (IsGuildChannel c) => IsGuildTextChannel c where
  -- | The topic of the channel. (0 - 1024 chars).
  channelTopic :: c -> ChannelTopic
  -- | The id of the last message in this channel
  channelLastMessage :: c -> Maybe MessageId
  -- | The last time something was pinned
  channelLastPinned :: c -> Maybe UTCTime
  -- | The rate limit per user for the channel
  channelUserRateLimit :: c -> UserRateLimit
  -- | The default rate limit in threads made in this channel. Does not update 
  -- existing threads when changed.
  channelThreadUserRateLimit :: c -> UserRateLimit
  -- | The default wait time of inactivity in a thread before it is archived
  channelThreadAutoArchiveDuration :: c -> AutoArchiveDuration

-- | Permission overwrites for a channel.
data Overwrite = Overwrite
  { -- | 'Role' or 'User' id
    overwriteId :: Either RoleId UserId,
    -- | Allowed permission bit set
    overwriteAllow :: Text,
    -- | Denied permission bit set
    overwriteDeny :: Text
  }
  deriving (Show, Read, Eq, Ord)

instance FromJSON Overwrite where
  parseJSON = withObject "Overwrite" $ \o -> do
    t <- o .: "type"
    i <- case (t :: Int) of
      0 -> Left <$> o .: "id"
      1 -> Right <$> o .: "id"
      _ -> error "Type field can only be 0 (role id) or 1 (user id)"
    Overwrite i
              <$> o .: "allow"
              <*> o .: "deny"

-- instance ToJSON Overwrite where
--   toJSON Overwrite {..} =
--     object
--       [ ("id", toJSON $ either unId unId overwriteId),
--         ("type", toJSON (either (const 0) (const 1) overwriteId :: Int)),
--         ("allow", toJSON overwriteAllow),
--         ("deny", toJSON overwriteDeny)
--       ]

newtype ChannelName = ChannelName (BoundedData 1 100 Text)
  deriving (Show)

instance FromJSON ChannelName where
  parseJSON v = parseJSON v <&> ChannelName . unChecked

newtype ChannelTopic = ChannelTopic (BoundedData 0 1024 Text)
  deriving (Show)

instance FromJSON ChannelTopic where
  parseJSON v = parseJSON v <&> ChannelTopic . unConstrained

newtype UserRateLimit = UserRateLimit (BoundedData 0 21600 Int)
  deriving (Show)

instance FromJSON UserRateLimit where
  parseJSON v = parseJSON v <&> UserRateLimit . unConstrained

unChannelName :: ChannelName -> Text
unChannelName (ChannelName bd) = unbind bd

unChannelTopic :: ChannelTopic -> Text
unChannelTopic (ChannelTopic bd) = unbind bd

unUserRateLimit :: UserRateLimit -> Int
unUserRateLimit (UserRateLimit bd) = unbind bd

-- | After how much inactivity to archive a thread
data AutoArchiveDuration
  = Hours1 -- ^ 60 minutes
  | Days1 -- ^ 1 day
  | Days3 -- ^ 3 days
  | Weeks1 -- ^ 1 week
  deriving (Eq, Ord, Show)

instance FromJSON AutoArchiveDuration where
  parseJSON = withScientific "AutoArchiveDuration" $
    \case
      60 -> pure Hours1
      1440 -> pure Days1
      4320 -> pure Days3
      10080 -> pure Weeks1
      s -> fail $ "bad auto archive duration: " <> show s

---- Channel Guild Text

data ChannelGuildText = ChannelGuildText
  { -- | The id of the channel (Will be equal to
    --   the guild if it's the "general" channel).
    _cgtId :: !ChannelId,
    -- | The id of the guild.
    _cgtGuildId :: !GuildId,
    -- | The name of the channel (1 - 100 characters).
    _cgtName :: !ChannelName,
    -- | The storing position of the channel.
    _cgtPosition :: !Nat,
    -- | An array of permission 'Overwrite's
    _cgtPermissions :: ![Overwrite],
    -- | Is not-safe-for-work
    _cgtNSFW :: !Bool,
    -- | The id of the parent channel (category)
    _cgtParent :: !(Maybe ChannelId),
    -- | The topic of the channel. (0 - 1024 chars).
    _cgtTopic :: !ChannelTopic,
    -- | The id of the last message sent in the channel
    _cgtLastMessage :: !(Maybe MessageId),
    -- | The last time something was pinned
    _cgtLastPinned :: !(Maybe UTCTime),
    -- | Seconds before a user can speak again
    _cgtUserRateLimit :: !UserRateLimit,
    -- | Seconds before a user can speak again
    _cgtThreadUserRateLimit :: !UserRateLimit,
    -- | How long a thread will stop showing in the channel list after inactivity
    _cgtThreadAutoArchiveDuration :: !AutoArchiveDuration
  } deriving (Show)

instance IsChannel ChannelGuildText where
  channelId = _cgtId
  channelType _ = ChannelTypeGuildText

instance InGuild ChannelGuildText where
  guildId = _cgtGuildId

instance IsGuildChannel ChannelGuildText where
  channelName = _cgtName
  channelPosition = _cgtPosition
  channelPermissions = _cgtPermissions
  channelNSFW = _cgtNSFW
  channelParent = _cgtParent

instance IsGuildTextChannel ChannelGuildText where
  channelTopic = _cgtTopic
  channelLastMessage = _cgtLastMessage
  channelLastPinned = _cgtLastPinned
  channelUserRateLimit = _cgtUserRateLimit
  channelThreadUserRateLimit = _cgtThreadUserRateLimit
  channelThreadAutoArchiveDuration = _cgtThreadAutoArchiveDuration

instance FromJSON ChannelGuildText where
  parseJSON = withObject "ChannelGuildText" $ \o -> do
    t <- o .: "type"
    case t of
      ChannelTypeGuildText -> 
        ChannelGuildText <$> o .: "id"
                         <*> o .: "guild_id"
                         <*> o .: "name"
                         <*> o .: "position"
                         <*> o .: "permissions_overwrites"
                         <*> o .: "nsfw"
                         <*> o .: "parent_id"
                         <*> (o .:? "topic" .!= ChannelTopic (bind ""))
                         <*> o .: "last_message_id"
                         <*> o .: "last_pin_timestamp"
                         <*> (o .:? "rate_limit_per_user" .!= UserRateLimit (bind 0))
                         <*> (o .:? "default_thread_rate_limit_per_user" .!= UserRateLimit (bind 0))
                         <*> o .: "default_auto_archive_duration"
      _ -> fail $ "cannot process channel type as ChannelGuildText: " <> show t

-----

---- Channel Guild Announcement

data ChannelGuildAnnouncement = ChannelGuildAnnouncement
  { -- | The id of the channel (Will be equal to
    --   the guild if it's the "general" channel).
    _cgaId :: !ChannelId,
    -- | The id of the guild.
    _cgaGuildId :: !GuildId,
    -- | The name of the channel (1 - 100 characters).
    _cgaName :: !ChannelName,
    -- | The storing position of the channel.
    _cgaPosition :: !Nat,
    -- | An array of permission 'Overwrite's
    _cgaPermissions :: ![Overwrite],
    -- | Is not-safe-for-work
    _cgaNSFW :: !Bool,
    -- | The id of the parent channel (category)
    _cgaParent :: !(Maybe ChannelId),
    -- | The topic of the channel. (0 - 1024 chars).
    _cgaTopic :: !ChannelTopic,
    -- | The id of the last message sent in the channel
    _cgaLastMessage :: !(Maybe MessageId),
    -- | The last time something was pinned
    _cgaLastPinned :: !(Maybe UTCTime),
    -- | Seconds before a user can speak again
    _cgaUserRateLimit :: !UserRateLimit,
    -- | Seconds before a user can speak again
    _cgaThreadUserRateLimit :: !UserRateLimit,
    -- | How long a thread will stop showing in the channel list after inactivity
    _cgaThreadAutoArchiveDuration :: !AutoArchiveDuration
  } deriving (Show)

instance IsChannel ChannelGuildAnnouncement where
  channelId = _cgaId
  channelType _ = ChannelTypeGuildAnnouncement

instance InGuild ChannelGuildAnnouncement where
  guildId = _cgaGuildId

instance IsGuildChannel ChannelGuildAnnouncement where
  channelName = _cgaName
  channelPosition = _cgaPosition
  channelPermissions = _cgaPermissions
  channelNSFW = _cgaNSFW
  channelParent = _cgaParent

instance IsGuildTextChannel ChannelGuildAnnouncement where
  channelTopic = _cgaTopic
  channelLastMessage = _cgaLastMessage
  channelLastPinned = _cgaLastPinned
  channelUserRateLimit = _cgaUserRateLimit
  channelThreadUserRateLimit = _cgaThreadUserRateLimit
  channelThreadAutoArchiveDuration = _cgaThreadAutoArchiveDuration

instance FromJSON ChannelGuildAnnouncement where
  parseJSON = withObject "ChannelGuildAnnouncement" $ \o -> do
    t <- o .: "type"
    case t of
      ChannelTypeGuildAnnouncement -> 
        ChannelGuildAnnouncement <$> o .: "id"
                         <*> o .: "guild_id"
                         <*> o .: "name"
                         <*> o .: "position"
                         <*> o .: "permissions_overwrites"
                         <*> o .: "nsfw"
                         <*> o .: "parent_id"
                         <*> (o .:? "topic" .!= ChannelTopic (bind ""))
                         <*> o .: "last_message_id"
                         <*> o .: "last_pin_timestamp"
                         <*> (o .:? "rate_limit_per_user" .!= UserRateLimit (bind 0))
                         <*> (o .:? "default_thread_rate_limit_per_user" .!= UserRateLimit (bind 0))
                         <*> o .: "default_auto_archive_duration"
      _ -> fail $ "cannot process channel type as ChannelGuildAnnouncement: " <> show t

-----

---

-- | The different channel types. Used for application commands and components.
--
-- https://discord.com/developers/docs/resources/channel#channel-object-channel-types
data ChannelType
  = -- | A text channel in a server.
    ChannelTypeGuildText
  | -- | A direct message between users.
    ChannelTypeDM
  | -- | A voice channel in a server.
    ChannelTypeGuildVoice
  | -- | A direct message between multiple users.
    ChannelTypeGroupDM
  | -- | An organizational category that contains up to 50 channels.
    ChannelTypeGuildCategory
  | -- | A channel that users can follow and crosspost into their own server.
    ChannelTypeGuildAnnouncement
  | -- | A channel in which game developers can sell their game on discord.
    ChannelTypeGuildStore
  | -- | A temporary sub-channel within a guild_news channel.
    ChannelTypeGuildNewsThread
  | -- | A temporary sub-channel within a guild_text channel.
    ChannelTypeGuildPublicThread
  | -- | A temporary sub-channel within a GUILD_TEXT channel that is only
    -- viewable by those invited and those with the MANAGE_THREADS permission
    ChannelTypeGuildPrivateThread
  | -- | A voice channel for hosting events with an audience.
    ChannelTypeGuildStageVoice
  deriving (Show, Read, Data, Eq, Ord)

instance InternalDiscordEnum ChannelType where
  discordTypeStartValue = ChannelTypeGuildText
  fromDiscordType ChannelTypeGuildText = 0
  fromDiscordType ChannelTypeDM = 1
  fromDiscordType ChannelTypeGuildVoice = 2
  fromDiscordType ChannelTypeGroupDM = 3
  fromDiscordType ChannelTypeGuildCategory = 4
  fromDiscordType ChannelTypeGuildAnnouncement = 5
  fromDiscordType ChannelTypeGuildStore = 6
  fromDiscordType ChannelTypeGuildNewsThread = 10
  fromDiscordType ChannelTypeGuildPublicThread = 11
  fromDiscordType ChannelTypeGuildPrivateThread = 12
  fromDiscordType ChannelTypeGuildStageVoice = 13

instance ToJSON ChannelType where
  toJSON = toJSON . fromDiscordType

instance FromJSON ChannelType where
  parseJSON = discordTypeParseJSON "ChannelType"
