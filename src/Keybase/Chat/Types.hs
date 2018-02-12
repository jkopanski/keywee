{-# language DeriveGeneric #-}
module Keybase.Chat.Types where

import Prelude          (Bool (..), Enum (..), Eq (..), Ord (..), Show (..), (.))
import Data.Int         (Int)
import Data.Maybe       (Maybe, fromMaybe)
import Data.Text        (Text, pack, stripPrefix, toLower, unpack)
import Data.Aeson       (FromJSON (..), ToJSON (..),
                         defaultOptions,
                         genericParseJSON, genericToEncoding, genericToJSON)
import Data.Aeson.Types (Options (..), SumEncoding(ObjectWithSingleField, TaggedObject),
                         contentsFieldName, tagFieldName)
import qualified GHC.Generics as G

options :: Options
options = defaultOptions
  { constructorTagModifier = unpack . toLower . pack }

data Existance = Active | Archived | Deleted
  deriving (G.Generic, Eq, Enum, Ord, Show)
instance FromJSON Existance where
  parseJSON = genericParseJSON options
instance ToJSON Existance where
  toJSON = genericToJSON options
  toEncoding = genericToEncoding options

data Members = KBFS | Team | ImpTeam
  deriving (G.Generic, Eq, Enum, Ord, Show)
instance FromJSON Members where
  parseJSON = genericParseJSON options
instance ToJSON Members where
  toJSON = genericToJSON options
  toEncoding = genericToEncoding options

data SyncInbox = Current | Incremental | Clear
  deriving (G.Generic, Eq, Enum, Ord, Show)
instance FromJSON SyncInbox where
  parseJSON = genericParseJSON options
instance ToJSON SyncInbox where
  toJSON = genericToJSON options
  toEncoding = genericToEncoding options

strip :: Text -> Text -> Text
strip prefix txt = fromMaybe txt (stripPrefix prefix txt)

msgOptions :: Options
msgOptions = defaultOptions
  { constructorTagModifier = unpack . strip "msg" . toLower . pack }

data Message
  = MsgNone
  | MsgText
  | MsgAttachement
  | MsgEdit
  | MsgDelete
  | MsgMetadata
  | MsgTLFName
  | MsgHeadline
  | MsgAttachementUploaded
  | MsgJoin
  | MsgLeave
  | MsgSystem
  deriving (G.Generic, Eq, Enum, Ord, Show)
instance FromJSON Message where
  parseJSON = genericParseJSON msgOptions
instance ToJSON Message where
  toJSON = genericToJSON msgOptions
  toEncoding = genericToEncoding msgOptions

topicOptions :: Options
topicOptions = defaultOptions
  { constructorTagModifier = unpack . strip "topic" . toLower . pack }

data Topic
  = TopicNone
  | TopicChat
  | TopicDev
  deriving (G.Generic, Eq, Enum, Ord, Show)
instance FromJSON Topic where
  parseJSON = genericParseJSON topicOptions
instance ToJSON Topic where
  toJSON = genericToJSON topicOptions
  toEncoding = genericToEncoding topicOptions

teamOptions :: Options
teamOptions = defaultOptions
  { constructorTagModifier = unpack . strip "team" . toLower . pack }

data Team
  = TeamNone
  | TeamSimple
  | TeamComplex
  deriving (G.Generic, Eq, Enum, Ord, Show)
instance FromJSON Team where
  parseJSON = genericParseJSON teamOptions
instance ToJSON Team where
  toJSON = genericToJSON teamOptions
  toEncoding = genericToEncoding teamOptions

data Notification = Generic | AtMention
  deriving (G.Generic, Eq, Enum, Ord, Show)
instance FromJSON Notification where
  parseJSON = genericParseJSON options
instance ToJSON Notification where
  toJSON = genericToJSON options
  toEncoding = genericToEncoding options

data Status
  = Unfiled
  | Favorite
  | Ignored
  | Blocked
  | Muted
  | Reported
  deriving (G.Generic, Eq, Enum, Ord, Show)
instance FromJSON Status where
  parseJSON = genericParseJSON options
instance ToJSON Status where
  toJSON = genericToJSON options
  toEncoding = genericToEncoding options

memberOptions :: Options
memberOptions = defaultOptions
  { constructorTagModifier = unpack . strip "member" . toLower . pack }

data MemberStatus
  = MemberActive
  | MemberRemoved
  | MemberLeft
  | MemberPreview
  | MemberReset
  deriving (G.Generic, Eq, Enum, Ord, Show)
instance FromJSON MemberStatus where
  parseJSON = genericParseJSON memberOptions
instance ToJSON MemberStatus where
  toJSON = genericToJSON memberOptions
  toEncoding = genericToEncoding memberOptions

data Channel = Channel
  { name         :: Text
  , public       :: Bool
  , members_type :: Members
  , topic_type   :: Topic
  , topic_name   :: Maybe Text
  } deriving (G.Generic, Eq, Show)
instance FromJSON Channel
instance ToJSON Channel where
  toEncoding = genericToEncoding defaultOptions

data Conversation = Conversation
  { id           :: Text
  , channel      :: Channel
  , unread       :: Bool
  , active_at    :: Int
  , active_at_ms :: Int
  } deriving (G.Generic, Eq, Show)
instance FromJSON Conversation
instance ToJSON Conversation where
  toEncoding = genericToEncoding defaultOptions

requestOptions :: Options
requestOptions = defaultOptions
  { constructorTagModifier = unpack . strip "member" . toLower . pack
  , sumEncoding = TaggedObject { contentsFieldName = "params"
                               , tagFieldName = "method"
                               }
  }

data Method = Attach
            | Delete
            | Download
            | Edit
            | List
            | Read
            | Send
  deriving (G.Generic, Eq, Show)
instance FromJSON Method where
  parseJSON = genericParseJSON requestOptions
instance ToJSON Method where
  toJSON = genericToJSON options
  toEncoding = genericToEncoding requestOptions

data Request = Request
  { method :: Method }
  deriving (G.Generic, Eq, Show)
instance FromJSON Request where
  parseJSON = genericParseJSON options
instance ToJSON Request where
  toJSON = genericToJSON options
  toEncoding = genericToEncoding options

data Result
  = Inbox { conversations :: [Conversation]
          , offline :: Bool
          }
  deriving (G.Generic, Eq, Show)
instance FromJSON Result where
  parseJSON = genericParseJSON options
instance ToJSON Result where
  toJSON = genericToJSON options
  toEncoding = genericToEncoding options

responseOptions :: Options
responseOptions = defaultOptions
  { constructorTagModifier = unpack . strip "member" . toLower . pack
  , sumEncoding = ObjectWithSingleField
  }

data Response
  = Error { error :: Text }
  | Result Result
  deriving (G.Generic, Eq, Show)
instance FromJSON Response where
  parseJSON = genericParseJSON responseOptions
instance ToJSON Response where
  toJSON = genericToJSON responseOptions
  toEncoding = genericToEncoding responseOptions
