{-# language
        DeriveGeneric
      , DuplicateRecordFields #-}
module Keybase.Chat.Types where

import Prelude          (Bool (..), Enum (..), Eq (..), Ord (..), Show (..), (.))
import Data.Int         (Int)
import Data.Maybe       (Maybe, fromMaybe)
import Data.Text        (Text, pack, stripPrefix, toLower, unpack)
import Data.Aeson       (FromJSON (..), ToJSON (..),
                         defaultOptions,
                         genericParseJSON, genericToEncoding, genericToJSON)
import Data.Aeson.Types (SumEncoding(ObjectWithSingleField, TaggedObject),
                         contentsFieldName, tagFieldName)
import qualified Data.Aeson.Types as A
import qualified GHC.Generics     as G

encOptions :: A.Options
encOptions = defaultOptions
  { A.constructorTagModifier = unpack . toLower . pack }

data Existance = Active | Archived | Deleted
  deriving (G.Generic, Eq, Enum, Ord, Show)
instance FromJSON Existance where
  parseJSON = genericParseJSON encOptions
instance ToJSON Existance where
  toJSON = genericToJSON encOptions
  toEncoding = genericToEncoding encOptions

data Members = KBFS | Team | ImpTeam
  deriving (G.Generic, Eq, Enum, Ord, Show)
instance FromJSON Members where
  parseJSON = genericParseJSON encOptions
instance ToJSON Members where
  toJSON = genericToJSON encOptions
  toEncoding = genericToEncoding encOptions

data SyncInbox = Current | Incremental | Clear
  deriving (G.Generic, Eq, Enum, Ord, Show)
instance FromJSON SyncInbox where
  parseJSON = genericParseJSON encOptions
instance ToJSON SyncInbox where
  toJSON = genericToJSON encOptions
  toEncoding = genericToEncoding encOptions

strip :: Text -> Text -> Text
strip prefix txt = fromMaybe txt (stripPrefix prefix txt)

topicOptions :: A.Options
topicOptions = defaultOptions
  { A.constructorTagModifier = unpack . strip "topic" . toLower . pack }

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

teamOptions :: A.Options
teamOptions = defaultOptions
  { A.constructorTagModifier = unpack . strip "team" . toLower . pack }

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
  parseJSON = genericParseJSON encOptions
instance ToJSON Notification where
  toJSON = genericToJSON encOptions
  toEncoding = genericToEncoding encOptions

data Status
  = Unfiled
  | Favorite
  | Ignored
  | Blocked
  | Muted
  | Reported
  deriving (G.Generic, Eq, Enum, Ord, Show)
instance FromJSON Status where
  parseJSON = genericParseJSON encOptions
instance ToJSON Status where
  toJSON = genericToJSON encOptions
  toEncoding = genericToEncoding encOptions

memberOptions :: A.Options
memberOptions = defaultOptions
  { A.constructorTagModifier = unpack . strip "member" . toLower . pack }

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

type ConversationId = Text
data Conversation = Conversation
  { id           :: ConversationId
  , channel      :: Channel
  , unread       :: Bool
  , active_at    :: Int
  , active_at_ms :: Int
  } deriving (G.Generic, Eq, Show)
instance FromJSON Conversation
instance ToJSON Conversation where
  toEncoding = genericToEncoding defaultOptions

requestOptions :: A.Options
requestOptions = defaultOptions
  { A.constructorTagModifier = unpack . strip "member" . toLower . pack
  , A.sumEncoding = TaggedObject { contentsFieldName = "params"
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
  toJSON = genericToJSON encOptions
  toEncoding = genericToEncoding requestOptions

data Options = Options
  { conversation_id :: Text }
  deriving (G.Generic, Eq, Show)
instance FromJSON Options where
  parseJSON = genericParseJSON encOptions
instance ToJSON Options where
  toJSON = genericToJSON encOptions
  toEncoding = genericToEncoding encOptions

data Params = Params
  { options :: Options }
  deriving (G.Generic, Eq, Show)
instance FromJSON Params where
  parseJSON = genericParseJSON encOptions
instance ToJSON Params where
  toJSON = genericToJSON encOptions
  toEncoding = genericToEncoding encOptions

data Request = Request
  { method :: Method
  , params :: Maybe Params
  } deriving (G.Generic, Eq, Show)
instance FromJSON Request where
  parseJSON = genericParseJSON encOptions
instance ToJSON Request where
  toJSON = genericToJSON encOptions
  toEncoding = genericToEncoding encOptions

data RateLimit = RateLimit
  { tank     :: Text
  , capacity :: Int
  , reset    :: Int
  , gas      :: Int
  } deriving (G.Generic, Eq, Show)
instance FromJSON RateLimit where
  parseJSON = genericParseJSON encOptions
instance ToJSON RateLimit where
  toJSON = genericToJSON encOptions
  toEncoding = genericToEncoding encOptions

data Pagination = Pagination
  { next     :: Text
  , previous :: Text
  , num      :: Int
  , last     :: Bool
  } deriving (G.Generic, Eq, Show)
instance FromJSON Pagination where
  parseJSON = genericParseJSON encOptions
instance ToJSON Pagination where
  toJSON = genericToJSON encOptions
  toEncoding = genericToEncoding encOptions

msgOptions :: A.Options
msgOptions = defaultOptions
  { A.tagSingleConstructors = True
  , A.sumEncoding = TaggedObject { tagFieldName = "msg"
                                 , contentsFieldName = "empty"}
  }

type MessageId = Int
data Message = Message
  { id :: MessageId
  , channel :: Channel
  , sender :: Text
  , sent_at :: Int
  , sent_at_ms :: Int
  , content :: MessageContent
  , prev :: [MessagePrev]
  , unread :: Bool
  } deriving (G.Generic, Eq, Show)
instance FromJSON Message where
  parseJSON = genericParseJSON encOptions
instance ToJSON Message where
  toJSON = genericToJSON encOptions
  toEncoding = genericToEncoding encOptions

data Msg = Msg
  { msg :: Message }
  deriving (G.Generic, Eq, Show)

instance FromJSON Msg where
  parseJSON = genericParseJSON encOptions
instance ToJSON Msg where
  toJSON = genericToJSON encOptions
  toEncoding = genericToEncoding encOptions

data MessageText = MessageText
  { body :: Text }
  deriving (G.Generic, Eq, Show)
instance FromJSON MessageText where
  parseJSON = genericParseJSON encOptions
instance ToJSON MessageText where
  toJSON = genericToJSON encOptions
  toEncoding = genericToEncoding encOptions

data MessageAttachementUploaded = MessageAttachementUploaded
  { messageID :: Int }
  deriving (G.Generic, Eq, Show)
instance FromJSON MessageAttachementUploaded where
  parseJSON = genericParseJSON encOptions
instance ToJSON MessageAttachementUploaded where
  toJSON = genericToJSON encOptions
  toEncoding = genericToEncoding encOptions

data MessageContent
  = MsgNone
  | MsgText { text :: MessageText }
  | MsgAttachementUploaded { attachement_uploaded :: MessageAttachementUploaded }
  -- | MsgEdit
  -- | MsgDelete
  -- | MsgMetadata
  -- | MsgTLFName
  -- | MsgHeadline
  -- | MsgAttachement
  -- | MsgJoin
  -- | MsgLeave
  -- | MsgSystem
  deriving (G.Generic, Eq, Show)
instance FromJSON MessageContent where
  parseJSON = genericParseJSON contentOptions
instance ToJSON MessageContent where
  toJSON = genericToJSON contentOptions
  toEncoding = genericToEncoding contentOptions

contentOptions :: A.Options
contentOptions = defaultOptions
  { A.unwrapUnaryRecords = True
  , A.sumEncoding = TaggedObject { tagFieldName = "type"
                                 , contentsFieldName = ""
                                 }
  , A.constructorTagModifier = unpack . strip "msg" . toLower . pack
  }

data MessagePrev = MessagePrev
  { id :: Int
  , hash :: Text
  } deriving (G.Generic, Eq, Show)
instance FromJSON MessagePrev where
  parseJSON = genericParseJSON encOptions
instance ToJSON MessagePrev where
  toJSON = genericToJSON encOptions
  toEncoding = genericToEncoding encOptions

data Result
  = Inbox { conversations :: [Conversation]
          , offline :: Bool
          }
  | Messages { messages :: [Msg]
             , pagination :: Pagination
             , ratelimits :: [RateLimit]
             }
  deriving (G.Generic, Eq, Show)
instance FromJSON Result where
  parseJSON = genericParseJSON encOptions
instance ToJSON Result where
  toJSON = genericToJSON encOptions
  toEncoding = genericToEncoding encOptions

responseOptions :: A.Options
responseOptions = defaultOptions
  { A.constructorTagModifier = unpack . strip "member" . toLower . pack
  , A.sumEncoding = ObjectWithSingleField
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
