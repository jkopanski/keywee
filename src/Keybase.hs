module Keybase where

import           Data.Text   (Text)
import           Data.Aeson  (FromJSON (..), ToJSON (..),
                              genericParseJSON, genericToEncoding, genericToJSON)
import qualified Keybase.Chat as Chat

-- data Response
--   = Error { error :: Text }
--   | Result { result :: Chat.Result }
--   deriving (Generic, Eq)
-- instance FromJSON Response where
--   parseJSON = genericParseJSON Chat.options
-- instance ToJSON Response where
--   toJSON = genericToJSON Chat.options
--   toEncoding = genericToEncoding Chat.options
