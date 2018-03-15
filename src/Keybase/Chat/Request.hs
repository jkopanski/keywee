module Keybase.Chat.Request where

import Data.Maybe (Maybe (..))

import Keybase.Chat.Types

list :: Request
list = Request
  { method = List
  , params = Nothing
  }

read :: ConversationId -> Request
read conv = Request
  { method = Read
  , params = Just Params
    { options = Options { conversation_id = conv} }
  }
