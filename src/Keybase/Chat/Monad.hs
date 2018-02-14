module Keybase.Chat.Monad where

import Control.Lens                  (Lens', lens)
import Control.Concurrent.STM.TQueue (TQueue)

import Keybase.Chat.Types

class HasCommand env where
  command :: Lens' env (TQueue Request)

class HasResponse env where
  response :: Lens' env (TQueue Response)

class (HasCommand env, HasResponse env) => HasApi env where
  api :: Lens' env API

instance HasCommand API where
  command = lens input (\a cmd -> a { input = cmd })

instance HasResponse API where
  response = lens output (\a resp -> a { output = resp })

instance HasApi API where
  api a = a

data API = API
  { input  :: !(TQueue Request)
  , output :: !(TQueue Response)
  }
