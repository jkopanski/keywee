module WeeChat.Monad where

import FRP

import Control.Lens                  (Lens', lens)
import Control.Concurrent.STM.TQueue (TQueue)

import Keybase.Chat

class HasEventSource env where
  eventSource :: Lens' env (EventSource Request)

instance HasEventSource ES where
  eventSource = lens esreq (\a es -> a { esreq = es })

data ES = ES { esreq :: !(EventSource Request) }
