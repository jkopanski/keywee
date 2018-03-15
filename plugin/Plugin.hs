{-# language
        ForeignFunctionInterface
      , OverloadedStrings #-}
module Plugin where

import Prelude hiding (init)
import RIO                           (runRIO)
import Control.Concurrent.STM.TQueue (newTQueue)
import Control.Monad.STM             (STM, atomically)
import System.IO                     (IO)

import Reactive.Banana
import Reactive.Banana.Frameworks

import FRP
import Keybase.Chat
import WeeChat.Buffer

foreign export ccall keyweeInit :: IO ()

keyweeInit :: IO ()
keyweeInit = do
  req <- atomically newTQueue
  res <- atomically newTQueue
  let chatApi = API req res
  runRIO chatApi open

  sources <- newAddHandler
  network <- compile (frpNetwork chatApi sources)
  actuate network

  -- get conversations on init
  fire sources list
  pure ()
