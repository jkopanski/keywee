{-# language ForeignFunctionInterface #-}
module Plugin where

import Control.Concurrent.STM.TQueue (newTQueue)
import Control.Monad.STM             (STM, atomically)
import System.IO                     (IO)

import Keybase.Chat
import WeeChat.Buffer

foreign export ccall keyweeInit :: IO ()

keyweeInit :: IO ()
keyweeInit = do
  req <- atomically newTQueue
  ev <- open req

  _ <- newBuffer "keybase"
  pure ()
