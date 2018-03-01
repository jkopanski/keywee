{-# language
        ForeignFunctionInterface
      , OverloadedStrings #-}
module Plugin where

import RIO                           (runRIO)
import Control.Concurrent.STM.TQueue (newTQueue)
import Control.Monad.STM             (STM, atomically)
import System.IO                     (IO)

import Keybase.Chat
import WeeChat.Buffer

foreign export ccall keyweeInit :: IO ()

keyweeInit :: IO ()
keyweeInit = do
  req <- atomically newTQueue
  res <- atomically newTQueue
  let chatApi = API req res
  runRIO chatApi open

  _ <- newBuffer "keybase"
  pure ()
