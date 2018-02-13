{-# language ForeignFunctionInterface #-}
module Plugin where

import           System.IO (IO)

import WeeChat.Buffer

foreign export ccall keyweeInit :: IO ()

keyweeInit :: IO ()
keyweeInit = do
  _ <- newBuffer "keybase"
  pure ()
