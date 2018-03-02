{-# language OverloadedStrings #-}
module FRP.Inbox
  where

import Prelude hiding (id)
import System.IO

import Data.Text (unpack)
import Keybase.Chat.Types

react :: [Conversation] -> IO ()
react = mapM_ (hPutStrLn stderr . unpack . name . channel)
