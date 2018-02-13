module Main where

import Control.Concurrent            (threadDelay)
import Control.Concurrent.STM.TQueue (newTQueue, writeTQueue)
import Control.Monad.STM             (atomically)

import System.IO             (IO, hClose)
import Keybase.Chat

main :: IO ()
main = do
  putStrLn "Enter keybase chat api commands."
  putStrLn "Enter \"exit\" to exit."

  q <- atomically newTQueue
  open q
  putStrLn "connection opened"

  loop q
  where loop q = do
          s <- getLine
          case s of
            "exit" -> return ()
            "list" -> do
              let req = Request List
              atomically $ writeTQueue q req
              loop q
            _ -> loop q

