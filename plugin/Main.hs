module Main where

import RIO                           (runRIO)

import Control.Concurrent            (threadDelay)
import Control.Concurrent.STM.TQueue (newTQueue, writeTQueue)
import Control.Monad.STM             (atomically)

import System.IO             (IO, hClose)
import Keybase.Chat

main :: IO ()
main = do
  putStrLn "Enter keybase chat api commands."
  putStrLn "Enter \"exit\" to exit."

  req <- atomically newTQueue
  res <- atomically newTQueue
  let chatApi = API req res

  runRIO chatApi open
  putStrLn "connection opened"

  loop req
  where loop q = do
          s <- getLine
          case s of
            "exit" -> return ()
            "list" -> do
              let cmd = Request List
              atomically $ writeTQueue q cmd
              loop q
            _ -> loop q

