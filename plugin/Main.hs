module Main where

import RIO                           ((.), ($), RIO, runRIO, when)

import Control.Concurrent            (threadDelay)
import Control.Concurrent.STM.TQueue (newTQueue, readTQueue, writeTQueue)
import Control.Monad.STM             (atomically)
import Reactive.Banana
import Reactive.Banana.Frameworks
import System.IO                     (IO, hClose)

import Keybase.Chat

type EventSource a = (AddHandler a, a -> IO ())

addHandler :: EventSource a -> AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO ()
fire = snd

frpNetwork :: API -> (EventSource Request, EventSource Response) -> MomentIO ()
frpNetwork api (esreq, eskb) = do
    -- obtain input events
    ereq <- fromAddHandler (addHandler esreq)

    let req = input api
    reactimate $ (atomically . writeTQueue req) <$> ereq

eventLoop :: (EventSource Request, EventSource Response) -> IO ()
eventLoop (esreq, eskb) = do
  putStrLn "Enter keybase chat api commands."
  putStrLn "Enter \"exit\" to exit."

  loop
  where loop = do
          s <- getLine
          case s of
            "list" -> fire esreq (Request List)
            "exit" -> pure ()

          when (s /= "exit") loop

main :: IO ()
main = do
  -- setup commmunication queues
  req <- atomically newTQueue
  res <- atomically newTQueue
  let chatApi = API req res

  runRIO chatApi open
  putStrLn "connection opened"

  sources <- (,) <$> newAddHandler <*> newAddHandler
  network <- compile (frpNetwork chatApi sources)
  actuate network
  eventLoop sources

