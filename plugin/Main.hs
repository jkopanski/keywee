module Main where

import RIO                           ((.), ($), (<$), ($>), (>>=)
                                     , RIO
                                     , asks, forever, runRIO, when
                                     )

import Control.Concurrent            (threadDelay)
import Control.Concurrent.Async      (async)
import Control.Concurrent.STM.TQueue (newTQueue, readTQueue, writeTQueue)
import Control.Lens                  (view)
import Control.Monad.STM             (atomically)
import Reactive.Banana
import Reactive.Banana.Frameworks
import System.IO                     (IO, hClose)

import FRP hiding (frpNetwork)
import Keybase.Chat

-- TODO: How To pass API as RIO env?
frpNetwork :: API -> EventSource Request -> MomentIO ()
frpNetwork api esreq = do
    -- obtain input events
    ereq <- fromAddHandler (addHandler esreq)
    eres <- liftIO (runRIO api registerApiResponse) >>= fromAddHandler

    let req = input api
        res = output api

    reactimate $ (atomically . writeTQueue req) <$> ereq
    reactimate $ putStrLn . show <$> eres

eventLoop :: EventSource Request -> IO ()
eventLoop esreq = do
  putStrLn "Enter keybase chat api commands."
  putStrLn "Enter \"exit\" to exit."

  loop
  where loop = do
          s <- getLine
          case s of
            "list" -> fire esreq (Request List Nothing)
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

  sources <- newAddHandler
  network <- compile (frpNetwork chatApi sources)
  actuate network
  eventLoop sources

