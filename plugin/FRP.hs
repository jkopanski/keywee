{-# language OverloadedStrings #-}
module FRP where

import RIO                           ((.), ($), (<$), ($>), (>>=)
                                     , RIO
                                     , asks, forever, runRIO, when
                                     )
import Control.Concurrent.Async      (async)
import Control.Concurrent.STM.TQueue (newTQueue, readTQueue, writeTQueue)
import Control.Lens                  (view)
import Control.Monad.STM             (atomically)
import Data.Text                     (unpack)

import Reactive.Banana
import Reactive.Banana.Frameworks
import System.IO                     (IO, hClose, hPutStr, stderr)

import Keybase.Chat

import qualified FRP.Inbox           as Inbox

-- type Handler a = a -> IO ()
type EventSource a = (AddHandler a, a -> IO ())

addHandler :: EventSource a -> AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO ()
fire = snd

registerApiResponse :: HasApi env => RIO env (AddHandler Response)
registerApiResponse = do
  res <- asks (view response)
  (addHandler, fire) <- liftIO newAddHandler
  liftIO $ async $ forever $ (atomically (readTQueue res) >>= fire)
  pure addHandler

-- TODO: How To pass API as RIO env?
frpNetwork :: API -> EventSource Request -> MomentIO ()
frpNetwork api esreq = do
  -- obtain input events
  ereq <- fromAddHandler (addHandler esreq)
  eres <- liftIO (runRIO api registerApiResponse) >>= fromAddHandler

  let req = input api
      res = output api

  reactimate $ (atomically . writeTQueue req) <$> ereq
  reactimate $ respond <$> eres

respond :: Response -> IO ()
respond (Error err)  = hPutStr stderr $ unpack err
respond (Result res) = react res

react :: Result -> IO ()
react = Inbox.react . conversations
