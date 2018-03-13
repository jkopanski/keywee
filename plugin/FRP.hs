{-# language
        OverloadedStrings
      , RecursiveDo
      , ScopedTypeVariables #-}
module FRP where

import Prelude                       hiding (empty, id, lookup)
import RIO                           ((.), ($), (<$), ($>), (>>=), (>=>)
                                     , RIO
                                     , asks, foldM, forever, runRIO, when
                                     )
import Control.Concurrent.Async      (async)
import Control.Concurrent.STM.TQueue (newTQueue, readTQueue, writeTQueue)
import Control.Lens                  (view)
import Control.Monad.STM             (atomically)
import Data.Map
import Data.Semigroup                ((<>))
import Data.Text                     (Text, unpack)

import Reactive.Banana               hiding (empty)
import Reactive.Banana.Frameworks
import System.IO                     (IO, hClose, hPutStr, stderr)

import Keybase.Chat
import qualified WeeChat.Buffer      as WC

-- Program state
type Id = Text
type Buffers = Map Id WC.Buffer

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
  liftIO $ async $ forever (atomically (readTQueue res) >>= fire)
  pure addHandler

-- TODO: How To pass API as RIO env?
frpNetwork :: API -> EventSource Request -> MomentIO ()
frpNetwork api esreq = mdo
  -- obtain input events
  ereq <- fromAddHandler (addHandler esreq)
  eres <- liftIO (runRIO api registerApiResponse) >>= fromAddHandler

  let req = input api
      res = output api
      (eError, eResult) = splitResponse eres
      eInbox = let go (Inbox _ _) = True
                   go _           = False
                in filterE go eResult

  -- split single Inbox event to separate Conversation events
  -- Inbox { conversations :: [Conversation] }
  (eConv, fireConv) <- newEvent
  reactimate $ (sequence_ . fmap fireConv) . conversations <$> eInbox

  -- filter out conversations that already have opened buffers
  let eNewConversation = filterApply ((\b c -> not $ member (id c) b) <$> bBuffers) eConv
  (eNewBuf, fireNewBuf) <- newEvent
  -- open new buffer for each conversation and fire appropriate action
  reactimate $ (makeBuffer >=> fireNewBuf) <$> eNewConversation

  let insertBuffer :: WC.Buffer -> Buffers -> Buffers
      insertBuffer buf = insert (WC.id buf) buf

  (bBuffers :: Behavior Buffers)
    <- accumB empty $ insertBuffer <$> eNewBuf

  -- fire separate action for each conversation
  reactimate $ (atomically . writeTQueue req) <$> ereq
  reactimate $ hPutStr stderr . unpack <$> eError

splitResponse :: Event Response -> (Event Text, Event Result)
splitResponse e =
  ( filterJust $ fromError <$> e
  , filterJust $ fromResult <$> e
  ) where fromError (Error err)   = Just err
          fromError (Result _)    = Nothing
          fromResult (Error _)    = Nothing
          fromResult (Result res) = Just res

makeBuffer :: MonadIO m => Conversation -> m WC.Buffer
makeBuffer conv = do
  let chanName = name $ channel conv
      ident = id conv
  handle <- liftIO $ WC.newBuffer chanName
  pure $ WC.Buffer ident chanName handle
