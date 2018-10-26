{-# language
        DataKinds
      , TypeFamilies #-}
module Data.Conduit.Process.Typed.Flush
  ( sourceTQueue
  , sinkTQueue
  , createSinkFlush
  , flush
  ) where

import Control.Concurrent.STM.TQueue (TQueue, readTQueue, writeTQueue)
import Control.Monad                 (Monad, (>>=), fmap, forever)
import Control.Monad.IO.Class        (MonadIO, liftIO)
import Control.Monad.STM             (STM, atomically)

import qualified Data.ByteString            as S
import           Data.Conduit               (ConduitM, yield)
import qualified Data.Conduit               as C
import qualified Data.Conduit.Binary        as CB
import qualified Data.Conduit.Combinators   as CC
import qualified Data.Conduit.List          as CL
import           Data.Conduit.Process.Typed (createPipe)
import           Data.Function              ((.), ($))
import qualified Data.Sequences             as Seq
import           Data.Void                  (Void)
import           System.Process.Typed       (StreamSpec, StreamType (STInput))
import           System.IO                  (hClose)

-- | The 3 functions below were taken from stm-conduit which is BSD3 licensed
-- | Copyright (c) Clark Gaebel
-- | TODO: remove after version bump on stackage
-- | A simple wrapper around a "TQueue". As data is pushed into the queue, the
--   source will read it and pass it down the conduit pipeline.
sourceTQueue :: MonadIO m => TQueue a -> ConduitM () a m ()
sourceTQueue q = forever $ liftSTM (readTQueue q) >>= yield

sinkTQueue :: MonadIO m => TQueue a -> ConduitM a Void m ()
sinkTQueue q = CL.mapM_ (liftSTM . writeTQueue q)

liftSTM :: MonadIO m => STM a -> m a
liftSTM = liftIO . atomically

-- | Alternative to default @createSink@ from @Data.Conduit.Process.Typed@
--   that creates sink that can be flushed
createSinkFlush :: MonadIO m => StreamSpec 'STInput (ConduitM (C.Flush S.ByteString) o m ())
createSinkFlush = CB.sinkHandleFlush `fmap` createPipe

flush :: Monad m => ConduitM (C.Flush a) (C.Flush a) m ()
flush = CC.concatMap (: (Seq.singleton C.Flush))
