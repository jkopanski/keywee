module Keybase.Chat
  ( module Keybase.Chat.Types
  , open
  ) where

import Prelude ((.), ($), (<$>), IO, fmap, print)

import Control.Applicative           ((<*>), pure)
import Control.Concurrent.Async      (async, mapConcurrently_, race_)
import Control.Concurrent.STM.TVar   (TVar)
import Control.Concurrent.STM.TQueue (TQueue, newTQueue, readTQueue)
import Control.Monad                 ((>>=), (>>), forever)
import Control.Monad.IO.Class        (MonadIO, liftIO)
import Control.Monad.Reader          (MonadReader, EnvType)
import Control.Monad.STM             (STM, atomically)

import           Data.Aeson                 (decode, encode, parseJSON)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as S
import qualified Data.ByteString.Lazy       as LBS
import           Data.Conduit               (ConduitM, (.|), runConduit, yield)
import qualified Data.Conduit               as C
import qualified Data.Conduit.Binary        as CB
import qualified Data.Conduit.List          as CL
import           Data.Conduit.Process.Typed (createPipe, createSink, createSource, withProcess_)
import           Data.Maybe                 (Maybe)
import           Data.Void                  (Void)
import           System.Process.Typed       (StreamSpec, StreamType (..)
                                            ,proc
                                            ,getStderr, getStdin, getStdout
                                            ,setStderr, setStdin, setStdout
                                            )
import           System.IO                  (hClose)

import Data.Conduit.Process.Typed.Flush
import Keybase.Chat.Types

-- data Connection = MkConnection
--   { handle :: TVar Handle
--   , request :: TQueue Request
--   , response :: TQueue Response
--   }

-- class HasConnection a where
--   conn :: a -> TVar Handle

-- instance HasConnection (TVar Handle) where
--   conn = identity

-- instance HasConnection Connection where
--   conn = handle

open :: MonadIO m
     => m (TQueue Request)
open = do
  req <- liftIO $ atomically newTQueue
  let chat = setStdin createSinkFlush
           $ setStdout createSource
           $ setStderr createSource
           $ proc "keybase" ["chat", "api"]

  _ <- liftIO $ async $
    withProcess_ chat $ \p ->
      let input :: ConduitM () Void IO ()
          input = sourceTQueue req
               .| CL.map (C.Chunk . LBS.toStrict . encode)
               .| flush
               .| getStdin p

          output :: ConduitM () Void IO ()
          output = getStdout p
                .| CL.map (decode . LBS.fromStrict :: ByteString -> Maybe Response)
                .| CL.mapM_ print

          errput :: ConduitM () Void IO ()
          errput = getStderr p
                .| CL.mapM_ print

      in mapConcurrently_ runConduit [input, output, errput]

  pure req

-- modify :: (MonadReader Api m, MonadIO m)
--        => (Api -> TVar Handle)
--        -> (Handle -> Handle)
--        -> m ()
-- modify sel f = do
--   ref <- asks sel
--   liftIO $
