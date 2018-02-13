module Keybase.Chat
  ( module Keybase.Chat.Types
  , open
  ) where

import Prelude ((.), ($), IO, print)

import Control.Applicative           (pure)
import Control.Concurrent.Async      (async, mapConcurrently_)
import Control.Concurrent.STM.TQueue (TQueue, readTQueue)
import Control.Monad.IO.Class        (MonadIO, liftIO)

import           Data.Aeson                 (decode, encode)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Lazy       as LBS
import           Data.Conduit               (ConduitM, (.|), runConduit)
import qualified Data.Conduit               as C
import qualified Data.Conduit.List          as CL
import           Data.Conduit.Process.Typed (createSource, withProcess_)
import           Data.Maybe                 (Maybe)
import           Data.Void                  (Void)
import           System.Process.Typed       (proc
                                            ,getStderr, getStdin, getStdout
                                            ,setStderr, setStdin, setStdout
                                            )

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
     => TQueue Request -> m ()
open req = do
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
  pure ()

-- modify :: (MonadReader Api m, MonadIO m)
--        => (Api -> TVar Handle)
--        -> (Handle -> Handle)
--        -> m ()
-- modify sel f = do
--   ref <- asks sel
--   liftIO $
