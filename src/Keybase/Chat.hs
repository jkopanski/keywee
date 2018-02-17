module Keybase.Chat
  ( module Keybase.Chat.Types
  , module Keybase.Chat.Monad
  , open
  ) where

import Prelude     ((.), ($), IO, print)
import RIO         (RIO, asks)

import Control.Applicative           (pure)
import Control.Concurrent.Async      (async, mapConcurrently_)
import Control.Concurrent.STM.TQueue (TQueue, readTQueue)
import Control.Lens                  (view)
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
import Keybase.Chat.Monad
import Keybase.Chat.Types

open :: HasApi env => RIO env ()
open = do
  let chat = setStdin createSinkFlush
           $ setStdout createSource
           $ setStderr createSource
           $ proc "keybase" ["chat", "api"]

  req <- asks (view command)
  res <- asks (view response)

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
                .| CL.catMaybes
                .| sinkTQueue res

          errput :: ConduitM () Void IO ()
          errput = getStderr p
                .| CL.mapM_ print

      in mapConcurrently_ runConduit [input, output, errput]
  pure ()
