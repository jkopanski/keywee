{-# language ForeignFunctionInterface #-}
module Plugin where

import           Control.Applicative      ((*>), pure)
import           Control.Concurrent.Async (Concurrently (..))
import           Data.Conduit             (await, yield, (.|), runConduit)
import qualified Data.Conduit.Binary      as CB
import qualified Data.Conduit.List        as CL
import           Data.Conduit.Process     (ClosedStream (..), CreateProcess (..), streamingProcess,
                                           proc, waitForStreamingProcess)
import           System.IO                (IO, stdin)
import           System.Process           (createPipe)

import WeeChat.Buffer

foreign export ccall keyweeInit :: IO ()

keyweeInit :: IO ()
keyweeInit = do
  _ <- newBuffer "keybase"
  pure ()

-- main :: IO ()
-- main = do
--   putStrLn "Enter lines of data. I'll base64-encode it."
--   putStrLn "Enter \"quit\" to exit."

--   ((toProcess, close), fromProcess, ClosedStream, cph) <-
--     streamingProcess (proc "keybase" ["chat", "api"])

--   (stdInReadH,  stdInWriteH)  <- createPipe
--   (stdErrReadH, stdErrWriteH) <- createPipe
--   (stdOutReadH, stdOutWriteH) <- createPipe

--   let chat = proc "keybase" ["chat", "api"]
--         { -- std_in  = UseHandle stdInReadH
--         -- , std_err = UseHandle stdErrWriteH
--         std_out = UseHandle stdOutWriteH
--         }

--       input = CB.sourceHandle stdin
--            .| CB.lines
--            .| inputLoop
--            .| toProcess

--       inputLoop = do
--           mbs <- await
--           case mbs of
--               Nothing -> close
--               Just "quit" -> close
--               Just bs -> do
--                   yield bs
--                   inputLoop

--       output = CB.sinkHandle stdOutReadH .| CL.mapM_
--         (\bs -> putStrLn $ "from process: " ++ show bs)

--   ec <- runConcurrently $
--     Concurrently input *>
--     Concurrently output *>
--     Concurrently (waitForStreamingProcess cph)

--   putStrLn $ "Process exit code: " ++ show ec
