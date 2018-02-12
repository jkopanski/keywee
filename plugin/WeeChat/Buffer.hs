{-# language ForeignFunctionInterface #-}
module WeeChat.Buffer
  where

import Prelude (($))
import           Foreign.C.String
import           Foreign.Ptr
import           Data.Text (Text)
import qualified Data.Text as T
import           System.IO (IO)

import WeeChat.FFI

data Buffer = Buffer
  { name :: Text }

-- mkInputCb ::
newBuffer :: Text -> IO BufferHandle
newBuffer name = withCString (T.unpack name) $ \cname -> kw_buffer_new cname my_input_cb nullPtr nullPtr my_close_cb nullPtr nullPtr
