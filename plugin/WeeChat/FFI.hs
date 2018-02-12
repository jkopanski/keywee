{-# language
        CApiFFI
      , EmptyDataDecls
      , ForeignFunctionInterface #-}
module WeeChat.FFI where

import Prelude (IO)

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

import WeeChat.Types

type InputBufferCb = Ptr () -> Ptr () -> BufferHandle -> CString -> IO RC
type CloseBufferCb = Ptr () -> Ptr () -> BufferHandle -> IO RC

type PluginHandle = Ptr Plugin
type BufferHandle = Ptr Buffer

-- foreign import ccall "kw_buffer_new"
--   kw_buffer_new :: CString -> FunPtr InputBufferCb -> FunPtr CloseBufferCb -> IO BufferHandle

foreign import capi "weechat/weechat-plugin.h weechat_buffer_new"
  kw_buffer_new :: CString
                -> FunPtr InputBufferCb -> Ptr () -> Ptr ()
                -> FunPtr CloseBufferCb -> Ptr () -> Ptr ()
                -> IO BufferHandle

foreign import capi "keywee.h &my_input_cb" my_input_cb :: FunPtr InputBufferCb

foreign import capi "keywee.h &my_close_cb" my_close_cb :: FunPtr CloseBufferCb

foreign import capi "keywee.h value weechat_plugin" weechat_plugin :: PluginHandle

-- foreign import ccall "wrapper kw_buffer_new"
--   kw_buffer_new :: CString -> IO BufferHandle
