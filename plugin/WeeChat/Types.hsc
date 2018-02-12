{-# language
        CApiFFI
      , EmptyDataDecls
      , ForeignFunctionInterface #-}
module WeeChat.Types where

import Foreign.C.Types
import Foreign.Ptr     (Ptr)

#include <weechat/weechat-plugin.h>

newtype RC = RC { rc :: CInt }
#{enum RC, RC
 , pluginOk    = WEECHAT_RC_OK
 , pluginOkEat = WEECHAT_RC_OK_EAT
 , pluginError = WEECHAT_RC_ERROR
 }

data {-# CTYPE "weechat/weechat-plugin.h" "struct t_gui_buffer" #-} Buffer
data {-# CTYPE "weechat/weechat-plugin.h" "struct t_weechat_plugin" #-} Plugin
