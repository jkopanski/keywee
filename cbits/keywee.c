#include <stdlib.h>
#include <weechat/weechat-plugin.h>
#include <HsFFI.h>
#if __GLASGOW_HASKELL__
#include <Rts.h>
#endif

#include "keywee.h"

extern void keyweeInit(void);

WEECHAT_PLUGIN_NAME("keywee");
WEECHAT_PLUGIN_DESCRIPTION(N_("Keybase chat plugin for WeeChat"));
WEECHAT_PLUGIN_AUTHOR("Jakub Kopański <jkopansk@gmail.com>");
WEECHAT_PLUGIN_VERSION("0.1");
WEECHAT_PLUGIN_LICENSE("GPL3");

struct t_weechat_plugin *weechat_keywee_plugin = NULL;

int
my_input_cb (const void *pointer, void *data,
             struct t_gui_buffer *buffer, const char *input_data)
{
  weechat_printf (buffer, "Text: %s", input_data);
  return WEECHAT_RC_OK;
}

int
my_close_cb (const void *pointer, void *data, struct t_gui_buffer *buffer)
{
  weechat_printf (NULL, "Buffer '%s' will be closed!",
                  weechat_buffer_get_string (buffer, "name"));
  return WEECHAT_RC_OK;
}

int
command_keybase_cb (const void *pointer,
                    void *data,
                    struct t_gui_buffer *buffer,
                    int argc,
                    char **argv,
                    char **argv_eol)
{
  if (argc > 1)
  {
    weechat_command (NULL, argv_eol[1]);
    weechat_command (NULL, argv_eol[1]);
  }

  return WEECHAT_RC_OK;
}

int
weechat_plugin_init (struct t_weechat_plugin *plugin,
                     int argc,
                     char *argv[])
{
  int hsargc = 1;
  char* hsargv[] = { "+RTS", NULL };
  char** pargv = hsargv;
  RtsConfig conf = defaultRtsConfig;
  conf.rts_opts_enabled = RtsOptsAll;
  hs_init_ghc(&hsargc, &pargv, conf);

  weechat_plugin = plugin;
  keyweeInit();
  return WEECHAT_RC_OK;
}

int
weechat_plugin_end (struct t_weechat_plugin *plugin)
{
  hs_exit();
  return WEECHAT_RC_OK;
}
