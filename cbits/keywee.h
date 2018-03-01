#ifndef PLUGIN_KEYWEE_H
#define PLUGIN_KEYWEE_H

#include <weechat/weechat-plugin.h>

#define weechat_plugin weechat_keywee_plugin

extern struct t_weechat_plugin *weechat_plugin;

int
my_input_cb (const void *pointer, void *data,
             struct t_gui_buffer *buffer, const char *input_data);

int
my_close_cb (const void *pointer, void *data, struct t_gui_buffer *buffer);

#endif /* PLUGIN_KEYWEE_H */
