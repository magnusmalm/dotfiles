require("session.js");
// require("mode-line-buttons.js");
require("favicon");
require("new-tabs.js");

session_auto_save_auto_load = true;

view_source_use_external_editor = true;
editor_shell_command = "xterm -e emacsclient -nw";

// modeline
add_hook("mode_line_hook", mode_line_adder(buffer_count_widget), true);
add_hook("mode_line_hook", mode_line_adder(loading_count_widget), true);
remove_hook("mode_line_hook", mode_line_adder(clock_widget));
remove_hook("mode_line_hook", mode_line_adder(current_buffer_scroll_position_widget));
add_hook("mode_line_hook", mode_line_adder(downloads_status_widget));
// load_paths.unshift("chrome://conkeror-contrib/content/");
// mode_line_add_buttons(standard_mode_line_buttons, true);

// favicons
add_hook("mode_line_hook", mode_line_adder(buffer_icon_widget), true);
read_buffer_show_icons = true;

// Color theme
theme_load_paths.unshift("~/src/");
theme_unload("default");
theme_load("conkeror-theme-zenburn");

function darken_page (I) {
    var styles='* { background: black !important; color: grey !important; }'+
        ':link, :link * { color: #4986dd !important; }'+
        ':visited, :visited * { color: #d75047 !important; }';
    var document = I.buffer.document;
    var newSS=document.createElement('link');
    newSS.rel='stylesheet';
    newSS.href='data:text/css,'+escape(styles);
    document.getElementsByTagName("head")[0].appendChild(newSS);
}

interactive("darken-page", "Darken the page in an attempt to save your eyes.",
            darken_page);
define_key(content_buffer_normal_keymap, "C-d", "darken-page");

user_pref("intl.charset.default", getenv("LANG").split(".")[1]);

define_key(content_buffer_normal_keymap, "d", "follow-new-buffer");

function define_switch_buffer_key (key, buf_num) {
    define_key(default_global_keymap, key,
               function (I) {
                   switch_to_buffer(I.window,
                                    I.window.buffers.get_buffer(buf_num));
               });
}
for (let i = 0; i < 10; ++i) {
    define_switch_buffer_key(String((i+1)%10), i);
}

interactive("switch-to-other-buffer",
            "Switch to the previously open buffer",
            function (I) {
                var blist = I.window.buffers.buffer_list
                if (blist.length > 1)
                    switch_to_buffer(I.window, blist[1]);
            });


interactive("switch-to-recent-buffer",
    "Prompt for a buffer and switch to it, displaying the list in last-visited order.",
    function (I) {
        switch_to_buffer(
            I.window,
            (yield I.minibuffer.read_buffer(
                $prompt = "Switch to buffer:",
                $buffers = I.window.buffers.buffer_history,
                $default = (I.window.buffers.count > 1 ?
                            I.window.buffers.buffer_history[1] :
                            I.buffer))));
    });

// don't open download buffer automatically
remove_hook("download_added_hook", open_download_buffer_automatically);

define_key(default_global_keymap, "C-x b", "switch-to-recent-buffer");

define_key(default_global_keymap, "M-page_down", "buffer-next");
define_key(default_global_keymap, "M-page_up", "buffer-previous");

define_key(default_global_keymap, "M-l", "cmd_scrollRight");
define_key(default_global_keymap, "M-j", "cmd_scrollLeft");

define_key(default_global_keymap, "M-i", "cmd_scrollLineUp");
define_key(default_global_keymap, "M-k", "cmd_scrollLineDown");

define_key(default_global_keymap, "M-I", "eye-guide-scroll-up");
define_key(default_global_keymap, "M-K", "eye-guide-scroll-down");

define_key(default_global_keymap, "M-J", "scroll-top-left");
define_key(default_global_keymap, "M-L", "cmd_scrollBottom");

define_key(default_global_keymap, "M-;", "isearch-forward");
define_key(default_global_keymap, "M-:", "isearch-backward");

define_key(default_global_keymap, "C-w", "kill-current-buffer");
define_key(default_global_keymap, "M-a", "execute-extended-command");

define_key(default_global_keymap, "M-c", "cmd_copy");
define_key(default_global_keymap, "M-v", "paste-url");

define_key(default_global_keymap, "M-c", "cmd_copy");
define_key(default_global_keymap, "M-v", "paste-url");

define_key(default_global_keymap, "C-M-l", "cmd_selectCharNext");
define_key(default_global_keymap, "C-M-j", "cmd_selectCharPrevious");

define_key(default_global_keymap, "C-M-L", "cmd_selectWordNext");
define_key(default_global_keymap, "C-M-J", "cmd_selectWordPrevious");

define_key(default_global_keymap, "f23", "back");
define_key(default_global_keymap, "f22", "forward");

// - + s and - + x does not work for some reason
// although xev reports correct keys... They also work in Emacs
// define_key(default_global_keymap, "f17", "buffer-next");
// define_key(default_global_keymap, "f18", "buffer-previous");

define_key(default_global_keymap, "M-m", "switch-to-recent-buffer");

cwd = get_home_directory();
cwd.append("downloads");
