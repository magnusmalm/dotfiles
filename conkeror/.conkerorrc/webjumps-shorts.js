minibuffer_read_url_select_initial = false;

// selection searches
function create_selection_search(webjump, key) {
    interactive(webjump+"-selection-search",
                "Search " + webjump + " with selection contents",
                "find-url-new-buffer",
                $browser_object = function (I) {
                    return webjump + " " + I.buffer.top_frame.getSelection();});
    define_key(content_buffer_normal_keymap, key.toUpperCase(), webjump + "-selection-search");

    interactive("prompted-"+webjump+"-search", null,
                function (I) {
                    var term = yield I.minibuffer.read_url($prompt = "Search "+webjump+":",
                                                           $initial_value = webjump+" ");
                    browser_object_follow(I.buffer, FOLLOW_DEFAULT, term);
                });
    define_key(content_buffer_normal_keymap, key, "prompted-" + webjump + "-search");
}
// examples
create_selection_search("g","l");
create_selection_search("wikipedia","w");
create_selection_search("dictionary","D");
create_selection_search("amazon","a");
create_selection_search("youtube","u");

// some bookmarks
define_webjump("conk",  "http://conkeror.org");
define_webjump("efu",   "http://emacs-fu.blogspot.com");
define_webjump("lkml",  "http://lkml.org");
define_webjump("ew",    "http://emacswiki.org");

define_webjump("so",    "http://stackoverflow.com/search?q=%s");
define_webjump("yts",   "http://www.youtube.com/results?search_query=%s&aq=f");
define_webjump("imdb",  "http://www.imdb.com/find?s=all&q=%s");
