interactive("browse-buffer-history",
            "Browse the session history for the current buffer",
            function browse_buffer_history (I) {
                var b = check_buffer(I.buffer, content_buffer);
                var history = b.web_navigation.sessionHistory;

                if (history.count > 1) {
                    var entries = [];

                    for(var i = 0 ; i < history.count ; i += 1) {
                        entries[i] = history.getEntryAtIndex(i, false).URI.spec;
                    }

                    var url = yield I.minibuffer.read(
                        $prompt = "Go back or forward to:",
                        $completer = new all_word_completer($completions = entries),
                        $default_completion = history.index > 0 ? entries[history.index - 1] : entries[history.index + 1],
                        $auto_complete = "url",
                        $auto_complete_initial = true,
                        $auto_complete_delay = 0,
                        $require_match = true);

                    b.web_navigation.gotoIndex(entries.indexOf(url));
                } else {
                    I.window.minibuffer.message("No history");
                }
            });
