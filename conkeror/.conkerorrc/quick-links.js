interactive("open-gmail", "Go to gmail", "follow-new-buffer-background",
            $browser_object = "https://mail.google.com");
define_key(content_buffer_normal_keymap, "f2", "open-gmail");

interactive("open-gmail", "Go to Google calendar", "follow-new-buffer-background",
            $browser_object = "https://www.google.com/calendar");
define_key(content_buffer_normal_keymap, "f3", "open-google-calendar");


interactive("open-all",
    "opens bookmarks I visit frequently",
    function(I){
         load_url_in_new_buffer("https://mail.google.com",I.window);
         load_url_in_new_buffer("https://www.google.com/calendar",I.window);
         load_url_in_new_buffer("https://www.reddit.com/",I.window);
    });
define_key(content_buffer_normal_keymap, "f1", "open-all");
