function my_zoom_set (buffer) {
    browser_zoom_set(buffer, false, 100);
}
add_hook('create_buffer_late_hook', my_zoom_set);

zoom_levels = [ 1, 10, 25, 50, 75, 90, 100, 110,
                120, 125, 130, 140, 150, 200, 300, 500, 1000, 2000 ];
