interactive("modi", "The Mouseover DOM Inspector, or MODI for short, is a favelet (also known as a bookmarklet)"
            + " that allows you to view and manipulate the DOM of a web page simply "
            + "by mousing around the document (http://slayeroffice.com/tools/modi/v2.0/modi_help.html).",
           function(I) {
               z=I.buffer.document.body.appendChild(I.buffer.document.createElement('script'));
               z.language='javascript';
               z.type='text/javascript';
               z.src='http://slayeroffice.com/tools/modi/v2.0/modi_v2.0.js';
               z.id='modi';
           });

// Examine element properties and style.

interactive("examine-element",
            "Examine the attributes and style of a DOM node.",
            function print_attribute (I) {
                var element = yield read_browser_object(I);
                var list = [];
                var style = I.window.getComputedStyle(element);
                var attributes = element.attributes;
                var name = element.tagName.toLowerCase();

                if (element.id) {
                    name += "#" + element.id;
                }

                for (i = 0 ; i < element.classList.length ; i += 1) {
                    name += "." + element.classList.item(i);
                }
                
                for (i = 0 ; i < style.length ; i += 1) {
                    list.push([style.item(i), 1]);
                }

                for (i = 0 ; i < attributes.length ; i += 1) {
                    list.push([attributes.item(i).name, 2]);
                }
                
                yield I.minibuffer.read(
                    $prompt = name + ":",
                    $completer = new prefix_completer(
                        $completions = list.sort(),
                        $get_string = function(item) item[0],
                        $get_description = function(item) {
                            var s, value;

                            switch(item[1]) {
                            case 1:
                                s = "CSS property";
                                value = style.getPropertyValue(item[0]);
                                
                                break;
                                
                            case 2:
                                s = "Attribute";
                                value = element.getAttribute(item[0]);
                                
                                break;
                            }
                            
                            if (value) {
                                s += " with value " + value;
                            }
                            
                            return s;
                        }),
                    $auto_complete = true,
                    $auto_complete_initial = true,
                    $auto_complete_delay = 0,
                    $require_match = false);
            },
            $browser_object = browser_object_dom_node);

define_key(content_buffer_normal_keymap, "x", "examine-element");


var ref;

interactive("interval-reload", "Reload current buffer every n minutes", function (I) {
    var b = I.buffer;
    var i = yield I.minibuffer.read($prompt="Interval (mm:ss)?");

    if (i.indexOf(":") != -1) {
        mmss = i.split(":");
        i = ((parseInt(mmss[0]) * 60) + parseInt(mmss[1])) * 1000;
    } else {
        i = parseInt(i) * 1000;
    }

    ref = call_at_interval(function () {
                  reload(b);
    }, i);

    add_hook.call(b, "kill_buffer_hook", function() {
        ref.cancel();
    });
});

interactive("cancel-intervals", "Cancel all running interval reloads", function (I) {
    ref.cancel();
});
