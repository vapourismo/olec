var KeyMap = require("keymap.js");

var keymap = new KeyMap();

keymap.bind(event.keys.control, event.keys.q, event.quit.bind(event));
keymap.bind(event.keys.control, event.keys.r, event.reload.bind(event));

event.keyHandler = keymap.trigger.bind(keymap);
event.dispatch();
