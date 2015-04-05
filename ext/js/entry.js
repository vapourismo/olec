var KeyMap = require("keymap.js");

var keymap = new KeyMap();

keymap.bind(event.keys.control, event.keys.q, event.quit.bind(event));
keymap.bind(event.keys.control, event.keys.r, event.reload.bind(event));

event.keyHandler = function (mod, key) {
	log.debug("keyHandler", mod, key);
	keymap.trigger(mod, key);
};

event.dispatch();
