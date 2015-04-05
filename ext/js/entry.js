var KeyMap = require("keymap.js");

var keymap = new KeyMap();

keymap.bind(events.Control, "q".charCodeAt(0), events.quit.bind(events));
keymap.bind(events.Control, "r".charCodeAt(0), events.reload.bind(events));

events.keyHandler = function (mod, key) {
	log.debug("keyHandler", mod, key);
	keymap.trigger(mod, key);
};

events.dispatch();
