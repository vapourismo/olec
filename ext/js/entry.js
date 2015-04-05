var KeyMap = require("keymap.js");

var keymap = new KeyMap();

keymap.bind(events.Control, KeyMap.charCode('q'), events.quit.bind(events));
keymap.bind(events.Control, KeyMap.charCode('r'), events.reload.bind(events));

events.keyHandler = function (mod, key) {
	log.debug("keyHandler", mod, key);
	keymap.trigger(mod, key);
};

events.dispatch();
