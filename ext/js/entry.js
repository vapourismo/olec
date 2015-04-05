var keys = require('keys.js');

var mainFrame = new Object();
mainFrame.globalKeyMap = new keys.KeyMap();

mainFrame.globalKeyMap.bind(events.Control, keys.charCode('q'), events.quit.bind(events));
mainFrame.globalKeyMap.bind(events.Control, keys.charCode('r'), events.reload.bind(events));

events.keyHandler = function (mod, key) {
	if (!mainFrame.globalKeyMap.trigger(mod, key)) {
		log.debug("Unbound key", mod, key);
	}
};

events.dispatch();
