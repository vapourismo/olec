var keys = require('keys.js');

var VSplit = function (frame) {

};

var mainFrame = new Object();
mainFrame.globalKeyMap = new keys.KeyMap();

mainFrame.globalKeyMap.bind(events.Control, keys.charCode('q'), events.quit.bind(events));
mainFrame.globalKeyMap.bind(events.Control, keys.charCode('r'), events.reload.bind(events));

events.keyHandler = function (mod, key) {
	if (!mainFrame.globalKeyMap.trigger(mod, key)) {
		log.debug("Unbound key", mod, key);
	}
};

var win1 = screen.createSubFrame(10, 10, 10, 10);
var win2 = win1.createSubFrame(2, 2, 6, 6);

screen.__proto__.fill = function (chr) {
	var line = new Array(this.width + 1).join(chr);

	for (var y = 0; y < this.height; y++) {
		this.moveCursor(0, y);
		this.drawString(line);
	}

	this.render();
};

win1.fill('1');
win2.fill('2');

mainFrame.globalKeyMap.bind(0, keys.charCode(' '), function (m, k) {
	win2.setBounds(0, 0, 10, 10);

	win1.fill('1');
	win2.fill('2');

	win1.render();
});

events.dispatch();
