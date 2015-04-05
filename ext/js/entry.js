screen.__proto__.fill = function (chr) {
	var line = new Array(this.width + 1).join(chr);

	for (var y = 0; y < this.height; y++) {
		this.moveCursor(0, y);
		this.drawString(line);
	}

	this.render();
};

var keys = require('keys.js');
var app = require("app.js");

// Key bindings
app.keyMap.bind(events.Control, keys.charCode('q'), events.quit.bind(events));
app.keyMap.bind(events.Control, keys.charCode('r'), events.reload.bind(events));

app.statusLine = "Hello World";

// Run application
app.render();
events.dispatch();
