var keys = require('keys.js');
var layout = require('layout.js');
var editor = require('editor.js');

var Application = function () {
	this.keyMap = new keys.KeyMap();
	this.hsplit = new layout.HSplit(screen, -1);
	this.editor = new editor.Editor(this.hsplit.top);

	this.statusLine = "";

	events.keyHandler = this.keyMap.trigger.bind(this.keyMap);
	events.resizeHandler = this.onResize.bind(this);
};

Application.statusStylePair = style.definePair(
	style.defineColor(0, 0, 0),
	style.defineColor(500, 500, 500)
);

Application.prototype.onResize = function () {
	this.hsplit.update();
	this.render();
};

Application.prototype.render = function () {
	// Render status bar
	this.hsplit.bottom.moveCursor(0, 0);
	this.hsplit.bottom.setStyle(style.Normal, Application.statusStylePair);

	var rightPadding = 1;
	var leftPadding = this.hsplit.bottom.width - this.statusLine.length - rightPadding;

	if (this.hsplit.bottom.width > 2) {
		this.hsplit.bottom.drawString(new Array(rightPadding + 1).join(' ') +
		                              this.statusLine +
		                              new Array(leftPadding + 1).join(' '));
	} else {
		this.hsplit.bottom.drawString(new Array(this.hsplit.bottom.width + 1).join(' '));
	}

	// Render editor
	this.editor.render();

	// Print to screen
	screen.render();
};


exports = new Application();
