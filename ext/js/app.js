var keys = require('keys.js');
var layout = require('layout.js');

var Application = function () {
	this.keyMap = new keys.KeyMap();
	this.hsplit = new layout.HSplit(screen, -1);

	this.statusLine = "";
	this.statusPair = style.definePair(style.defineColor(0, 0, 0),
	                                   style.defineColor(500, 500, 500));

	events.keyHandler = this.keyMap.trigger.bind(this.keyMap);
	events.resizeHandler = this.onResize.bind(this);
};

Application.prototype.onResize = function () {
	this.hsplit.update();
	this.render();
};

Application.prototype.render = function () {
	// Render status bar
	this.hsplit.bottom.moveCursor(0, 0);
	this.hsplit.bottom.setStyle(style.Normal, this.statusPair);

	var rightPadding = 1;
	var leftPadding = this.hsplit.bottom.width - this.statusLine.length - rightPadding;

	if (this.hsplit.bottom.width > 2) {
		this.hsplit.bottom.drawString(new Array(rightPadding + 1).join(' ') +
		                              this.statusLine +
		                              new Array(leftPadding + 1).join(' '));
	} else {
		this.hsplit.bottom.drawString(new Array(this.hsplit.bottom.width + 1).join(' '));
	}

	// Render all
	screen.moveCursor(0, 0);
	screen.render();
};


exports = new Application();
