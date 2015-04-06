var Editor = function (frame) {
	this.parent = frame || screen;
};

Editor.lineNumStylePair = style.definePair(
	style.defineColor(0, 0, 0),
	style.defineColor(750, 750, 750)
);

function numberWidth(num) {
	return Math.floor(Math.log10(num)) + 1;
}

Editor.prototype.render = function () {
	var numLines = this.parent.height;

	var rightPadding = 1;
	var leftPadding = 1;

	var lineNumWidth = numberWidth(numLines);
	var lineNumRectWidth = lineNumWidth + rightPadding + leftPadding;

	this.parent.setStyle(style.Normal, Editor.lineNumStylePair);

	var rightPadStr = new Array(rightPadding + 1).join(' ');
	var leftPadStr = new Array(leftPadding + 1).join(' ');

	for (var y = 0; y < numLines; y++) {
		this.parent.moveCursor(0, y);
		this.parent.drawString(leftPadStr);

		var lnValue = y + 1;
		for (var d = numberWidth(lnValue); d < lineNumWidth; d++)
			this.parent.drawString(" ");

		this.parent.drawString("" + lnValue);
		this.parent.drawString(rightPadStr);
	}
};

exports.Editor = Editor;
