var Editor = function (frame) {
	this.parent = frame || screen;
	this.lines = [];
};

Editor.lineNumStylePair = style.definePair(
	style.defineColor(0, 0, 0),
	style.defineColor(750, 750, 750)
);

function numberWidth(num) {
	return Math.floor(Math.log10(num)) + 1;
}

Editor.prototype.render = function () {
	var numLines = this.lines.length;

	var rightPadding = 1;
	var leftPadding = 1;

	var lineNumWidth = numberWidth(numLines);
	var lineNumRectWidth = lineNumWidth + rightPadding + leftPadding;
	var contentWidth = this.parent.width - lineNumRectWidth - 1;

	var rightPadStr = new Array(rightPadding + 1).join(' ');
	var leftPadStr = new Array(leftPadding + 1).join(' ');

	for (var y = 0; y < numLines && y < this.lines.length; y++) {
		this.parent.moveCursor(0, y);
		this.parent.setStyle(style.Normal, Editor.lineNumStylePair);

		this.parent.drawString(leftPadStr);

		var lnValue = y + 1;
		for (var d = numberWidth(lnValue); d < lineNumWidth; d++)
			this.parent.drawString(" ");

		this.parent.drawString("" + lnValue);
		this.parent.drawString(rightPadStr);

		this.parent.setStyle(style.Normal, 0);

		this.parent.drawString(" ");
		this.parent.drawString(this.lines[y].substring(0, contentWidth));
	}
};

exports.Editor = Editor;
