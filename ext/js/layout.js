var HSplit = function (frame, height) {
	this.parent = frame || screen;
	this.height = height || 1;

	var targetHeight = 0;

	if (this.height >= 0)
		targetHeight = this.height;
	else
		targetHeight = this.parent.height + this.height;

	this.top = this.parent.createSubFrame(0, 0, this.parent.width, targetHeight);
	this.bottom = this.parent.createSubFrame(0, targetHeight, this.parent.width,
	                                         this.parent.height - targetHeight);
};

HSplit.prototype.update = function () {
	var targetHeight = 0;

	if (this.height >= 0)
		targetHeight = this.height;
	else
		targetHeight = this.parent.height + this.height;

	this.top.setBounds(0, 0, this.parent.width, targetHeight);
	this.bottom.setBounds(0, targetHeight, this.parent.width, this.parent.height - targetHeight);
};

exports.HSplit = HSplit;
