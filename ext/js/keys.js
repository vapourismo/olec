var KeyBinding = function (mod, key, callback) {
	this.mod = mod;
	this.key = key;
	this.callback = callback;
	this.higher = this.lower = null;
};

KeyBinding.prototype.compare = function (mod, key) {
	if (this.mod == mod) {
		if (this.key == key) {
			return 0;
		} else if (this.key < key) {
			return -1;
		} else {
			return 1;
		}
	} else if (this.mod < mod) {
		return -1;
	} else {
		return 1;
	}
};

KeyBinding.prototype.insert = function (other) {
	var cmp = this.compare(other.mod, other.key);

	if (cmp == 0) {
		this.callback = other.callback;
	} else if (cmp == -1) {
		if (this.higher) {
			this.higher.insert(other);
		} else {
			this.higher = other;
		}
	} else {
		if (this.lower) {
			this.lower.insert(other);
		} else {
			this.lower = other;
		}
	}
};

KeyBinding.prototype.find = function (mod, key) {
	var cmp = this.compare(mod, key);

	if (cmp == 0)
		return this;
	else if (cmp == -1 && this.higher)
		return this.higher.find(mod, key);
	else if (cmp == 1 && this.lower)
		return this.lower.find(mod, key);
	else
		return null;
};

var KeyMap = function () {
	this.root = null;
};

KeyMap.prototype.bind = function (mod, key, callback) {
	var elem = new KeyBinding(mod, key, callback);

	if (this.root)
		this.root.insert(elem);
	else
		this.root = elem;
};

KeyMap.prototype.trigger = function (mod, key) {
	if (!this.root)
		return false;

	var elem = this.root.find(mod, key);
	if (!elem)
		return false;

	elem.callback.call(null, mod, key);
	return true;
};

exports.KeyMap = KeyMap;

exports.charCode = function (char) {
	if (typeof(char) == "string" && char.length > 0) {
		return char.charCodeAt(0);
	}
};
