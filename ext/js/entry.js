var target = screen.createSubFrame(10, 10, 30, 10);

event.keyHandler = function (mod, key) {
	if (mod == event.keys.control && key == event.keys.q) {
		event.quit();
		return;
	} else if (mod == event.keys.control && key == event.keys.r) {
		event.reload();
		return;
	}

	log.debug("Key press:", mod, key);

	target.clear();

	target.moveCursor(0, 0);
	target.drawString("Key press: Mod = " + mod + ", Key = " + key);

	target.render();
};

event.dispatch();
