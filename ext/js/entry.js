var target = screen.createSubFrame(10, 10, 10, 10);

event.setKeyHandler(function(mod, key) {
	log.debug("Key press: Mod = " + mod + ", Key = " + key);

	target.clear();
	target.moveCursor(0, 0);
	target.drawString("Key press: Mod = " + mod + ", Key = " + key);
	target.render();
});

event.dispatch();
