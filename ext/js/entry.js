event.setKeyHandler(function(mod, key) {
	log.debug("Key press: Mod = " + mod + ", Key = " + key);

	screen.clear();

	screen.moveCursor(0, 0);
	screen.drawString("Key press: Mod = " + mod + ", Key = " + key);

	screen.render();
});

event.dispatch();
