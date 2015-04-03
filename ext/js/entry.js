event.setKeyHandler(function(mod, key) {
	log.debug("Key press: Mod = " + mod + ", Key = " + key);
});

event.dispatch();
