var counter = 0

application.eventHandler = function () {
	debug.log(this, counter++);
}

application.main();
