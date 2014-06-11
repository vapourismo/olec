#include "session.h"
#include "log.h"
#include <ncurses.h>

int session_start() {
	if (!initscr()) {
		error("Failed to initialize 'stdscr'");
		return 0;
	}

	/* configure terminal */
	noecho();
	raw();
	timeout(-1);

	/* initialize colors */
	start_color();

	return 1;
}

void session_stop() {
	endwin();
}
