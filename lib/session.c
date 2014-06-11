#include "session.h"
#include <ncurses.h>

int session_start() {
	if (!initscr())
		return 0;

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
