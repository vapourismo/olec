#include "session.h"

/*
 * Session
 */

int session_start(session_t* s) {
	s->root = initscr();

	if (!s->root)
		return 0;

	/* configure terminal */
	noecho();
	raw();
	timeout(-1);

	/* initialize colors */
	start_color();

	return 1;
}

void session_stop(session_t* s) {
	endwin();
	s->root = NULL;
}
