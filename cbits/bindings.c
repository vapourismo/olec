#include <ncurses.h>

extern void olec_init(void) {
	initscr();
	noecho();
	raw();
	keypad(stdscr, true);
	start_color();
}

extern void olec_dispose(void) {
	endwin();
}

extern int olec_maxx(void) {
	return getmaxx(stdscr);
}

extern int olec_maxy(void) {
	return getmaxy(stdscr);
}

extern int olec_cursor_x(void) {
	return getcurx(stdscr);
}

extern int olec_cursor_y(void) {
	return getcury(stdscr);
}
