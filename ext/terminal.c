#include <ncurses.h>

extern void olec_initterm(void) {
	initscr();
	raw();
	noecho();
	keypad(stdscr, true);
	start_color();
}

extern int olec_get_width(const WINDOW* win) {
	return win->_maxx + 1;
}

extern int olec_get_height(const WINDOW* win) {
	return win->_maxy + 1;
}

extern int olec_get_x(const WINDOW* win) {
	return win->_begx;
}

extern int olec_get_y(const WINDOW* win) {
	return win->_begy;
}
