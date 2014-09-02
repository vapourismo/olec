#include <ncurses.h>

extern void olec_init(void) {
	initscr();
	noecho();
	raw();
	keypad(stdscr, true);
	start_color();
}

extern int olec_width(void) {
	return getmaxx(stdscr) + 1;
}

extern int olec_height(void) {
	return getmaxy(stdscr) + 1;
}

extern WINDOW* olec_parent(WINDOW* win) {
	return win->_parent;
}
