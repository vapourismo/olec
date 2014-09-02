#include <ncurses.h>

extern void olec_init(void) {
	initscr();
	noecho();
	raw();
	keypad(stdscr, true);
	start_color();
}

extern WINDOW* olec_stdwin(void) {
	return stdscr;
}
