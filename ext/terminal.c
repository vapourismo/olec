#include <ncurses.h>

extern void terminal_begin(void) {
	initscr();
	raw();
	noecho();
}

extern void terminal_end(void) {
	endwin();
}

extern int terminal_width(void) {
	return stdscr->_maxx + 1;
}

extern int terminal_height(void) {
	return stdscr->_maxy + 1;
}

extern void terminal_move_cursor(int x, int y) {
	move(y, x);
}

extern void terminal_draw_string(const char* str) {
	addstr(str);
}

extern void terminal_draw_char(char c) {
	addch(c);
}

extern void terminal_render(void) {
	refresh();
}
