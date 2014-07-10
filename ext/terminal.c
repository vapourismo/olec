#include <ncurses.h>

extern void terminal_begin(void) {
	initscr();
	raw();
	noecho();
	keypad(stdscr, true);
	start_color();
}

extern void terminal_end(void) {
	endwin();
}

extern int terminal_width(void) {
	return getmaxx(stdscr);
}

extern int terminal_height(void) {
	return getmaxy(stdscr);
}

extern int terminal_cursor_x(void) {
	return getcurx(stdscr);
}

extern int terminal_cursor_y(void) {
	return getcury(stdscr);
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

extern void terminal_clear(void) {
	clear();
}

extern void terminal_bind_pair(short pair, short fg, short bg) {
	init_pair(pair, fg, bg);
}

extern void terminal_bind_color(short col, short r, short g, short b) {
	init_color(col, r, g, b);
}

extern void terminal_attr_color(int pair) {
	attrset(COLOR_PAIR(pair));
}

extern void terminal_change_attr(int len, short pair) {
	chgat(len, 0, pair, NULL);
}
