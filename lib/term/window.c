#include "window.h"
#include "../aux.h"
#include <string.h>
#include <stdarg.h>
#include <malloc.h>
#include <ncurses.h>

typedef WINDOW curswin_t;

void window_create(window_t* win, size_t x, size_t y, size_t w, size_t h) {
	win->x = x;
	win->y = y;
	win->w = w;
	win->h = h;
}

void window_fill(const window_t* win, char c) {
	for (size_t i = 0; i < win->h; i++) {
		move(win->y + i, win->x);

		for (size_t j = 0; j < win->w; j++)
			addch(c);
	}
}

void window_clear(const window_t* win) {
	window_fill(win, ' ');
}

void window_move_cursor(const window_t* win, size_t x, size_t y) {
	if (window_encloses(win, x, y)) {
		move(win->y + y, win->x + x);
	}
}

int window_encloses(const window_t* win, size_t x, size_t y) {
	return
		x >= win->x &&
		y >= win->y &&
		x < win->x + win->w &&
		y < win->y + win->h;
}

void window_draw_string(const window_t* win, const char* message) {
	/* retrieve the global cursor position */
	size_t cur_x, cur_y;
	getyx(stdscr, cur_y, cur_x);

	/* if cursor is positioned within the window */
	if (window_encloses(win, cur_x, cur_y)) {
		size_t end_x = min(win->x + win->w,
		                   cur_x + strlen(message));
		addnstr(message, end_x - cur_x);
	}
}

void window_draw_format(const window_t* win, const char* format, ...) {
	/* retrieve the global cursor position */
	size_t cur_x, cur_y;
	getyx(stdscr, cur_y, cur_x);

	/* if the cursor is positioned within the window */
	if (window_encloses(win, cur_x, cur_y)) {
		size_t space = 1 + (win->x + win->w) - cur_x;

		/* no need to write if there is no space */
		if (space == 0) return;

		char* buffer = (char*) malloc(space);

		if (!buffer) return;

		/* prepare the variadic argument list */
		va_list vargs;
		va_start(vargs, format);

		/* format and write to screen if there is anything to be written */
		ssize_t n = vsnprintf(buffer, space, format, vargs);
		if (n > 0)
			addnstr(buffer, n);

		va_end(vargs);
		free(buffer);
	}
}

void window_vsplit_abs(const window_t* base,
                       window_t* left, window_t* right,
                       ssize_t sep) {
	if (sep < 0)
		sep = base->w - min(-sep, base->w);

	/* place left */
	left->x = base->x;
	left->w = min(sep, base->w);

	/* place right */
	right->x = left->x + left->w;
	right->w = base->w - left->w;

	/* left and right have the same height and y-position */
	left->y = right->y = base->y;
	left->h = right->h = base->h;
}

void window_vsplit_rel(const window_t* base,
                       window_t* left, window_t* right,
                       double sep) {
	window_vsplit_abs(base, left, right, base->w * sep);
}

void window_hsplit_abs(const window_t* base,
                       window_t* top, window_t* bottom,
                       ssize_t sep) {
	if (sep < 0)
		sep = base->h - min(-sep, base->h);

	/* place top */
	top->y = base->y;
	top->h = min(sep, base->h);

	/* place bottom */
	bottom->y = top->y + top->h;
	bottom->h = base->h - top->h;

	/* top and bottom have the same width and x-position */
	top->x = bottom->x = base->x;
	top->w = bottom->w = base->w;
}

void window_hsplit_rel(const window_t* base,
                       window_t* top, window_t* bottom,
                       double sep) {
	window_hsplit_abs(base, top, bottom, base->h * sep);
}

void window_maximize(window_t* window) {
	window->x = stdscr->_begx;
	window->y = stdscr->_begy;
	window->w = stdscr->_maxx + 1;
	window->h = stdscr->_maxy + 1;
}
