#include "window.h"
#include "aux.h"

window_t* window_new(size_t x, size_t y, size_t w, size_t h) {
	window_t* win = new(window_t);

	if (win) {
		win->ref = newwin(h, w, y, x);
	}

	return win;
}

void window_free(window_t* win) {
	if (win) {
		if (win->ref)
			delwin(win->ref);

		free(win);
	}
}

void window_get_bounds(const window_t* win, rect_t* bounds) {
	bounds->x = getbegx(win->ref);
	bounds->y = getbegy(win->ref);
	bounds->w = getmaxx(win->ref);
	bounds->h = getmaxy(win->ref);
}

void window_set_bounds(window_t* win, const rect_t* bounds) {
	if (win->ref)
		delwin(win->ref);

	win->ref = newwin(bounds->h, bounds->w, bounds->y, bounds->x);
}
