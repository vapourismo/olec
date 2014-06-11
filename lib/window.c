#include "window.h"
#include "aux.h"

void window_create(window_t* win, size_t x, size_t y, size_t w, size_t h) {
	*win = newwin(h, w, y, x);
}

void window_delete(window_t* win) {
	delwin(*win);
}

void window_get_bounds(const window_t* win, rect_t* bounds) {
	window_t w = *win;
	bounds->x = getbegx(w);
	bounds->y = getbegy(w);
	bounds->w = getmaxx(w);
	bounds->h = getmaxy(w);
}

void window_set_bounds(window_t* win, const rect_t* bounds) {
	window_t w = *win;
	if (w) delwin(w);
	*win = newwin(bounds->h, bounds->w, bounds->y, bounds->x);
}

void window_clear(window_t* win) {
	wclear(*win);
}
