#include "window.h"
#include "aux.h"
#include <string.h>

window_t* window_new(size_t x, size_t y, size_t w, size_t h) {
	return newwin(h, w, y, x);
}

void window_free(window_t* win) {
	delwin(win);
}

void window_get_bounds(const window_t* win, rect_t* bounds) {
	bounds->x = getbegx(win);
	bounds->y = getbegy(win);
	bounds->w = getmaxx(win);
	bounds->h = getmaxy(win);
}

void window_set_bounds(window_t* win, const rect_t* bounds) {
	delwin(win);
	window_t* tmp = newwin(bounds->h, bounds->w, bounds->y, bounds->x);
	memcpy(win, tmp, sizeof(window_t));
}
