#include "window.h"
#include "aux.h"

struct _window {
	curswin_t* ref;
};

void window_create(window_t* win, size_t x, size_t y, size_t w, size_t h) {
	win->ref = newwin(h, w, y, x);
}

void window_delete(window_t* win) {
	delwin(win->ref);
}

void window_get_bounds(const window_t* win, rect_t* bounds) {
	bounds->x = getbegx(win->ref);
	bounds->y = getbegy(win->ref);
	bounds->w = getmaxx(win->ref);
	bounds->h = getmaxy(win->ref);
}

void window_set_bounds(window_t* win, const rect_t* bounds) {
	delwin(win->ref);
	win->ref = newwin(bounds->h, bounds->w, bounds->y, bounds->x);
}
