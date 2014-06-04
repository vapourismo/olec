#include "../lib/session.h"
#include "../lib/rect.h"
#include "../lib/window.h"
#include "../lib/layout.h"

#include <stdio.h>

void fill_rect(const rect_t* rect, char c) {
	for (size_t i = 0; i < rect->h; i++) {
		move(rect->y + i, rect->x);

		for (size_t j = 0; j < rect->w; j++)
			addch(c);
	}
}

void fill_window(const window_t* window, char c) {
	rect_t tmp;
	window_get_bounds(window, &tmp);
	fill_rect(&tmp, c);
}

int main(void) {
	session_start();

	rect_t bounds;
	window_t win = {stdscr};

	window_get_bounds(&win, &bounds);

	layout_t* lay = layout_new(&bounds, VSPLIT_ABS, 50);

	fill_window(&lay->a, 'A');
	fill_window(&lay->b, 'B');

	refresh();
	fgetc(stdin);

	layout_free(lay);
	session_stop();

	return 0;
}
