#include "../lib/session.h"
#include "../lib/rect.h"
#include "../lib/window.h"

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

	window_t win;
	window_create(&win, 0, 0, 10, 10);
	fill_window(&win, '1');

	refresh();
	fgetc(stdin);

	window_delete(&win);
	session_stop();

	return 0;
}
