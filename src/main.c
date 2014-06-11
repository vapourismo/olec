#include "../lib/session.h"
#include "../lib/rect.h"
#include "../lib/window.h"
#include "../lib/layout.h"
#include "../lib/log.h"
#include <stdio.h>
#include <time.h>

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

int main(int argc, char** argv) {
	log_open("application.log");
	session_start();

	window_t win;
	window_create(&win, 0, 0, 0, 0);

	layout_t lay;
	layout_sub_win(&lay, &win, VSPLIT_REL, 0.5);

	/* fill window and wait for input */
	fill_window(&lay.a, 'A');
	fill_window(&lay.b, 'B');

	refresh();
	fgetc(stdin);

	/* change window bounds */
	rect_t new_bounds;
	window_get_bounds(&win, &new_bounds);

	new_bounds.x += new_bounds.w / 4;
	new_bounds.y += new_bounds.h / 4;
	new_bounds.w -= new_bounds.x * 2;
	new_bounds.h -= new_bounds.y * 2;

	window_set_bounds(&win, &new_bounds);

	/* clear and update window, and wait for input */
	clear();
	layout_update(&lay);

	fill_window(&lay.a, 'A');
	fill_window(&lay.b, 'B');

	refresh();
	fgetc(stdin);

	/* free everything */
	layout_free(&lay);
	session_stop();
	log_close();

	return 0;
}
