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

	window_t win;
	window_create(&win, 0, 0, 0, 0);

	layout_t* lay = layout_sub_win(&win, VSPLIT_REL, 0.5);

	/* fill window and wait for input */
	fill_window(&lay->a, 'A');
	fill_window(&lay->b, 'B');

	refresh();
	fgetc(stdin);

	/* change window bounds */
	rect_t new_bounds;
	window_get_bounds(&win, &new_bounds);

	new_bounds.x += 10;
	new_bounds.y += 10;
	new_bounds.w -= new_bounds.x * 2;
	new_bounds.h -= new_bounds.y * 2;

	window_set_bounds(&win, &new_bounds);

	/* clear and update window, and wait for input */
	clear();
	layout_update(lay);

	fill_window(&lay->a, 'A');
	fill_window(&lay->b, 'B');

	refresh();
	fgetc(stdin);

	/* free everything */
	layout_free(lay);
	session_stop();

	return 0;
}
