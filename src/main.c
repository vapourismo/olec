#include "../lib/log.h"
#include "../lib/term/session.h"
#include "../lib/term/window.h"
#include "../lib/term/layout.h"
#include "../lib/term/views/mainview.h"

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
	/* init app */
	log_open("application.log");
	session_start();

	init_pair(1, COLOR_BLACK, COLOR_WHITE);

	mainview_t mview;
	mainview_init(&mview, &stdscr);

	fill_window(mainview_get_viewport(&mview), '1');

	attrset(COLOR_PAIR(1));
	fill_window(mainview_get_statusbar(&mview), '2');

	refresh();
	fgetc(stdin);

	/* finalize */
	session_stop();
	log_close();

	return 0;
}
