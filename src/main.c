#include "../lib/log.h"
#include "../lib/term/session.h"
#include "../lib/term/window.h"
#include "../lib/term/layout.h"
#include "../lib/term/views/mainview.h"
#include <ncurses.h>

void render_viewport(window_t* target) {
	window_fill(target, '#');
}

int main(int argc, char** argv) {
	/* init app */
	log_open("application.log");
	session_start();

	window_t root;
	getyx(stdscr, root.y, root.x);
	getmaxyx(stdscr, root.h, root.w);

	root.x += root.w / 4;
	root.y += root.h / 4;
	root.h /= 2;
	root.w /= 2;

	mainview_t mview;
	mainview_create(&mview, &root);

	render_viewport(mview.viewport);
	window_fill(mview.statusbar, '2');

	session_render();
	fgetc(stdin);

	/* finalize */
	session_stop();
	log_close();

	return 0;
}
