#include "../lib/log.h"
#include "../lib/term/session.h"
#include "../lib/term/window.h"
#include "../lib/term/layout.h"
#include "../lib/term/views/mainview.h"
#include <ncurses.h>

int main(int argc, char** argv) {
	/* init app */
	log_open("application.log");
	session_start();

	window_t root;
	getyx(stdscr, root.y, root.x);
	getmaxyx(stdscr, root.h, root.w);

	mainview_t mview;
	mainview_create(&mview, &root);

	window_fill(mainview_get_viewport(&mview), '1');
	window_fill(mainview_get_statusbar(&mview), '2');

	session_render();
	fgetc(stdin);

	/* finalize */
	session_stop();
	log_close();

	return 0;
}
