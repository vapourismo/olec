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

	mainview_t mview;
	mainview_create(&mview);

	render_viewport(mview.viewport);
	window_fill(mview.statusbar, '2');

	session_render();
	fgetc(stdin);

	/* finalize */
	session_stop();
	log_close();

	return 0;
}
