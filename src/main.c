#include "../lib/log.h"
#include "../lib/term/session.h"
#include "../lib/term/views/mainview.h"
#include "../lib/term/views/editor.h"
#include <ncurses.h>

#define COLFAC (100 / 255)

int main(int argc, char** argv) {
	/* init app */
	log_open("application.log");
	session_start();

	init_pair(1, COLOR_BLACK, COLOR_WHITE);
	init_pair(2, COLOR_YELLOW, COLOR_BLACK);
	init_pair(3, COLOR_BLUE, COLOR_WHITE);

	mainview_t mview;
	mainview_create(&mview);

	editor_t ed;
	editor_create(&ed, mview.viewport);

	ed.num_lines = 40;

	attrset(COLOR_PAIR(1));
	window_fill(mview.statusbar, ' ');
	attrset(0);

	editor_render(&ed);

	session_render();
	fgetc(stdin);

	/* finalize */
	session_stop();
	log_close();

	return 0;
}
