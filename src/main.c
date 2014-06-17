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

	init_pair(2, COLOR_YELLOW, COLOR_BLACK);
	init_pair(3, COLOR_BLUE, COLOR_WHITE);

	mainview_t mview;
	mainview_create(&mview);

	editor_t ed;
	ed.linenum_padding = 1;
	ed.linenum_min_width = 2;
	ed.linenum_color_normal = 2;
	ed.linenum_color_selected = 3;
	ed.text_margin = 1;

	editor_create(&ed, mview.viewport);

	ed.render_from = 1;
	ed.render_to = ed.lay_split.a.h;
	ed.lines = ed.lay_split.a.h;
	ed.current_line = 3;
	editor_update(&ed);

	window_fill(mview.statusbar, '2');
	editor_render(&ed);

	session_render();
	fgetc(stdin);

	/* finalize */
	session_stop();
	log_close();

	return 0;
}
