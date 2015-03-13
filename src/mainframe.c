#include "mainframe.h"

void olec_main_frame_init(OlecMainFrame* frame) {
	size_t global_width = getmaxx(stdscr);
	size_t main_height = getmaxy(stdscr) - 1;

	frame->main = derwin(stdscr, main_height, global_width, 0, 0);
	frame->status = derwin(stdscr, 1, global_width, main_height, 0);
}

void olec_main_frame_update(OlecMainFrame* frame) {
	OlecCursesFrame* del_main = frame->main;
	OlecCursesFrame* del_status = frame->status;

	olec_main_frame_init(frame);

	if (del_main)
		delwin(del_main);

	if (del_status)
		delwin(del_status);
}

void olec_main_frame_render(const OlecMainFrame* frame) {
	refresh();
}
