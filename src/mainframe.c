#include "mainframe.h"

void olec_main_frame_init(OlecMainFrame* frame) {
	size_t global_width = getmaxx(stdscr);
	size_t main_height = getmaxy(stdscr) - 1;

	frame->main = derwin(stdscr, main_height, global_width, 0, 0);
	frame->status = derwin(stdscr, 1, global_width, main_height, 0);
}

void olec_main_frame_update(OlecMainFrame* frame) {
	if (frame->main)
		delwin(frame->main);

	if (frame->status)
		delwin(frame->status);

	olec_main_frame_init(frame);
	clear();
}

void olec_main_frame_render(const OlecMainFrame* frame) {
	mvwprintw(frame->status, 0, 1, "[%ix%i] Put status bar contents here",
	          getmaxx(frame->status), getmaxy(frame->status));
	mvwchgat(frame->status, 0, 0, getmaxx(frame->status), 0, 1, NULL);

	refresh();
}
