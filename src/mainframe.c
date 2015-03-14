#include "mainframe.h"

void olec_main_frame_init(OlecMainFrame* frame) {
	frame->main_window = NULL;
	frame->status_window = NULL;

	olec_editor_view_init(&frame->editor_view);
	olec_main_frame_update(frame);

	frame->editor_view.num_lines = 500;
	frame->editor_view.scroll_line = 10;
	frame->editor_view.active_line = 11;
}

void olec_main_frame_update(OlecMainFrame* frame) {
	if (frame->main_window)
		delwin(frame->main_window);

	if (frame->status_window)
		delwin(frame->status_window);

	size_t global_width = getmaxx(stdscr);
	size_t main_height = getmaxy(stdscr) - 1;

	frame->main_window = derwin(stdscr, main_height, global_width, 0, 0);
	frame->status_window = derwin(stdscr, 1, global_width, main_height, 0);

	olec_editor_view_update(&frame->editor_view, frame->main_window);
}

void olec_main_frame_render(const OlecMainFrame* frame) {
	clear();

	olec_editor_view_render(&frame->editor_view);

	mvwprintw(frame->status_window, 0, 1, "[%ix%i] Put contents here (active_line = %zu)",
	          getmaxx(frame->status_window), getmaxy(frame->status_window),
	          frame->editor_view.active_line);
	mvwchgat(frame->status_window, 0, 0, getmaxx(frame->status_window), 0, 1, NULL);

	refresh();
}
