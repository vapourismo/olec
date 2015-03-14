#include "editorview.h"

#include <math.h>

void olec_editor_view_init(OlecEditorView* edview) {
	edview->num_lines = 0;
	edview->active_line = 0;
	edview->scroll_line = edview->scroll_col = 0;

	edview->frame = NULL;
}

void olec_editor_view_update(OlecEditorView* edview, OlecCursesFrame* frame) {
	edview->frame = frame;
}

void olec_editor_fix_viewport(OlecEditorView* edview) {
	size_t viewport_height = getmaxy(edview->frame);

	if (edview->active_line < edview->scroll_line)
		edview->scroll_line = edview->active_line;
	else if (edview->active_line >= edview->scroll_line + viewport_height)
		edview->scroll_line = edview->active_line - viewport_height + 1;
}

static
void ev_render_linenums(const OlecEditorView* edview) {
	size_t digits = floorf(log10f(edview->num_lines)) + 1;

	size_t scroll_line = edview->scroll_line;
	size_t viewport_height = getmaxy(edview->frame);

	for (size_t line = scroll_line;
	     line < edview->num_lines && line - scroll_line < viewport_height;
	     line++) {

		mvwprintw(edview->frame, line - scroll_line, 0, " %0*zu ", digits, line + 1);

		if (line == edview->active_line)
			mvwchgat(edview->frame, line - scroll_line, 0, digits + 2, 0, 3, NULL);
		else
			mvwchgat(edview->frame, line - scroll_line, 0, digits + 2, 0, 2, NULL);
	}
}

static
void ev_render_scrollbar(const OlecEditorView* edview) {
	float coverage = edview->num_lines;
	float vp_coverage = getmaxy(edview->frame);
	float s_coverage = edview->scroll_line;

	size_t s_height = roundf((s_coverage / coverage) * vp_coverage);
	size_t vp_height = ceilf((vp_coverage / coverage) * vp_coverage);

	size_t x = getmaxx(edview->frame) - 1;

	wattrset(edview->frame, COLOR_PAIR(1));

	for (size_t y = s_height; y < s_height + vp_height; y++)
		mvwaddch(edview->frame, y, x, ' ');
}

void olec_editor_view_render(const OlecEditorView* edview) {
	ev_render_linenums(edview);
	ev_render_scrollbar(edview);
}
