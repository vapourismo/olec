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

void olec_editor_view_render_lines(const OlecEditorView* edview) {
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

void olec_editor_view_render(const OlecEditorView* edview) {
	olec_editor_view_render_lines(edview);
}
