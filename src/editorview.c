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

void olec_editor_view_render_lines(const OlecEditorView* edview) {
	size_t digits = floorf(log10f(edview->num_lines)) + 1;

	for (size_t line = edview->scroll_line;
	     line < edview->num_lines && line - edview->scroll_line < ((size_t) getmaxy(edview->frame));
	     line++) {

		mvwprintw(edview->frame, line - edview->scroll_line, 0, " %*zu ", digits, line + 1);

		if (line == edview->active_line)
			mvwchgat(edview->frame, line - edview->scroll_line, 0, digits + 2, 0, 3, NULL);
		else
			mvwchgat(edview->frame, line - edview->scroll_line, 0, digits + 2, 0, 2, NULL);
	}
}

void olec_editor_view_render(const OlecEditorView* edview) {
	olec_editor_view_render_lines(edview);
}
