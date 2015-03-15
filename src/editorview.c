#include "editorview.h"

#include <math.h>

static
void ev_fix_viewport(OlecEditorView* edview) {
	size_t viewport_height = getmaxy(edview->frame);

	if (edview->editor.cursor_line < edview->scroll_line)
		edview->scroll_line = edview->editor.cursor_line;
	else if (edview->editor.cursor_line >= edview->scroll_line + viewport_height)
		edview->scroll_line = edview->editor.cursor_line - viewport_height + 1;
}

static
bool ev_kb_up(OlecEditorView* edview, OlecKeyModifier mod, OlecKeySymbol key) {
	olec_editor_move_cursor_relative(&edview->editor, -1, 0);
	ev_fix_viewport(edview);

	return true;
}

static
bool ev_kb_down(OlecEditorView* edview, OlecKeyModifier mod, OlecKeySymbol key) {
	olec_editor_move_cursor_relative(&edview->editor, 1, 0);
	ev_fix_viewport(edview);

	return true;
}

void olec_editor_view_init(OlecEditorView* edview) {
	olec_editor_init(&edview->editor);
	olec_key_map_init(&edview->keymap);

	edview->scroll_line = edview->scroll_col = 0;
	edview->frame = NULL;

	olec_key_map_bind(&edview->keymap, 0, GDK_KEY_Up,
	                  (OlecKeyHook) ev_kb_up, edview);
	olec_key_map_bind(&edview->keymap, 0, GDK_KEY_Down,
	                  (OlecKeyHook) ev_kb_down, edview);

	olec_editor_insert_lines(&edview->editor, 9);
}

void olec_editor_view_update(OlecEditorView* edview, OlecCursesFrame* frame) {
	edview->frame = frame;
}

static
size_t ev_render_linenums(const OlecEditorView* edview) {
	size_t digits = floorf(log10f(edview->editor.num_lines)) + 1;

	size_t scroll_line = edview->scroll_line;
	size_t viewport_height = getmaxy(edview->frame);

	for (size_t line = scroll_line;
	     line < edview->editor.num_lines && line - scroll_line < viewport_height;
	     line++) {

		mvwprintw(edview->frame, line - scroll_line, 0, " %0*zu ", digits, line + 1);

		if (line == edview->editor.cursor_line)
			mvwchgat(edview->frame, line - scroll_line, 0, digits + 2, 0, 3, NULL);
		else
			mvwchgat(edview->frame, line - scroll_line, 0, digits + 2, 0, 2, NULL);
	}

	return digits + 2;
}

// static
// void ev_render_scrollbar(const OlecEditorView* edview) {
// 	float coverage = edview->editor.num_lines;
// 	float vp_coverage = getmaxy(edview->frame);
// 	float s_coverage = edview->scroll_line;

// 	size_t s_height = roundf((s_coverage / coverage) * vp_coverage);
// 	size_t vp_height = ceilf((vp_coverage / coverage) * vp_coverage);

// 	size_t x = getmaxx(edview->frame) - 1;

// 	wattrset(edview->frame, COLOR_PAIR(1));

// 	for (size_t y = s_height; y < s_height + vp_height; y++)
// 		mvwaddch(edview->frame, y, x, ' ');

// 	wattrset(edview->frame, 0);
// }

void olec_editor_view_render(const OlecEditorView* edview) {
	ev_render_linenums(edview);
	// ev_render_scrollbar(edview);
}
