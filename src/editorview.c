#include "editorview.h"

#include <math.h>
#include <ctype.h>

static
void ev_fix_viewport(OlecEditorView* edview) {
	size_t viewport_height = getmaxy(edview->frame);

	if (edview->editor.cursor_line < edview->scroll_line)
		edview->scroll_line = edview->editor.cursor_line;
	else if (edview->editor.cursor_line >= edview->scroll_line + viewport_height)
		edview->scroll_line = edview->editor.cursor_line - viewport_height + 1;
}

static
bool ev_kb_movement(OlecEditorView* edview, OlecKeyModifier mod, OlecKeySymbol key) {
	switch (key) {
		case GDK_KEY_Up:
			olec_editor_move_cursor_relative(&edview->editor, -1, 0);
			break;

		case GDK_KEY_Down:
			olec_editor_move_cursor_relative(&edview->editor, 1, 0);
			break;

		case GDK_KEY_Left:
			olec_editor_move_cursor_relative(&edview->editor, 0, -1);
			break;

		case GDK_KEY_Right:
			olec_editor_move_cursor_relative(&edview->editor, 0, 1);
			break;

		case GDK_KEY_BackSpace:
			if (edview->editor.cursor_col > 0) {
				olec_editor_move_cursor_relative(&edview->editor, 0, -1);
				olec_editor_remove_char(&edview->editor);
			} else if (edview->editor.cursor_line > 0 && edview->editor.num_lines > 1) {

			}

			break;

		default:
			return false;
	}

	ev_fix_viewport(edview);

	return true;
}

static
bool ev_kb_delete_line(OlecEditorView* edview, OlecKeyModifier mod, OlecKeySymbol key) {
	// olec_editor_remove_line(&edview->editor);
	olec_editor_insert_string(&edview->editor, "delete me", 9);
	return true;
}

void olec_editor_view_init(OlecEditorView* edview) {
	olec_editor_init(&edview->editor);
	olec_key_map_init(&edview->keymap);

	edview->scroll_line = edview->scroll_col = 0;
	edview->frame = NULL;

	// Movement
	olec_key_map_bind(&edview->keymap, 0, GDK_KEY_Up, (OlecKeyHook) ev_kb_movement, edview);
	olec_key_map_bind(&edview->keymap, 0, GDK_KEY_Down, (OlecKeyHook) ev_kb_movement, edview);
	olec_key_map_bind(&edview->keymap, 0, GDK_KEY_Left, (OlecKeyHook) ev_kb_movement, edview);
	olec_key_map_bind(&edview->keymap, 0, GDK_KEY_Right, (OlecKeyHook) ev_kb_movement, edview);
	olec_key_map_bind(&edview->keymap, 0, GDK_KEY_BackSpace, (OlecKeyHook) ev_kb_movement, edview);

	olec_key_map_bind(&edview->keymap, GDK_CONTROL_MASK, GDK_KEY_D,
	                  (OlecKeyHook) ev_kb_delete_line, edview);

	olec_editor_insert_lines(&edview->editor, 9);
}

void olec_editor_view_update(OlecEditorView* edview, OlecCursesFrame* frame) {
	edview->frame = frame;
}

static
void ev_render_line(const OlecEditorView* edview, size_t line, size_t vp_line, size_t vp_col) {
	size_t line_width = getmaxx(edview->frame) - vp_col - 2;

	OlecLineEditor* le = edview->editor.lines[line];

	mvwaddnstr(edview->frame, vp_line, vp_col,
	           le->contents, le->length > line_width ? line_width : le->length);
}

static
void ev_render_lines(const OlecEditorView* edview) {
	size_t digits = floorf(log10f(edview->editor.num_lines)) + 1;

	size_t scroll_line = edview->scroll_line;
	size_t viewport_height = getmaxy(edview->frame);

	for (size_t line = scroll_line;
	     line < edview->editor.num_lines && line - scroll_line < viewport_height;
	     line++) {

		// Render line number
		mvwprintw(edview->frame, line - scroll_line, 0, " %0*zu ", digits, line + 1);

		// Render line contents
		ev_render_line(edview, line, line - scroll_line, digits + 3);

		// Is active line?
		if (line == edview->editor.cursor_line) {
			mvwchgat(edview->frame, line - scroll_line, 0, digits + 2, 0, 3, NULL);

			wmove(edview->frame, line - scroll_line, digits + 3 + edview->editor.cursor_col);
			wcursyncup(edview->frame);
		} else {
			mvwchgat(edview->frame, line - scroll_line, 0, digits + 2, 0, 2, NULL);
		}
	}
}

static
bool ev_handle_key(OlecEditorView* edview, OlecKeyModifier mod, OlecKeySymbol key) {
	if (mod != 0 && mod != GDK_SHIFT_MASK)
		return false;

	if (key < 256 && isprint(key)) {
		return olec_editor_insert_char(&edview->editor, key & 0xFF);
	} else {
		return false;
	}
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
	ev_render_lines(edview);
	// ev_render_scrollbar(edview);
}

bool olec_editor_view_handle_event(OlecEditorView* edview, const OlecEvent* event) {
	return
		event->type == OLEC_KEY_PRESS &&
	    (olec_key_map_invoke(&edview->keymap, event->info.key_press.mod, event->info.key_press.key) ||
	     ev_handle_key(edview, event->info.key_press.mod, event->info.key_press.key));
}
