#include "editor.h"
#include "../../aux.h"
#include <ncurses.h>
#include <math.h>

static const char* _space = "";

void editor_update(editor_t* ed) {
	ed->linenum_width = max(ed->linenum_min_width, ceil(log10(ed->lines + 1)));
	ed->lay_split.seperator.absolute = ed->linenum_width + 2 * ed->linenum_padding;
	layout_update(&ed->lay_split);
}

void editor_render_linenums(editor_t* ed) {
	attrset(COLOR_PAIR(ed->linenum_color_normal));

	ed->render_to = min(ed->lines, ed->render_to);

	for (size_t ln = ed->render_from; ln <= ed->render_to; ln++) {
		window_move_cursor(&ed->lay_split.a, 0, ln - ed->render_from);

		if (ln == ed->current_line)
			attrset(COLOR_PAIR(ed->linenum_color_selected));

		window_draw_format(&ed->lay_split.a, "%*s%0*lu%*s",
		                   ed->linenum_padding, _space,
		                   ed->linenum_width, ln,
		                   ed->linenum_padding, _space);

		if (ln == ed->current_line)
			attrset(COLOR_PAIR(ed->linenum_color_normal));
	}

	attrset(A_NORMAL);
}

void editor_create(editor_t* ed, const window_t* root) {
	layout_create(&ed->lay_split, root, VSPLIT_ABS, 1);

	ed->lines = ed->render_from = ed->render_to = ed->current_line = 1;
	editor_update(ed);
}

void editor_render(editor_t* ed) {
	editor_render_linenums(ed);

	window_move_cursor(&ed->lay_split.b, 0, 0);
	window_draw_string(&ed->lay_split.b, "Hello World");
	// window_draw_format(&ed->lay_split.b, "%*s%s",
	//                    ed->text_margin, _space,
	//                    "Hello World");
}
