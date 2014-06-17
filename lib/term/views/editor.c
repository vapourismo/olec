#include "editor.h"
#include "../../aux.h"
#include "../../log.h"
#include <ncurses.h>
#include <math.h>

static const char* _space = "";

void editor_render_linenums(editor_t* ed) {
	/* the viewport y-offset may not exceed the number of lines */
	ed->viewport_y = min(ed->num_lines, ed->viewport_y);

	/* set the style */
	attrset(COLOR_PAIR(ed->linenum_color_normal));

	/* draw each line number */
	for (size_t view_y = 0;
	     view_y < ed->lay_split.b.h && view_y + ed->viewport_y < ed->num_lines;
	     view_y++) {

		/* position the cursor */
		window_move_cursor(&ed->lay_split.a, 0, view_y);

		/* if this is a selected line, we need to apply the corresponding style */
		if (view_y + ed->viewport_y == ed->cursor_y)
			attrset(COLOR_PAIR(ed->linenum_color_selected));

		/* draw the line number including padding */
		window_draw_format(&ed->lay_split.a, "%*s%0*lu%*s",
		                   ed->linenum_padding, _space,
		                   ed->linenum_width, view_y + ed->viewport_y + 1,
		                   ed->linenum_padding, _space);

		/* restore the normal line number style if necessary */
		if (view_y + ed->viewport_y  == ed->cursor_y)
			attrset(COLOR_PAIR(ed->linenum_color_normal));
	}

	/* reset the style */
	attrset(0);
}

void editor_create(editor_t* ed, const window_t* root) {
	layout_create(&ed->lay_split, root, VSPLIT_ABS, 1);

	/* set default configuration values */
	ed->linenum_padding = 1;
	ed->linenum_min_width = 2;
	ed->linenum_color_normal = 2;
	ed->linenum_color_selected = 3;
	ed->text_margin = 1;

	/* set viewport/cursor properties */
	ed->viewport_x = ed->viewport_y = 0;
	ed->cursor_x = ed->cursor_y = 0;

	ed->num_lines = 0;

	editor_update(ed);
}

void editor_render(editor_t* ed) {
	editor_render_linenums(ed);

	window_move_cursor(&ed->lay_split.b, ed->text_margin, 0);
	window_draw_format(&ed->lay_split.b, "%lux%lu+%lu+%lu",
	                   ed->viewport_width, ed->viewport_height,
	                   ed->viewport_x, ed->viewport_y);
}

void editor_update(editor_t* ed) {
	/* calulate the max line number width (e.g. 100 => 3, 10 => 2, 1 => 1) */
	ed->linenum_width = max(ed->linenum_min_width, ceil(log10(ed->num_lines + 1)));

	/* set new seperator and preserve old value */
	size_t old_split = ed->lay_split.seperator.absolute;
	ed->lay_split.seperator.absolute = ed->linenum_width + 2 * ed->linenum_padding;

	/* update only if the bounds have changed */
	if (old_split != ed->lay_split.seperator.absolute)
		layout_update(&ed->lay_split);

	ed->viewport_height = ed->lay_split.b.h;
	ed->viewport_width = ed->lay_split.b.w - ed->text_margin - 1;
}
