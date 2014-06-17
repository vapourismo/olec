#ifndef OLEC_TERM_VIEWS_EDITOR_H
#define OLEC_TERM_VIEWS_EDITOR_H

#include "../window.h"
#include "../layout.h"

/**
 *
 */
typedef struct {
	/* configurable */
	short linenum_color_normal;
	short linenum_color_selected;
	size_t linenum_padding;
	size_t linenum_min_width;

	size_t text_margin;

	/* wm internals */
	layout_t lay_split;

	/* viewport internals */
	size_t viewport_x, viewport_y;
	size_t viewport_width, viewport_height;
	size_t cursor_x, cursor_y;

	size_t num_lines;
	size_t linenum_width;
} editor_t;

/**
 *
 */
void editor_create(editor_t* ed, const window_t* root);

/**
 *
 */
void editor_render(editor_t* ed);

/**
 *
 */
void editor_update(editor_t* ed);

#endif
