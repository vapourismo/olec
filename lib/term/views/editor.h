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
	size_t linenum_width;
	size_t lines;
	size_t current_line;
	size_t render_from, render_to;
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
