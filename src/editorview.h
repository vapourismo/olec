#ifndef OLEC_EDITORVIEW_H
#define OLEC_EDITORVIEW_H

#include "curses.h"

#include <stddef.h>

/**
 *
 */
typedef struct {
	size_t num_lines;
	size_t scroll_line, scroll_col;
	size_t active_line;

	OlecCursesFrame* frame;
} OlecEditorView;

/**
 *
 */
void olec_editor_view_init(OlecEditorView* edview);

/**
 *
 */
void olec_editor_view_update(OlecEditorView* edview, OlecCursesFrame* frame);

/**
 *
 */
void olec_editor_view_render_lines(const OlecEditorView* edview);

/**
 *
 */
void olec_editor_view_render(const OlecEditorView* edview);

#endif
