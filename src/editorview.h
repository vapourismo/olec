#ifndef OLEC_EDITORVIEW_H
#define OLEC_EDITORVIEW_H

#include "curses.h"
#include "editor.h"
#include "event.h"
#include "keymap.h"

#include <stddef.h>

/**
 *
 */
typedef struct {
	OlecEditor editor;
	size_t scroll_line, scroll_col;

	OlecCursesFrame* frame;

	OlecKeyMap keymap;
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
void olec_editor_view_render(const OlecEditorView* edview);

/**
 *
 */
bool olec_editor_view_handle_event(OlecEditorView* edview, const OlecEvent* event);

#endif
