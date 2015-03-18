#ifndef OLEC_EDITORVIEW_H
#define OLEC_EDITORVIEW_H

#include "curses.h"
#include "editor.h"
#include "event.h"
#include "keymap.h"

#include <stddef.h>

/**
 * Editor view
 */
typedef struct {
	OlecEditor editor;
	size_t scroll_line, scroll_col;

	OlecCursesFrame* frame;

	OlecKeyMap keymap;
} OlecEditorView;

/**
 * Initialize the editor view.
 */
void olec_editor_view_init(OlecEditorView* edview);

/**
 * Update the editor view.
 */
void olec_editor_view_update(OlecEditorView* edview, OlecCursesFrame* frame);

/**
 * Render the editor view.
 */
void olec_editor_view_render(const OlecEditorView* edview);

/**
 * Delegate an event to the editor view.
 */
bool olec_editor_view_handle_event(OlecEditorView* edview, const OlecEvent* event);

#endif
