#ifndef OLEC_MAINFRAME_H
#define OLEC_MAINFRAME_H

#include "curses.h"
#include "editorview.h"
#include "event.h"

/**
 *
 */
typedef struct {
	OlecCursesFrame* main_window;
	OlecCursesFrame* status_window;

	OlecEditorView editor_view;
} OlecMainFrame;

/**
 *
 */
void olec_main_frame_init(OlecMainFrame* frame);

/**
 *
 */
void olec_main_frame_update(OlecMainFrame* frame);

/**
 *
 */
void olec_main_frame_render(const OlecMainFrame* frame);

/**
 *
 */
bool olec_main_frame_handle_event(OlecMainFrame* frame, const OlecEvent* event);

#endif
