#ifndef OLEC_MAINFRAME_H
#define OLEC_MAINFRAME_H

#include "curses.h"

/**
 *
 */
typedef struct {
	OlecCursesFrame* main;
	OlecCursesFrame* status;
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

#endif
