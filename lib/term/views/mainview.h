#ifndef OLEC_TERM_VIEWS_MAINVIEW_H
#define OLEC_TERM_VIEWS_MAINVIEW_H

#include "../layout.h"

typedef struct {
	window_t root;
	layout_t layout;
	window_t* statusbar;
	window_t* viewport;
} mainview_t;

/**
 *
 */
void mainview_create(mainview_t* mview);

/**
 *
 */
void mainview_update(mainview_t* mview);

#endif
