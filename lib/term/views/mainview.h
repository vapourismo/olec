#ifndef OLEC_TERM_VIEWS_MAINVIEW_H
#define OLEC_TERM_VIEWS_MAINVIEW_H

#include "../layout.h"

typedef struct {
	layout_t layout;
} mainview_t;

/**
 *
 */
void mainview_create(mainview_t* mview, const window_t* parent);

/**
 *
 */
void mainview_update(mainview_t* mview);

/**
 *
 */
window_t* mainview_get_statusbar(mainview_t* mview);

/**
 *
 */
window_t* mainview_get_viewport(mainview_t* mview);

#endif
