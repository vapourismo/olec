#ifndef OLEC_LAYOUT_H
#define OLEC_LAYOUT_H

#include "session.h"
#include "window.h"

typedef enum {
	VSPLIT_REL,
	VSPLIT_ABS,
	HSPLIT_REL,
	HSPLIT_ABS
} split_mode_t;

typedef struct {
	split_mode_t mode;
	const window_t* parent;
	window_t a, b;

	union {
		ssize_t absolute;
		double relative;
	} seperator;
} layout_t;

/**
 * Create a new layout container within an existing window.
 */
void layout_create(layout_t* lay, const window_t* win, split_mode_t mode, ...);

/**
 * Resize the underlying windows to match the layout bounds.
 */
void layout_update(layout_t* lay);

#endif
