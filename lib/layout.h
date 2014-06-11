#ifndef OLEC_LAYOUT_H
#define OLEC_LAYOUT_H

#include "session.h"
#include "window.h"
#include "rect.h"

typedef enum {
	VSPLIT_REL,
	VSPLIT_ABS,
	HSPLIT_REL,
	HSPLIT_ABS
} split_mode_t;

typedef struct {
	split_mode_t mode;
	rect_t bounds;
	const window_t* parent;

	union {
		ssize_t absolute;
		double relative;
	} seperator;

	window_t a, b;
} layout_t;

/**
 * Create a new layout container.
 */
void layout_new(layout_t* lay, const rect_t* bounds, split_mode_t mode, ...);

/**
 * Create a new layout container within an existing window.
 */
void layout_sub_win(layout_t* lay, const window_t* source, split_mode_t mode, ...);

/**
 * Free an existing layout container.
 */
void layout_free(layout_t* lay);

/**
 * Resize the underlying windows to match the layout bounds.
 */
void layout_update(layout_t* lay);

/**
 * Update the layout bounds manually.
 */
void layout_set_bounds(layout_t* lay, size_t x, size_t y, size_t w, size_t h);

#endif
