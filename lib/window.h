#ifndef OLEC_WINDOW_H
#define OLEC_WINDOW_H

#include "session.h"
#include "rect.h"

typedef struct _window {
	struct _window* parent;
	curswin* ref;
} window_t;

/**
 * Create a new window.
 */
void window_create(window_t* win,
                   size_t x, size_t y, size_t w, size_t h);

/**
 * Delete a window.
 */
void window_delete(const window_t* win);

/**
 * Extract the rectangle from a window.
 */
void window_get_bounds(const window_t* win, rect_t* bounds);

/**
 * Set the bounds.
 */
void window_set_bounds(window_t* win, const rect_t* bounds);

#endif
