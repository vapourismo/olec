#ifndef OLEC_WINDOW_H
#define OLEC_WINDOW_H

#include <stddef.h>
#include <unistd.h>

typedef struct {
	size_t x, y, w, h;
} window_t;

/**
 * Create a new window.
 */
void window_create(window_t* win, size_t x, size_t y, size_t w, size_t h);

/**
 * Fill a window with the given character.
 */
void window_fill(window_t* win, char c);

/**
 * Clear the window contents.
 */
void window_clear(window_t* win);

/**
 * Position the cursor within the window.
 */
void window_move_cursor(window_t* win, size_t x, size_t y);

/**
 * Draw a string.
 */
void window_draw_string(window_t* win, const char* message);

/**
 * Draw a string using a format.
 */
void window_draw_format(window_t* win, const char* format, ...);

/**
 * Checks if the given coordinates are within 'win'.
 */
int window_encloses(window_t* win, size_t x, size_t y);

/**
 * Flush the changes in win to the screen.
 */
void window_render(const window_t* win);

/**
 * Split a window vertically (absolute)
 */
void window_vsplit_abs(const window_t* base,
                       window_t* left, window_t* right,
                       ssize_t sep);

/**
 * Split a window vertically (relative)
 */
void window_vsplit_rel(const window_t* base,
                       window_t* left, window_t* right,
                       double sep);

/**
 * Split a window horizontally (absolute)
 */
void window_hsplit_abs(const window_t* base,
                       window_t* top, window_t* bottom,
                       ssize_t sep);

/**
 * Split a window vertically (relative)
 */
void window_hsplit_rel(const window_t* base,
                       window_t* top, window_t* bottom,
                       double sep);

#endif
