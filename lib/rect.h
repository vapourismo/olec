#ifndef OLEC_LAYOUT_H
#define OLEC_LAYOUT_H

#include "session.h"

#include <stddef.h>
#include <unistd.h>

/**
 * Rectangle
 */
typedef struct {
	size_t x, y, w, h;
} rect_t;

/**
 * Split vertically (absolute splitter position).
 */
void rect_vsplit_abs(const rect_t* base,
                     rect_t* left, rect_t* right,
                     ssize_t sep);

/**
 * Split vertically (relative splitter position).
 */
void rect_vsplit_rel(const rect_t* base,
                     rect_t* left, rect_t* right,
                     float sep);

/**
 * Split horizontally (absolute splitter position);
 */
void rect_hsplit_abs(const rect_t* base,
                     rect_t* top, rect_t* bottom,
                     ssize_t sep);

/**
 * Split horizontally (relative splitter position).
 */
void rect_hsplit_rel(const rect_t* base,
                     rect_t* top, rect_t* bottom,
                     float sep);

#endif
