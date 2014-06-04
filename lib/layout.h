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

	union {
		ssize_t absolute;
		double relative;
	} seperator;

	window_t a, b;
} layout_t;

/**
 *
 */
layout_t* layout_new(const rect_t* bounds, split_mode_t mode, ...);

/**
 *
 */
void layout_free(layout_t* lay);

/**
 *
 */
void layout_update(layout_t* lay);

#endif
