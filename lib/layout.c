#include "layout.h"
#include "aux.h"

#include <stdarg.h>

void layout_new_vargs(layout_t* lay, const rect_t* bounds, split_mode_t mode, va_list vargs) {
	if (bounds) lay->bounds = *bounds;
	lay->mode = mode;
	lay->a = lay->b = NULL;

	/* determine which kind of argument needs to be extracted from
	   the variable arguments.
	   also assign it to the right field. */
	if (mode == HSPLIT_ABS || mode == VSPLIT_ABS)
		lay->seperator.absolute = va_arg(vargs, ssize_t);
	else
		lay->seperator.relative = va_arg(vargs, double);

	/* update/create the underlying windows */
	layout_update(lay);
}

void layout_new(layout_t* lay, const rect_t* bounds, split_mode_t mode, ...) {
	va_list vargs;
	va_start(vargs, mode);

	/* there is no parent container */
	lay->parent = NULL;

	/* let 'layout_new_vargs' initialize the structure */
	layout_new_vargs(lay, bounds, mode, vargs);

	va_end(vargs);
}

void layout_sub_win(layout_t* lay, const window_t* target, split_mode_t mode, ...) {
	va_list vargs;
	va_start(vargs, mode);

	/* reference the parent container */
	lay->parent = target;

	/* let 'layout_new_vargs' initialize the structure */
	layout_new_vargs(lay, NULL, mode, vargs);

	va_end(vargs);
}


void layout_free(layout_t* lay) {
	if (lay) {
		window_delete(&lay->a);
		window_delete(&lay->b);
	}
}

void layout_update(layout_t* lay) {
	if (lay->parent) {
		/* update the window bounds if this layout is part of another window */
		window_get_bounds(lay->parent, &lay->bounds);
	}

	rect_t ra, rb;

	/* calculate both window bounds based on the splitting mode */
	switch (lay->mode) {
		case VSPLIT_ABS:
			rect_vsplit_abs(&lay->bounds, &ra, &rb, lay->seperator.absolute);
			break;

		case HSPLIT_ABS:
			rect_hsplit_abs(&lay->bounds, &ra, &rb, lay->seperator.absolute);
			break;

		case VSPLIT_REL:
			rect_vsplit_rel(&lay->bounds, &ra, &rb, lay->seperator.relative);
			break;

		case HSPLIT_REL:
			rect_hsplit_rel(&lay->bounds, &ra, &rb, lay->seperator.relative);
			break;
	}

	/* update both window bounds */
	window_set_bounds(&lay->a, &ra);
	window_set_bounds(&lay->b, &rb);
}

void layout_set_bounds(layout_t* lay, size_t x, size_t y, size_t w, size_t h) {
	lay->bounds.x = x;
	lay->bounds.y = y;
	lay->bounds.w = w;
	lay->bounds.h = h;
}
