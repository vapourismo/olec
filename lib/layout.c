#include "layout.h"
#include "aux.h"

#include <stdarg.h>

void layout_new_vargs(layout_t* lay, const rect_t* bounds, split_mode_t mode, va_list vargs) {
	lay->bounds = *bounds;
	lay->mode = mode;
	lay->parent = NULL;
	lay->a.ref = lay->b.ref = NULL;

	if (mode == HSPLIT_ABS || mode == VSPLIT_ABS)
		lay->seperator.absolute = va_arg(vargs, ssize_t);
	else
		lay->seperator.relative = va_arg(vargs, double);

	layout_update(lay);
}

layout_t* layout_new(const rect_t* bounds, split_mode_t mode, ...) {
	layout_t* lay = new(layout_t);

	if (lay) {
		va_list vargs;
		va_start(vargs, mode);

		layout_new_vargs(lay, bounds, mode, vargs);

		va_end(vargs);
	}

	return lay;
}

layout_t* layout_sub_win(const window_t* target, split_mode_t mode, ...) {
	layout_t* lay = new(layout_t);

	if (lay) {
		va_list vargs;
		va_start(vargs, mode);

		rect_t tmp;
		window_get_bounds(target, &tmp);

		layout_new_vargs(lay, &tmp, mode, vargs);
		lay->parent = target;

		va_end(vargs);
	}

	return lay;
}


void layout_free(layout_t* lay) {
	if (lay) {
		window_delete(&lay->a);
		window_delete(&lay->b);

		free(lay);
	}
}

void layout_update(layout_t* lay) {
	if (lay->parent) {
		window_get_bounds(lay->parent, &lay->bounds);
	}

	rect_t ra, rb;

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

	window_set_bounds(&lay->a, &ra);
	window_set_bounds(&lay->b, &rb);
}

void layout_set_bounds(layout_t* lay, size_t x, size_t y, size_t w, size_t h) {
	lay->bounds.x = x;
	lay->bounds.y = y;
	lay->bounds.w = w;
	lay->bounds.h = h;
}
