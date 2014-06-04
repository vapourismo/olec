#include "layout.h"
#include "aux.h"

#include <stdarg.h>

layout_t* layout_new(const rect_t* bounds, split_mode_t mode, ...) {
	layout_t* lay = new(layout_t);

	if (lay) {
		lay->bounds = *bounds;
		lay->mode = mode;
		lay->a.ref = lay->b.ref = NULL;

		va_list vargs;
		va_start(vargs, mode);

		if (mode == HSPLIT_ABS || mode == VSPLIT_ABS)
			lay->seperator.absolute = va_arg(vargs, ssize_t);
		else
			lay->seperator.relative = va_arg(vargs, double);

		va_end(vargs);

		layout_update(lay);
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

	wclear(lay->a.ref);
	wclear(lay->b.ref);

	window_set_bounds(&lay->a, &ra);
	window_set_bounds(&lay->b, &rb);
}
