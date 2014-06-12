#include "layout.h"
#include "../aux.h"
#include <stdarg.h>

void layout_create(layout_t* lay, const window_t* win, split_mode_t mode, ...) {
	lay->mode = mode;
	lay->parent = win;

	va_list vargs;
	va_start(vargs, mode);

	/* determine which kind of argument needs to be extracted from
	   the variable arguments.
	   also assign it to the right field. */
	if (mode == HSPLIT_ABS || mode == VSPLIT_ABS)
		lay->seperator.absolute = va_arg(vargs, ssize_t);
	else
		lay->seperator.relative = va_arg(vargs, double);

	va_end(vargs);

	/* update/create the underlying windows */
	layout_update(lay);
}

void layout_update(layout_t* lay) {
	/* calculate both window bounds based on the splitting mode */
	switch (lay->mode) {
		case VSPLIT_ABS:
			window_vsplit_abs(lay->parent, &lay->a, &lay->b, lay->seperator.absolute);
			break;

		case HSPLIT_ABS:
			window_hsplit_abs(lay->parent, &lay->a, &lay->b, lay->seperator.absolute);
			break;

		case VSPLIT_REL:
			window_vsplit_rel(lay->parent, &lay->a, &lay->b, lay->seperator.relative);
			break;

		case HSPLIT_REL:
			window_hsplit_rel(lay->parent, &lay->a, &lay->b, lay->seperator.relative);
			break;
	}
}
