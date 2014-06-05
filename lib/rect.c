#include "rect.h"
#include "aux.h"

void rect_vsplit_abs(const rect_t* base,
                     rect_t* left, rect_t* right,
                     ssize_t sep) {
	if (sep < 0)
		sep = base->w + sep;


	left->x = base->x;
	left->w = min(sep, base->w);

	right->x = left->x + left->w;
	right->w = base->w - left->w;

	left->y = right->y = base->y;
	left->h = right->h = base->h;
}

void rect_vsplit_rel(const rect_t* base,
                     rect_t* left, rect_t* right,
                     double sep) {
	rect_vsplit_abs(base, left, right, base->w * sep);
}

void rect_hsplit_abs(const rect_t* base,
                     rect_t* top, rect_t* bottom,
                     ssize_t sep) {
	if (sep < 0)
		sep = base->h + sep;

	top->y = base->y;
	top->h = min(sep, base->h);


	bottom->y = top->y + top->h;
	bottom->h = base->h - top->h;

	top->x = bottom->x = base->x;
	top->w = bottom->w = base->w;
}

void rect_hsplit_rel(const rect_t* base,
                     rect_t* top, rect_t* bottom,
                     double sep) {
	rect_hsplit_abs(base, top, bottom, base->h * sep);
}