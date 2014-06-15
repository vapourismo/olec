#include "mainview.h"

void mainview_create(mainview_t* mview, const window_t* parent) {
	layout_create(&mview->layout, parent, HSPLIT_ABS, (ssize_t) -1);
	mview->viewport = &mview->layout.a;
	mview->statusbar = &mview->layout.b;
}

void mainview_update(mainview_t* mview) {
	layout_update(&mview->layout);
}
