#include "mainview.h"

void mainview_create(mainview_t* mview) {
	window_maximize(&mview->root);
	layout_create(&mview->layout, &mview->root, HSPLIT_ABS, (ssize_t) -1);

	mview->viewport = &mview->layout.a;
	mview->statusbar = &mview->layout.b;
}

void mainview_update(mainview_t* mview) {
	window_maximize(&mview->root);
	layout_update(&mview->layout);
}
