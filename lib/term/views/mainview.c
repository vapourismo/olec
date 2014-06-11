#include "mainview.h"

void mainview_init(mainview_t* mview, const window_t* parent) {
	layout_sub_win(&mview->layout, parent, HSPLIT_ABS, (ssize_t) -1);
}

void mainview_update(mainview_t* mview) {
	layout_update(&mview->layout);
}

window_t* mainview_get_statusbar(mainview_t* mview) {
	return &mview->layout.b;
}

window_t* mainview_get_viewport(mainview_t* mview) {
	return &mview->layout.a;
}
