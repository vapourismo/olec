#include "widget.h"

void olec_widget_reuse(OlecWidget* widget, OlecWindow* window) {
	widget->window = window;
}
