#include "widget.h"

void olec_widget_update(OlecWidget* widget, const OlecWidget* parent) {
	widget->update(widget, parent);
}

void olec_widget_render(OlecWidget* widget) {
	widget->render(widget);
}
