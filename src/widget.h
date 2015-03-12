#ifndef OLEC_WIDGET_H
#define OLEC_WIDGET_H

#include <ncurses.h>

/**
 * NCurses Window
 */
typedef WINDOW OlecWindow;

/**
 * Widget
 */
typedef struct {
	OlecWindow* window;
} OlecWidget;

/**
 *
 */
void olec_widget_reuse(OlecWidget* widget, OlecWindow* window);

#endif
