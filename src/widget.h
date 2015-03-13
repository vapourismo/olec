#ifndef OLEC_WIDGET_H
#define OLEC_WIDGET_H

#include <ncurses.h>

/**
 * NCurses Window
 */
typedef WINDOW OlecWindow;

/**
 * Forward declaration
 */
struct _OlecWidget;

/**
 *
 */
typedef void (* OlecUpdateWidgetFun)(struct _OlecWidget*, const struct _OlecWidget*);

/**
 *
 */
typedef void (* OlecRenderWidgetFun)(struct _OlecWidget*);

/**
 *
 */
typedef struct _OlecWidget {
	OlecUpdateWidgetFun update;
	OlecRenderWidgetFun render;
	OlecWindow* window;
} OlecWidget;

/**
 *
 */
void olec_widget_update(OlecWidget* widget, const OlecWidget* parent);

/**
 *
 */
void olec_widget_render(OlecWidget* widget);

#endif
