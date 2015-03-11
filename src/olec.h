#ifndef OLEC_OLEC_H
#define OLEC_OLEC_H

#include "event.h"
#include "child.h"

#include <fcntl.h>
#include <ncurses.h>
#include <event2/event.h>

/**
 * NCurses Window
 */
typedef WINDOW OlecWindow;

/**
 * Olec Application
 */
typedef struct {
	int event_fd;
	OlecWindow* stage;
	int exit_status;

	struct event_base* event_base;
} Olec;

/**
 * Initialize the application
 */
bool olec_init(Olec* olec, const char* ipc_path);

/**
 * Application main loop
 */
int olec_main(Olec* olec);

#endif
