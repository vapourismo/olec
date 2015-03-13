#ifndef OLEC_OLEC_H
#define OLEC_OLEC_H

#include "event.h"
#include "child.h"
#include "keymap.h"
#include "mainframe.h"

#include <fcntl.h>
#include <event2/event.h>

/**
 * Olec Application
 */
typedef struct {
	int event_fd;
	struct event_base* event_base;
	int exit_status;

	OlecKeyMap global_keymap;
	OlecMainFrame main_frame;
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
