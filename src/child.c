#include "child.h"
#include "olec.h"

#include <stdlib.h>
#include <signal.h>

static void handle_signal(int sig) {
	exit(OLEC_CHILD_EXIT_RELOAD);
}

int olec_child_launch(int argc, char** argv) {
	// Setup environment
	char* event_source = getenv("OLEC_IPC");
	Olec olec;

	if (!event_source || !olec_init(&olec, event_source))
		return OLEC_CHILD_EXIT_ERROR;

	signal(SIGUSR1, handle_signal);

	return olec_main(&olec);
}
