#include "child.h"
#include "olec.h"

#include <stdlib.h>

int olec_child_launch(int argc, char** argv) {
	// Setup environment
	char* event_source = getenv("OLEC_IPC");
	Olec olec;

	if (!event_source || !olec_init(&olec, event_source))
		return OLEC_CHILD_EXIT_ERROR;

	return olec_main(&olec);
}
