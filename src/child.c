#include "child.h"
#include "event.h"

#include <stdlib.h>
#include <fcntl.h>

int olec_child_launch(int argc, char** argv) {
	// Check for environment variable
	char* ipc_path = getenv("OLEC_IPC");
	if (!ipc_path)
		return 1;

	// Open FIFO
	int fd = open(ipc_path, O_RDONLY);
	if (fd < 0)
		return 1;

	// Main loop
	OlecEvent event;
	while (olec_event_read(fd, &event)) {
		printf("%i + %i\n", event.info.key_press.mod, event.info.key_press.key);

		if (event.info.key_press.mod == GDK_CONTROL_MASK && event.info.key_press.key == 'q')
			break;
	}

	close(fd);
	return 0;
}
