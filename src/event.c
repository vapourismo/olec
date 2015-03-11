#include "event.h"

#include <unistd.h>

bool olec_event_write(int fd, const OlecEvent* event) {
	uint8_t* pos = (uint8_t*) event;
	uint8_t* end = pos + sizeof(OlecEvent);
	size_t rem = sizeof(OlecEvent);

	while (pos != end) {
		ssize_t r = write(fd, pos, rem);

		if (r < 0)
			return false;

		pos += r;
		rem -= r;
	}

	return true;
}

bool olec_event_read(int fd, OlecEvent* event) {
	uint8_t* pos = (uint8_t*) event;
	uint8_t* end = pos + sizeof(OlecEvent);
	size_t rem = sizeof(OlecEvent);

	while (pos != end) {
		ssize_t r = read(fd, pos, rem);

		if (r < 0)
			return false;

		pos += r;
		rem -= r;
	}

	return true;
}
