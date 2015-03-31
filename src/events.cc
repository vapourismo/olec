#include "events.h"

#include <stdint.h>

namespace olec {

bool send_entirely(int fd, const void* data, size_t rem) {
	const uint8_t* pos = (const uint8_t*) data;
	const uint8_t* end = pos + rem;

	while (pos != end) {
		ssize_t r = ::write(fd, pos, rem);

		if (r < 0)
			return false;

		pos += r;
		rem -= r;
	}

	return true;
}

bool receive_entirely(int fd, void* data, size_t rem) {
	uint8_t* pos = (uint8_t*) data;
	uint8_t* end = pos + rem;

	while (pos != end) {
		ssize_t r = ::read(fd, pos, rem);

		if (r < 0)
			return false;

		pos += r;
		rem -= r;
	}

	return true;
}

}
