#include "events.h"

namespace olec {

bool send_entirely(int fd, const void* data, size_t rem) {
	const uint8_t* pos = (const uint8_t*) data;
	const uint8_t* end = pos + rem;

	while (pos != end) {
		ssize_t r = ::send(fd, pos, rem, 0);

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
		ssize_t r = ::recv(fd, pos, rem, 0);

		if (r < 0)
			return false;

		pos += r;
		rem -= r;
	}

	return true;
}

}
