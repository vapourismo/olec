#include "anchor.h"

#include <cassert>

#include <pty.h>
#include <fcntl.h>
#include <sys/stat.h>

using namespace std;

namespace olec {

static inline
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

static inline
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

Anchor::Anchor():
	fifo_path("/tmp/olec-" + to_string(getpid()))
{
	assert(mkfifo(fifo_path.c_str(), S_IWUSR | S_IRUSR) == 0);

	pid = forkpty(&pty_fd, nullptr, nullptr, nullptr);
	assert(pid >= 0);

	if (pid == 0) {
		// Child process
		fifo_fd = open(fifo_path.c_str(), O_RDONLY | O_NONBLOCK);
		assert(fifo_fd > 0);
	} else if (pid > 0) {
		// Parent process
		fifo_fd = open(fifo_path.c_str(), O_WRONLY);
		assert(fifo_fd > 0);
	}
}

Anchor::~Anchor() {
	close(fifo_fd);
	if (pid > 0) unlink(fifo_path.c_str());
}

bool Anchor::send(const Event& ev) const {
	return send_entirely(fifo_fd, &ev, sizeof(Event));
}

bool Anchor::receive(Event& ev) const {
	return receive_entirely(fifo_fd, &ev, sizeof(Event));
}

}
