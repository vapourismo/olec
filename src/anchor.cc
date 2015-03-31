#include "anchor.h"

#include <iostream>
#include <cassert>

#include <pty.h>
#include <fcntl.h>
#include <sys/stat.h>

using namespace std;

namespace olec {

Anchor::Anchor():
	fifo_path("/tmp/olec-" + to_string(getpid()))
{
	assert(mkfifo(fifo_path.c_str(), S_IWUSR | S_IRUSR) == 0);

	pid = forkpty(&pty_fd, nullptr, nullptr, nullptr);
	assert(pid >= 0);

	if (pid == 0) {
		// Child process
		fifo_fd = open(fifo_path.c_str(), O_RDONLY);
	} else if (pid > 0) {
		// Parent process
		fifo_fd = open(fifo_path.c_str(), O_WRONLY);
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
