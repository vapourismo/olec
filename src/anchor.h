#ifndef OLEC_ANCHOR_H_
#define OLEC_ANCHOR_H_

#include "events.h"

#include <string>
#include <unistd.h>

namespace olec {

struct Anchor {
	int pty_fd = -1;
	pid_t pid = -1;

	std::string fifo_path;
	int fifo_fd = -1;

	Anchor();

	~Anchor();

	inline
	operator bool() {
		return pid > 0;
	}

	bool send(const Event& ev) const;

	bool receive(Event& ev) const;
};

}

#endif
