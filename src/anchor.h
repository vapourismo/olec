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

	FILE* log_fd = nullptr;

	/**
	 * Fork process and construct anchor
	 */
	Anchor();

	~Anchor();

	/**
	 * Is parent anchor?
	 */
	inline
	operator bool() {
		return pid > 0;
	}

	/**
	 * Send event through communication channel.
	 */
	bool send(const Event& ev) const;

	/**
	 * Get event from communication channel.
	 */
	bool receive(Event& ev) const;

	/**
	 * Log Level
	 */
	enum LogLevel {
		Debug,
		Info,
		Warn,
		Error
	};

	/**
	 *
	 */
	void log(LogLevel lvl, const char* format, ...) const;
};

}

#endif
