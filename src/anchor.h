#ifndef OLEC_ANCHOR_H_
#define OLEC_ANCHOR_H_

#include "events.h"

#include <string>
#include <unistd.h>

namespace olec {

struct Anchor {
	static
	const Anchor* self;

	int pty_fd = -1;
	pid_t pid = -1;

	std::string fifo_path;
	int fifo_fd = -1;

	FILE* log_fd = nullptr;

	std::string base_path = ".";

	/**
	 * Fork process and construct anchor
	 */
	Anchor(const char* progname);

	Anchor(const Anchor&) = delete;
	Anchor(Anchor&&) = delete;

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
	 * Log a message
	 */
	void log(LogLevel lvl, const char* format, ...) const;
};

#ifdef DEBUG
	#define logdebug(...) olec::Anchor::self->log(olec::Anchor::Debug, __VA_ARGS__)
#endif

#define loginfo(...)  olec::Anchor::self->log(olec::Anchor::Info,  __VA_ARGS__)
#define logwarn(...)  olec::Anchor::self->log(olec::Anchor::Warn,  __VA_ARGS__)
#define logerror(...) olec::Anchor::self->log(olec::Anchor::Error, __VA_ARGS__)

}

#endif
