#ifndef OLEC_APP_H_
#define OLEC_APP_H_

#include "ipc.h"
#include "keymap.h"

#include <string>
#include <sys/ioctl.h>
#include <termios.h>
#include <event2/event.h>

namespace olec {

struct Application {
	enum Error {
		EventBaseAllocFailed,
		EventAllocFailed,
		EventDispatchFailed,
		CommOpenFailed
	};

	event_base* ev_base;
	int ipc_fd;
	int exit_status = 0;

	Application(const std::string& ipc_path) throw (Error);

	virtual
	~Application();

	int main() throw (Error);

	inline
	void exit(int status = 0) {
		exit_status = status;
		event_base_loopbreak(ev_base);
	}

	virtual
	void handle_event(const Event& ev) = 0;

	virtual
	void handle_resize(const winsize& ws) = 0;
};

}

#endif
