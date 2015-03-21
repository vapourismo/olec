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

	KeyMap key_map;

	Application(const std::string& ipc_path) throw (Error);
	~Application();

	int main() throw (Error);

	inline
	void exit(int status = 0) {
		exit_status = status;
		event_base_loopbreak(ev_base);
	}

	void handle_event(const Event& ev);

	void handle_resize(const winsize& ws);
};

}

#endif
