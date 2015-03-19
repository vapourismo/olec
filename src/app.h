#ifndef OLEC_APP_H_
#define OLEC_APP_H_

#include "ipc.h"
#include "keymap.h"

#include <string>
#include <event2/event.h>

namespace olec {

struct Application {
	enum Error {
		EventBaseAllocFailed,
		EventAllocFailed,
		EventDispatchFailed
	};

	event_base* ev_base;
	CommChannel comm;
	int exit_status = 0;

	KeyMap key_map;

	Application(const std::string& ipc_path) throw (Error);
	~Application();

	int main() throw (Error);

	void handle_event(const Event& ev);
};

}

#endif
