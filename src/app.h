#ifndef OLEC_APP_H_
#define OLEC_APP_H_

#include "events.h"
#include "keymap.h"

#include <string>
#include <sys/ioctl.h>
#include <termios.h>
#include <event2/event.h>

namespace olec {

struct Application: EventNode<Event> {
	int exit_status = 0;

	event* ev_resize;
	event* ev_reload;

	Application(const std::string& ipc_path);

	virtual
	~Application();

	void main();

	void exit(int status = 0);

	virtual
	void resize(const winsize& ws) = 0;
};

}

#endif
