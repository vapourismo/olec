#ifndef OLEC_APP_H_
#define OLEC_APP_H_

#include "events.h"
#include "keymap.h"

#include <sys/ioctl.h>
#include <termios.h>
#include <event2/event.h>

namespace olec {

struct Application {
	int exit_status = 0;

	event* ev_resize;

	Application();

	virtual
	~Application();

	void main();

	void exit(int status = 0);

	virtual
	void resize(const winsize& ws) = 0;
};

}

#endif
