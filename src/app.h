#ifndef OLEC_APP_H_
#define OLEC_APP_H_

#include "anchor.h"
#include "events.h"

#include <sys/ioctl.h>
#include <termios.h>
#include <event2/event.h>

namespace olec {

struct Application {
	const Anchor& anchor;

	event_base* ev_base;
	event* ev_event;
	event* ev_resize;

	Application(const Anchor& anchor);

	virtual
	~Application();

	void dispatch();

	void quit();

	virtual
	void event(const Event& ev) = 0;

	virtual
	void resize(const winsize& ws) = 0;
};

}

#endif
