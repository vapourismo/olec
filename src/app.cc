#include "app.h"

#include <signal.h>
#include <termios.h>
#include <sys/ioctl.h>

namespace olec {

Application::Application(const std::string& ipc_path) throw (Application::Error):
	ev_base(event_base_new()),
	comm(ipc_path)
{
	if (!ev_base)
		throw EventBaseAllocFailed;
}

Application::~Application() {
	if (ev_base)
		event_base_free(ev_base);
}

static
void cb_event_dispatch(int data, short what, Application* app) throw (Application::Error) {
	if (what == EV_SIGNAL && data == SIGWINCH) {
		// Handle window resize
		winsize ws;
		if (ioctl(STDIN_FILENO, TIOCGWINSZ, &ws) != 0)
			return;

		// TODO: Handle new size
	} else if (what == EV_SIGNAL && data == SIGUSR1) {
		// Reload application
		app->exit_status = 1;
		event_base_loopbreak(app->ev_base);
	} else if (what == EV_READ && data == app->comm.fd) {
		// Fetch input event
		Event ev;
		if (!app->comm.receive(ev))
			throw Application::EventDispatchFailed;

		// TODO: Handle event
	}
}

int Application::main() throw (Application::Error) {
	// Allocate events
	event* ev_input =
		event_new(ev_base, comm.fd, EV_PERSIST | EV_READ,
		          (event_callback_fn) cb_event_dispatch, this);
	event* ev_resize =
		event_new(ev_base, SIGWINCH, EV_PERSIST | EV_SIGNAL,
		          (event_callback_fn) cb_event_dispatch, this);
	event* ev_reload =
		event_new(ev_base, SIGUSR1, EV_PERSIST | EV_SIGNAL,
		          (event_callback_fn) cb_event_dispatch, this);

	if (!ev_input || !ev_resize || !ev_reload)
		throw EventAllocFailed;

	// Add events
	event_add(ev_input, NULL);
	event_add(ev_resize, NULL);
	event_add(ev_reload, NULL);

	// Dispatch events
	event_base_dispatch(ev_base);

	// Remove events
	event_del(ev_input);
	event_del(ev_resize);
	event_del(ev_reload);

	// Free events
	event_free(ev_input);
	event_free(ev_resize);
	event_free(ev_reload);

	return exit_status;
}

}
