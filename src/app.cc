#include "app.h"

#include <signal.h>
#include <fcntl.h>
#include <unistd.h>

namespace olec {

Application::Application(const std::string& ipc_path) throw (Application::Error):
	ev_base(event_base_new()),
	ipc_fd(open(ipc_path.c_str(), O_RDONLY | O_NONBLOCK))
{
	if (!ev_base)
		throw EventBaseAllocFailed;

	if (ipc_fd < 0)
		throw CommOpenFailed;
}

Application::~Application() {
	if (ev_base)
		event_base_free(ev_base);

	if (ipc_fd >= 0)
		close(ipc_fd);
}

static
void cb_event_dispatch(int data, short what, Application* app) throw (Application::Error) {
	if (what == EV_SIGNAL && data == SIGWINCH) {
		// Handle window resize
		winsize ws;
		if (ioctl(STDIN_FILENO, TIOCGWINSZ, &ws) != 0)
			return;

		app->handle_resize(ws);
	} else if (what == EV_SIGNAL && data == SIGUSR1) {
		// Reload application
		app->exit(1);
	} else if (what == EV_READ && data == app->ipc_fd) {
		// Fetch input event
		Event ev;

		if (!ipc_receive(app->ipc_fd, ev)) {
			app->exit(2);
		} else {
			app->handle_event(ev);
		}
	}
}

int Application::main() throw (Application::Error) {
	// Allocate events
	event* ev_input =
		event_new(ev_base, ipc_fd, EV_PERSIST | EV_READ,
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
	event_add(ev_input, nullptr);
	event_add(ev_resize, nullptr);
	event_add(ev_reload, nullptr);

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
