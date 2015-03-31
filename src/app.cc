#include "app.h"

#include <signal.h>
#include <fcntl.h>
#include <unistd.h>

namespace olec {

static
void cb_event_dispatch(int data, short what, Application* app) {
	if (what == EV_SIGNAL && data == SIGWINCH) {
		// Handle window resize
		winsize ws;
		if (ioctl(STDIN_FILENO, TIOCGWINSZ, &ws) != 0)
			return;

		app->resize(ws);
	} else if (what == EV_SIGNAL && data == SIGUSR1) {
		// Reload application
		app->exit(1);
	}
}

Application::Application() {
	// Allocate events
	// ev_resize =
	// 	event_new(ev_base, SIGWINCH, EV_PERSIST | EV_SIGNAL,
	// 	          (event_callback_fn) cb_event_dispatch, this);
}

Application::~Application() {
	// Free events
	// event_free(ev_resize);
}

void Application::exit(int status) {
	exit_status = status;

	// Remove events
	// event_del(ev_resize);
}

void Application::main() {
	// Add events
	// event_add(ev_resize, nullptr);

	// Dispatch events
	// dispatch();
}

}
