#include "app.h"

#include <cassert>

#include <signal.h>
#include <fcntl.h>
#include <unistd.h>

using namespace std;

namespace olec {

static
void cb_resize(int sig, short what, Application* app) {
	// Handle window resize
	winsize ws;
	if (ioctl(STDIN_FILENO, TIOCGWINSZ, &ws) != 0)
		return;

	app->resize(ws);
}

static
void cb_event(int fd, short what, Application* app) {
	Event ev;

	if (app->anchor.receive(ev)) {
		app->event(ev);
	}
}

Application::Application(const Anchor& anchor):
	anchor(anchor)
{
	ev_base = event_base_new();
	assert(ev_base != nullptr);

	ev_event = event_new(ev_base, anchor.fifo_fd, EV_PERSIST | EV_READ,
	                     (event_callback_fn) cb_event, this);
	assert(ev_event != nullptr);

	ev_resize = event_new(ev_base, SIGWINCH, EV_PERSIST | EV_SIGNAL,
	                      (event_callback_fn) cb_resize, this);
	assert(ev_resize != nullptr);
}

void Application::dispatch() {
	// Add events
	event_add(ev_event, nullptr);
	event_add(ev_resize, nullptr);

	// Dispatch events
	event_base_dispatch(ev_base);
}

void Application::quit() {
	// Remove events
	event_del(ev_event);
	event_del(ev_resize);
}

Application::~Application() {
	// Free events
	event_free(ev_event);
	event_free(ev_resize);
	event_base_free(ev_base);
}

}
