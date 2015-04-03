#include "events.h"
#include "js/tpls.h"

#include <cassert>

#include <signal.h>
#include <fcntl.h>
#include <unistd.h>

using namespace std;

namespace olec {

static inline
bool receive_entirely(int fd, void* data, size_t rem) {
	uint8_t* pos = (uint8_t*) data;
	uint8_t* end = pos + rem;

	while (pos != end) {
		ssize_t r = ::read(fd, pos, rem);

		if (r < 0)
			return false;

		pos += r;
		rem -= r;
	}

	return true;
}

static
void cb_resize(int sig, short what, EventDispatcher* app) {
	// Handle window resize
	winsize ws;
	if (ioctl(STDIN_FILENO, TIOCGWINSZ, &ws) != 0)
		return;

	app->resize(ws);
}

static
void cb_event(int fd, short what, EventDispatcher* app) {
	Event ev;
	if (receive_entirely(fd, &ev, sizeof(Event))) {
		app->event(ev);
	}
}

EventDispatcher::EventDispatcher(int fd) {
	ev_base = event_base_new();
	assert(ev_base != nullptr);

	ev_event = event_new(ev_base, fd, EV_PERSIST | EV_READ,
	                     (event_callback_fn) cb_event, this);
	assert(ev_event != nullptr);

	ev_resize = event_new(ev_base, SIGWINCH, EV_PERSIST | EV_SIGNAL,
	                      (event_callback_fn) cb_resize, this);
	assert(ev_resize != nullptr);
}

void EventDispatcher::dispatch() {
	// Add events
	event_add(ev_event, nullptr);
	event_add(ev_resize, nullptr);

	// Dispatch events
	event_base_dispatch(ev_base);
}

void EventDispatcher::quit() {
	// Remove events
	event_del(ev_event);
	event_del(ev_resize);
}

EventDispatcher::~EventDispatcher() {
	// Free events
	event_free(ev_event);
	event_free(ev_resize);
	event_base_free(ev_base);
}

// void EventDispatcher::event(const Event& ev) {
// 	if (ev.type == Event::KeyPress) {
// 		if (ev.info.key_press.mod == GDK_CONTROL_MASK &&
// 		    ev.info.key_press.key == 'q') {

// 			quit();
// 			return;
// 		}

// 		v8::Local<v8::Value> params[2] = {
// 			v8::Uint32::NewFromUnsigned(isolate, ev.info.key_press.mod),
// 			v8::Uint32::NewFromUnsigned(isolate, ev.info.key_press.key)
// 		};

// 		auto eh = v8::Local<v8::Object>::New(isolate, key_handler);
// 		eh->CallAsFunction(v8::Null(isolate), 2, params);
// 	}
// }

// void EventDispatcher::resize(const winsize& ws) {

// }

// void EventDispatcher::set_key_handler(v8::Local<v8::Object> eh) {
// 	if (!eh.IsEmpty() && eh->IsCallable()) {
// 		key_handler.Reset(isolate, eh);
// 	} else {
// 		v8::String::Utf8Value strval(eh);
// 		logwarn("Provided key handler '%s' is not callable", *strval);
// 	}
// }

}
