#ifndef OLEC_COMM_H_
#define OLEC_COMM_H_

#include <string>
#include <list>
#include <thread>
#include <cstdint>
#include <cassert>

#include <sys/socket.h>
#include <sys/un.h>

#include <unistd.h>
#include <string.h>

#include <gdk/gdk.h>
#include <event2/event.h>
#include <event2/thread.h>

namespace olec {

/**
 * Event type
 */
typedef enum {
	OLEC_KEY_PRESS = 0,
} EventType;

/**
 * Key modifier
 */
typedef GdkModifierType KeyModifier;

/**
 * Key symbol
 */
typedef uint32_t KeySymbol;

/**
 * Event
 */
struct Event {
	enum {
		KeyPress = 0,
	} type;

	union {
		struct {
			KeyModifier mod;
			KeySymbol key;
		} key_press;
	} info;

	inline Event() {}

	inline Event(KeyModifier mod, KeySymbol key) {
		type = KeyPress;
		info.key_press.mod = mod;
		info.key_press.key = key;
	}
};

/**
 * Send a chunk of data in it's entirety.
 */
bool send_entirely(int fd, const void* data, size_t rem);

/**
 * Receive the requested amount of bytes.
 */
bool receive_entirely(int fd, void* data, size_t rem);

/**
 * Main event hub
 */
template <typename T>
struct EventHub {
	static
	void dispatch_read(int fd, short, EventHub<T>* self) {
		T data;
		if (receive_entirely(fd, &data, sizeof(T))) {
			self->handle(data);
		}
	}

	static
	void dispatch_accept(int fd, short, EventHub<T>* self) {
		int cl = accept(fd, nullptr, nullptr);
		evutil_make_socket_nonblocking(cl);

		event* ev_read =
			event_new(self->ev_base, cl, EV_PERSIST | EV_READ,
			          (event_callback_fn) dispatch_read, self);
		event_add(ev_read, NULL);

		self->listeners.push_back(ev_read);
	}

	int fd;
	std::string path;
	std::list<event*> listeners;

	event_base* ev_base;
	event* ev_accept;

	EventHub(const std::string& un_path):
		path(un_path)
	{
		fd = socket(AF_UNIX, SOCK_STREAM, 0);
		assert(fd >= 0);

		// Generate socket address
		sockaddr_un un_addr;
		memset(&un_addr, 0, sizeof(un_addr));

		un_addr.sun_family = AF_UNIX;
		strcpy(un_addr.sun_path, un_path.c_str());

		// Bind and listen
		int bind_result = bind(fd, (const sockaddr*) &un_addr, sizeof(un_addr));
		assert(bind_result == 0);

		int listen_result = listen(fd, 2);
		assert(listen_result == 0);

		evutil_make_socket_nonblocking(fd);

		// Create event base and accept event
		ev_base = event_base_new();
		assert(ev_base != nullptr);

		ev_accept = event_new(ev_base, fd, EV_PERSIST | EV_READ, (event_callback_fn) dispatch_accept, this);
		assert(ev_accept != nullptr);
	}

	~EventHub() {
		// Free all listeners
		for_each(listeners.begin(), listeners.end(), event_free);

		// Free events
		event_free(ev_accept);
		event_base_free(ev_base);

		// Close socket and remove occupied unix socket
		close(fd);
		unlink(path.c_str());
	}

	void dispatch() {
		event_add(ev_accept, nullptr);
		event_base_dispatch(ev_base);
	}

	void notify(const T& data) {
		// Send message to every node
		for (auto it: listeners) {
			send_entirely(event_get_fd(it), &data, sizeof(T));
		}
	}

	void exit() {
		event_del(ev_accept);

		// Remove all listeners
		for_each(listeners.begin(), listeners.end(), event_del);
	}

	virtual
	void handle(const T& data) = 0;
};

/**
 * Participant in the event network
 */
template <typename T>
struct EventNode {
	static
	void dispatch_read(int fd, short, EventNode* self) {
		T data;
		if (receive_entirely(fd, &data, sizeof(T))) {
			self->handle(data);
		}
	}

	int fd;
	event_base* ev_base;
	event* ev_read;

	EventNode(const std::string& un_path) {
		fd = socket(AF_UNIX, SOCK_STREAM, 0);
		assert(fd >= 0);

		// Generate socket address
		sockaddr_un un_addr;
		memset(&un_addr, 0, sizeof(un_addr));

		un_addr.sun_family = AF_UNIX;
		strcpy(un_addr.sun_path, un_path.c_str());

		// Connect to event hub
		int connect_result = connect(fd, (const sockaddr*) &un_addr, sizeof(un_addr));
		assert(connect_result == 0);

		evutil_make_socket_nonblocking(fd);

		// Generate event base and read event
		ev_base = event_base_new();
		assert(ev_base != nullptr);

		ev_read = event_new(ev_base, fd, EV_PERSIST | EV_READ,
		                    (event_callback_fn) dispatch_read, this);
		assert(ev_read != nullptr);
	}

	~EventNode() {
		// Remove events
		event_free(ev_read);
		event_base_free(ev_base);

		// Signal disconnect
		send(fd, nullptr, 0, 0);
		close(fd);
	}

	void dispatch() {
		event_add(ev_read, NULL);
		event_base_dispatch(ev_base);
	}

	bool notify(const T& data) {
		return send_entirely(fd, &data, sizeof(T));
	}

	void exit() {
		event_del(ev_read);
	}

	virtual
	void handle(const T& data) = 0;
};

struct _EvThreadSetup {
	inline
	_EvThreadSetup() {
		assert(evthread_use_pthreads() == 0);
	}
};

/**
 * Threaded event hub
 */
template <typename T>
struct ThreadedEventHub: _EvThreadSetup, EventHub<T> {
	static void thread_worker(ThreadedEventHub<T>* self) {
		self->dispatch();
	}

	std::thread worker;

	ThreadedEventHub(const std::string& un_path):
		EventHub<T>(un_path),
		worker(thread_worker, this)
	{}

	~ThreadedEventHub() {
		EventHub<T>::exit();
		worker.join();
	}

	void exit() = delete;
};

}

#endif
