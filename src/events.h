#ifndef OLEC_COMM_H_
#define OLEC_COMM_H_

#include "anchor.h"

#include <v8.h>
#include <gdk/gdk.h>
#include <sys/ioctl.h>
#include <termios.h>
#include <event2/event.h>

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

struct EventDispatcher {
	event_base* ev_base;
	event* ev_event;
	event* ev_resize;

	EventDispatcher(int fd);

	~EventDispatcher();

	void dispatch();

	void quit();

	virtual
	void event(const Event& ev) = 0;

	virtual
	void resize(const winsize& ws) = 0;
};

}

#endif
