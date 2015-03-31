#ifndef OLEC_COMM_H_
#define OLEC_COMM_H_

#include <gdk/gdk.h>
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

/**
 * Send a chunk of data in it's entirety.
 */
bool send_entirely(int fd, const void* data, size_t len);

/**
 * Receive the requested amount of bytes.
 */
bool receive_entirely(int fd, void* data, size_t len);

}

#endif
