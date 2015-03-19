#ifndef OLEC_IPC_H_
#define OLEC_IPC_H_

#include <gdk/gdk.h>

#include <cstdint>
#include <string>

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

	Event(KeyModifier mod, KeySymbol key) {
		type = KeyPress;
		info.key_press.mod = mod;
		info.key_press.key = key;
	}
};

/**
 * Send an event through the communication channel.
 */
bool ipc_send(int fd, const Event& event);

/**
 * Receive an event from the communication channel.
 */
bool ipc_receive(int fd, Event& event);

}

#endif
