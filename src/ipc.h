#ifndef OLEC_IPC_H_
#define OLEC_IPC_H_

#include <gdk/gdk.h>

#include <cstdint>
#include <string>

namespace Olec {

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

	Event(KeyModifier mod, KeySymbol key) {
		type = KeyPress;
		info.key_press.mod = mod;
		info.key_press.key = key;
	}
};

/**
 * Inter-process communication channel
 */
struct CommChannel {
	/**
	 * FIFO File Descriptor
	 */
	int fd;

	/**
	 * This is set if the FIFO shall be cleaned upon deconstruction
	 */
	std::string clean_me;

	/**
	 * Copy constructor
	 */
	CommChannel(const CommChannel& me) = delete;

	/**
	 * Move constructor
	 */
	inline CommChannel(CommChannel&& other) {
		fd = other.fd;
		other.fd = -1;
	}

	/**
	 * Open an existing communication channel.
	 */
	CommChannel(const std::string& path, bool make = false);

	/**
	 * Automatically close the communication channel.
	 */
	~CommChannel();

	/**
	 * Send an event.
	 */
	bool send(const Event& event);

	/**
	 * Receive an event.
	 */
	bool receive(Event& event);
};

}

#endif
