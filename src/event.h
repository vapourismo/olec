#ifndef OLEC_EVENT_H
#define OLEC_EVENT_H

#include <gdk/gdk.h>

#include <stdbool.h>
#include <stdint.h>

/**
 * Event type
 */
typedef enum {
	OLEC_KEY_PRESS = 0,
	OLEC_RESIZE = 1
} OlecEventType;

/**
 * Key modifier
 */
typedef GdkModifierType OlecKeyModifier;

/**
 * Key symbol
 */
typedef uint32_t OlecKeySymbol;

/**
 * Event
 */
typedef struct {
	OlecEventType type;
	union {
		struct {
			OlecKeyModifier mod;
			OlecKeySymbol key;
		} key_press;
		struct {
			size_t width, height;
		} resize;
	} info;
} OlecEvent;

/**
 * Write an event to the IPC channel
 */
bool olec_event_write(int fd, const OlecEvent* event);

/**
 * Read an event from the IPC channel
 */
bool olec_event_read(int fd, OlecEvent* event);

#endif
