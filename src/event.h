#ifndef OLEC_EVENT_H
#define OLEC_EVENT_H

#include <gdk/gdk.h>

#include <stdbool.h>
#include <stdint.h>

/**
 * Event Type
 */
typedef enum {
	OLEC_KEY_PRESS = 0,
} OlecEventType;

/**
 * Event
 */
typedef struct {
	OlecEventType type;
	union {
		struct {
			GdkModifierType mod;
			uint32_t key;
		} key_press;
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
