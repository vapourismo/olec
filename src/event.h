#ifndef OLEC_EVENT_H
#define OLEC_EVENT_H

#include <gdk/gdk.h>

#include <stdbool.h>
#include <stdint.h>

typedef enum {
	OLEC_KEY_PRESS = 0,
} OlecEventType;

typedef struct {
	OlecEventType type;
	union {
		struct {
			GdkModifierType mod;
			uint32_t key;
		} key_press;
	} info;
} OlecEvent;

bool olec_event_write(int fd, const OlecEvent* event);

bool olec_event_read(int fd, OlecEvent* event);

#endif
