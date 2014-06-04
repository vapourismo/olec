#ifndef OLEC_SESSION_H
#define OLEC_SESSION_H

#include <ncurses.h>


/*
 * Type Definitions
 */

typedef WINDOW curswin;


/*
 * Session
 */

typedef struct {
	curswin* root;
} session_t;

/**
 * Spawn a unicorn.
 */
int session_start(session_t* session);

/**
 * Get rich.
 */
void session_stop(session_t* session);

#endif
