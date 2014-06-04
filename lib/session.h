#ifndef OLEC_SESSION_H
#define OLEC_SESSION_H

#include <ncurses.h>

typedef WINDOW curswin;

/**
 * Spawn a unicorn.
 */
int session_start();

/**
 * Get rich.
 */
void session_stop();

#endif
