#ifndef OLEC_SESSION_H
#define OLEC_SESSION_H

/**
 * Spawn a unicorn.
 */
int session_start();

/**
 * Get rich.
 */
void session_stop();

/**
 * Render all changes to the screen.
 */
void session_render();

#endif
