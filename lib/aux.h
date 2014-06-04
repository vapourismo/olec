#ifndef TERMWM_AUX_H
#define TERMWM_AUX_H

#include <malloc.h>

/**
 * Allocate an instance of 't'.
 */
#define new(t) ((t*) malloc(sizeof(t)))

/**
 * Return the smaller value.
 */
#define min(x, y) ((x) < (y) ? (x) : (y))

#endif
