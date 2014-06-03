#ifndef TERMWM_AUX_H
#define TERMWM_AUX_H

#include <malloc.h>

/**
 * Allocate an instance of 't'.
 */
#define new(t) ((t*) malloc(sizeof(t)))

#endif
