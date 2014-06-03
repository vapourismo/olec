#ifndef OLEC_TOKENIZER_H
#define OLEC_TOKENIZER_H

#include <unistd.h>
#include <regex.h>

typedef struct {
	size_t id;
	regex_t pattern;
} tokpattern_t;

/**
 * Create a new token pattern.
 */
tokpattern_t* tokpattern_new(size_t id, const char* preg);

/**
 * Free a token pattern.
 */
void tokpattern_free(tokpattern_t* tp);

/**
 * Check whether the token pattern applies to the given input.
 */
ssize_t tokpattern_check(const tokpattern_t* tp, const char* input);

#endif
