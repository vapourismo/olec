#ifndef OLEC_TOKENIZER_H
#define OLEC_TOKENIZER_H

#include <unistd.h>
#include <regex.h>

/**
 * Token Pattern (Regular Expression)
 */
typedef struct {
	size_t id;
	regex_t pattern;
} tokpattern_t;

/**
 * Constuct a token pattern.
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

/**
 * Parsed Token
 */
typedef struct {
	size_t id;
	const char* position;
	size_t length;
} token_t;

/**
 * Tokenizer
 */
typedef struct {
	char* contents;
	const char* position;
	const char* last_position;

	struct _tokelem* head;
	struct _tokelem* tail;
} tokenizer_t;

/**
 * Construct a tokenizer.
 */
int tokenizer_new(tokenizer_t* tok, const char* file_path);

/**
 * Free a tokenizer.
 */
void tokenizer_free(tokenizer_t* tok);

/**
 * Add a token pattern.
 */
int tokenizer_add(tokenizer_t* tok, tokpattern_t* pattern);

/**
 * Try to apply one of the given token patterns.
 * Returns 0 if the tokenizer has reached the end of input,
 * otherwise returns -1 for invalid input or the amount of characters matched.
 */
ssize_t tokenizer_do(tokenizer_t* tok, token_t* out);

#endif
