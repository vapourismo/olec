#ifndef OLEC_TOKENIZER_H
#define OLEC_TOKENIZER_H

#include <unistd.h>
#include <regex.h>


/*
 * Token Pattern
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


/*
 * Token
 */

typedef struct {
	size_t id;
	char* contents;
} token_t;

/**
 * Free contents associated with the given token-
 */
void token_free_contents(token_t* token);


/*
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
tokenizer_t* tokenizer_new(const char* file_path);

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
 */
ssize_t tokenizer_do(tokenizer_t* tok, token_t* out);

#endif
