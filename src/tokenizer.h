#ifndef OLEC_TOKENIZER_H
#define OLEC_TOKENIZER_H

#include <stddef.h>
#include <stdio.h>
#include <unistd.h>

typedef enum {
	T_IDENTIFIER,
	T_LINEFEED
} token_type_t;

typedef struct _token {
	token_type_t type;
	size_t line, column;
	char* contents;
}* token_t;

/**
 * Deallocate a token
 */
void token_free(token_t tok);

typedef struct _input* input_t;

/**
 * Construct a new input source.
 */
input_t input_new(const char* file);

/**
 * Deallocate the input source.
 */
void input_free(input_t in);

/**
 * Read an identifier token.
 */
token_t input_identifier(input_t in);

/**
 * Read a line feed token.
 */
token_t input_linefeed(input_t in);

/**
 *
 */
token_t input_tokenize(input_t in);

/**
 * Skip whitespace
 */
void input_skip_whitespace(input_t in);


#endif
