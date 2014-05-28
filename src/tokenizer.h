#ifndef OLEC_TOKENIZER_H
#define OLEC_TOKENIZER_H

#include <stddef.h>
#include <stdio.h>
#include <unistd.h>

/**
 *
 */
typedef enum {
	T_IDENTIFIER
} token_type_t;

/**
 *
 */
typedef struct _token {
	token_type_t type;
	size_t line, column;
	char* contents;
}* token_t;

/**
 *
 */
void token_free(token_t tok);

/**
 *
 */
typedef struct _input* input_t;

/**
 *
 */
input_t input_new(const char* file);

/**
 *
 */
void input_free(input_t in);

/**
 *
 */
token_t input_identifier(input_t in);

#endif
