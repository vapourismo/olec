#ifndef OLEC_TOKENIZER_H
#define OLEC_TOKENIZER_H

#include <stddef.h>
#include <stdio.h>
#include <unistd.h>

typedef enum {
	T_IDENTIFIER,
	T_SEPERATOR,
	T_STRING,
	T_NUMBER,
	T_OPERATOR,
	T_KW_IF,
	T_KW_ELSE,
	T_KW_WHILE,
	T_KW_DO,
	T_KW_FOR,
	T_KW_RETURN,
	T_KW_BREAK,
	T_KW_CONTINUE,
	T_KW_FUN,
	T_KW_LET,
	T_KW_STRUCT,
	T_KW_ENUM,
	T_ERROR
} token_type_t;

typedef struct _token {
	token_type_t type;
	size_t line, column;
	union {
		char* token;
		struct {
			const char* message;
		} error;
	} data;
} token_t;

typedef struct _input {
	FILE* source;
	size_t line, column;
} input_t;

/**
 * A string identifier the token type.
 */
const char* token_type_name(token_type_t type);

/**
 * Deallocate a token
 */
void token_free(token_t* tok);

/**
 * Check an identifier is actually a keyword.
 */
void token_fix_identifier(token_t* tok);

/**
 * Construct a new input source.
 */
input_t* input_new(const char* file);

/**
 * Deallocate the input source.
 */
void input_free(input_t* in);

/**
 * Check if the input has reached the end.
 * Also retrieves the current line and column.
 */
int input_done(const input_t* in);

/**
 * Read an identifier token.
 */
token_t* input_identifier(input_t* in);

/**
 * Read a line feed token.
 */
token_t* input_seperator(input_t* in);

/**
 * Read a string token.
 */
token_t* input_string(input_t* in);

/**
 * Read a number token.
 */
token_t* input_number(input_t* in);

/**
 * Read an operator token.
 */
token_t* input_operator(input_t* in);

/**
 * Read a token.
 */
token_t* input_tokenize(input_t* in);

/**
 * Skip whitespace
 */
void input_skip_whitespace(input_t* in);

#endif
