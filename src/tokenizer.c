#include "tokenizer.h"
#include "chunklist.h"

#include <malloc.h>
#include <string.h>

#define TOK_DATA_CHUNKS 1024

/* token type */
token_t token_new(token_type_t type,
                  size_t line, size_t col,
                  char* contents) {
	token_t tok = (token_t) malloc(sizeof(struct _token));

	if (tok) {
		tok->type = type;
		tok->line = line;
		tok->column = col;
		tok->contents = contents;
	}

	return tok;
}

void token_free(token_t tok) {
	free(tok->contents);
	free(tok);
}

/* input type */
struct _input {
	FILE* source;
	size_t line, column;
};

input_t input_new(const char* file) {
	input_t in = (input_t) malloc(sizeof(struct _input));

	if (in) {
		in->source = fopen(file, "rt");

		if (!in->source) {
			free(in);
			return NULL;
		}

		in->line = 0;
		in->column = 0;
	}

	return in;
}

void input_free(input_t in) {
	if (in) {
		fclose(in->source);
		free(in);
	}
}

/* helpers */
int input_end(input_t in) {
	return feof(in->source);
}

int input_get(input_t in) {
	in->column++;
	return fgetc(in->source);
}

int input_unget(input_t in) {
	in->column--;
	return fseek(in->source, -1, SEEK_CUR);
}

int input_letter(input_t in, chunklist_t target) {
	return 0;
}

#define is_letter(r) (((r) >= 'a' && (r) <= 'z') \
                      || ((r) >= 'A' && (r) <= 'Z') \
                      || (r) == '_')

#define is_digit(r) ((r) >= '0' && (r) <= '9')

#define is_suffix(r) ((r) == '\'' || (r) == '?' || (r) == '!')

#define is_whitespace(r) ((r) == ' ' \
                          || (r) == '\a' \
                          || (r) == '\b' \
                          || (r) == '\v' \
                          || (r) == '\f' \
                          || (r) == '\t')

/* whitespace */
void input_skip_whitespace(input_t in) {
	int r;

	while ((r = input_get(in)) >= 0) {
		if (!is_whitespace(r)) {
			input_unget(in);
			break;
		}
	}
}

/* tokenizers */
token_t input_identifier(input_t in) {
	size_t ln = in->line,
	       col = in->column;

	int r = input_get(in);

	if (r < 0)
		return NULL;

	chunklist_t buffer = chunklist_new(TOK_DATA_CHUNKS);

	/* prefix [a-zA-Z_] */
	if (is_letter(r)) {
		chunklist_append(buffer, r);
	} else {
		input_unget(in);
		return NULL;
	}

	/* body [a-zA-Z_0-9] */
	while (1) {
		r = input_get(in);

		if (r < 0)
			break;

		if (!((is_letter(r) || is_digit(r))
		      && chunklist_append(buffer, r))) {
			input_unget(in);
			break;
		}
	}

	/* suffix [!?'] */
	while (1) {
		r = input_get(in);

		if (r < 0)
			break;

		if (!(is_suffix(r)
		      && chunklist_append(buffer, r))) {
			input_unget(in);
			break;
		}
	}

	/* finalize buffer */
	token_t tok = token_new(T_IDENTIFIER, ln, col,
	                        chunklist_to_string(buffer));

	chunklist_free(buffer);

	return tok;
}

token_t input_linefeed(input_t in) {
	size_t ln = in->line,
	       col = in->column;

	int r = input_get(in);

	if (r < 0)
		return NULL;

	if (r != '\n') {
		input_unget(in);
		return NULL;
	}

	in->line++;
	in->column = 0;

	return token_new(T_LINEFEED, ln, col, NULL);
}

token_t input_tokenize(input_t in) {
	input_skip_whitespace(in);

	token_t tok = input_linefeed(in);

	if (tok)
		return tok;

	tok = input_identifier(in);

	return tok;
}

