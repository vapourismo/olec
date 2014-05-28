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
	size_t line, col;
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
		in->col = 0;
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
	return fgetc(in->source);
}

ssize_t input_get_multiple(input_t in, char* buffer, size_t length) {
	return fread(buffer, 1, length, in->source);
}

int input_unget(input_t in) {
	return fseek(in->source, -1, SEEK_CUR);
}

int input_unget_multiple(input_t in, size_t length) {
	return fseek(in->source, -length, SEEK_CUR);
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

/* tokenizers */
token_t input_identifier(input_t in) {
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
	token_t tok = token_new(T_IDENTIFIER, in->line, in->col,
	                        chunklist_to_string(buffer));

	in->col += chunklist_size(buffer);
	chunklist_free(buffer);

	return tok;
}
