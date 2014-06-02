#include "tokenizer.h"
#include "chunklist.h"

#include <malloc.h>
#include <string.h>
#include <strings.h>

#define TOK_DATA_CHUNKS 1024

/* token type */
const char* token_type_name(token_type_t type) {
	switch (type) {
		case T_IDENTIFIER:
			return "identifier";

		case T_LINEFEED:
			return "line feed";

		case T_KW_IF:
			return "if";

		case T_KW_ELSE:
			return "else";

		case T_KW_WHILE:
			return "while";

		case T_KW_DO:
			return "do";

		case T_KW_FOR:
			return "for";

		case T_KW_RETURN:
			return "return";

		case T_KW_BREAK:
			return "break";

		case T_KW_CONTINUE:
			return "continue";

		case T_KW_FUN:
			return "fun";

		case T_KW_LET:
			return "let";

		case T_KW_STRUCT:
			return "struct";

		case T_KW_ENUM:
			return "enum";

		case T_STRING:
			return "string";

		default:
			return "unknown";
	}
}

token_t* token_new(token_type_t type,
                   size_t line, size_t col,
                   char* contents) {
	token_t* tok = (token_t*) malloc(sizeof(struct _token));

	if (tok) {
		tok->type = type;
		tok->line = line;
		tok->column = col;
		tok->data.token = contents;
	}

	return tok;
}

token_t* token_error(size_t line, size_t col,
                     const char* msg) {
	token_t* tok = (token_t*) malloc(sizeof(struct _token));

	if (tok) {
		tok->type = T_ERROR;
		tok->line = line;
		tok->column = col;
		tok->data.error.message = msg;
	}

	return tok;
}


void token_free(token_t* tok) {
	if (tok) {
		if (tok->type != T_ERROR) {
			free(tok->data.token);
		}

		free(tok);
	}
}

void token_fix_identifier(token_t* tok) {
	if (!tok || tok->type != T_IDENTIFIER || tok->type == T_ERROR)
		return;

	const char* cnt = tok->data.token;

	if (strcasecmp(cnt, "if") == 0)
		tok->type = T_KW_IF;
	else if (strcasecmp(cnt, "else") == 0)
		tok->type = T_KW_ELSE;
	else if (strcasecmp(cnt, "while") == 0)
		tok->type = T_KW_WHILE;
	else if (strcasecmp(cnt, "do") == 0)
		tok->type = T_KW_DO;
	else if (strcasecmp(cnt, "for") == 0)
		tok->type = T_KW_FOR;
	else if (strcasecmp(cnt, "return") == 0)
		tok->type = T_KW_RETURN;
	else if (strcasecmp(cnt, "break") == 0)
		tok->type = T_KW_BREAK;
	else if (strcasecmp(cnt, "continue") == 0)
		tok->type = T_KW_CONTINUE;
	else if (strcasecmp(cnt, "fun") == 0)
		tok->type = T_KW_FUN;
	else if (strcasecmp(cnt, "let") == 0)
		tok->type = T_KW_LET;
	else if (strcasecmp(cnt, "struct") == 0)
		tok->type = T_KW_STRUCT;
	else if (strcasecmp(cnt, "enum") == 0)
		tok->type = T_KW_ENUM;
}

/* input type */
input_t* input_new(const char* file) {
	input_t* in = (input_t*) malloc(sizeof(struct _input));

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

void input_free(input_t* in) {
	if (in) {
		fclose(in->source);
		free(in);
	}
}

int input_done(const input_t* in) {
	return feof(in->source) != 0;
}

/* helpers */
int input_get(input_t* in) {
	in->column++;
	return fgetc(in->source);
}

int input_unget(input_t* in) {
	in->column--;
	return fseek(in->source, -1, SEEK_CUR);
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
void input_skip_whitespace(input_t* in) {
	int r;

	while ((r = input_get(in)) >= 0) {
		if (!is_whitespace(r)) {
			input_unget(in);
			break;
		}
	}
}

/* tokenizers */
token_t* input_identifier(input_t* in) {
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
	token_t* tok = token_new(T_IDENTIFIER, ln, col,
	                        chunklist_to_string(buffer));

	chunklist_free(buffer);

	/* fix in case of a keyword */
	token_fix_identifier(tok);

	return tok;
}

token_t* input_linefeed(input_t* in) {
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

token_t* input_string(input_t* in) {
	size_t ln = in->line,
	       col = in->column;

	int r = input_get(in);

	if (r < 0)
		return NULL;

	if (r != '"') {
		input_unget(in);
		return NULL;
	}

	chunklist_t list = chunklist_new(TOK_DATA_CHUNKS);

	while (1) {
		r = input_get(in);

		if (r == '"') {
			break;
		} else if (r == '\\') {
			/* escape sequence */

		} else if (r < 0) {
			return token_error(ln, col, "Unterminated string literal");
		} else {
			chunklist_append(list, r);
		}
	}

	token_t* tok = token_new(T_STRING, ln, col, chunklist_to_string(list));
	chunklist_free(list);

	return tok;
}

token_t* input_tokenize(input_t* in) {
	input_skip_whitespace(in);

	if (feof(in->source))
		return NULL;

	token_t* tok = input_linefeed(in);

	if (tok)
		return tok;

	tok = input_identifier(in);

	if (tok)
		return tok;

	tok = input_string(in);

	if (!tok)
		return token_error(in->line, in->column, "Unknown input");
	else
		return tok;
}
