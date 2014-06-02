#include "tokenizer.h"
#include "chunklist.h"

#include <malloc.h>
#include <string.h>
#include <strings.h>

#define TOK_DATA_CHUNKS 32

/* token type */
const char* token_type_name(token_type_t type) {
	switch (type) {
		case T_IDENTIFIER:
			return "identifier";

		case T_SEPERATOR:
			return "seperator";

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

		case T_NUMBER:
			return "number";

		case T_OPERATOR:
			return "operator";

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
		/* free the string associated with the token,
		   but only if it's not an error token */
		if (tok->type != T_ERROR) {
			free(tok->data.token);
		}

		free(tok);
	}
}

void token_fix_identifier(token_t* tok) {
	/* only valid T_IDENTIFIER tokens can be fixed here */
	if (!tok || tok->type != T_IDENTIFIER || tok->type == T_ERROR)
		return;

	const char* cnt = tok->data.token;

	/* figure out if the identifier is a keyword,
	   by comparing the associated string */
	if (strcasecmp(cnt, "if") == 0)            tok->type = T_KW_IF;
	else if (strcasecmp(cnt, "else") == 0)     tok->type = T_KW_ELSE;
	else if (strcasecmp(cnt, "while") == 0)    tok->type = T_KW_WHILE;
	else if (strcasecmp(cnt, "do") == 0)       tok->type = T_KW_DO;
	else if (strcasecmp(cnt, "for") == 0)      tok->type = T_KW_FOR;
	else if (strcasecmp(cnt, "return") == 0)   tok->type = T_KW_RETURN;
	else if (strcasecmp(cnt, "break") == 0)    tok->type = T_KW_BREAK;
	else if (strcasecmp(cnt, "continue") == 0) tok->type = T_KW_CONTINUE;
	else if (strcasecmp(cnt, "fun") == 0)      tok->type = T_KW_FUN;
	else if (strcasecmp(cnt, "let") == 0)      tok->type = T_KW_LET;
	else if (strcasecmp(cnt, "struct") == 0)   tok->type = T_KW_STRUCT;
	else if (strcasecmp(cnt, "enum") == 0)     tok->type = T_KW_ENUM;
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
	return getc(in->source);
}

int input_unget(input_t* in, int c) {
	in->column--;
	return ungetc(c, in->source);
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
                          || (r) == '\r' \
                          || (r) == '\t')

#define is_operator(r) (index("+-*/%$=,;.:&^|!", (r)) != NULL)

/* whitespace */
void input_skip_whitespace(input_t* in) {
	int r;

	while ((r = input_get(in)) >= 0) {
		if (!is_whitespace(r)) {
			input_unget(in, r);
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
		input_unget(in, r);
		return NULL;
	}

	/* body [a-zA-Z_0-9] */
	while (1) {
		r = input_get(in);

		if (r < 0)
			break;

		if (!is_letter(r) && !is_digit(r)) {
			input_unget(in, r);
			break;
		}

		chunklist_append(buffer, r);
	}

	/* suffix [!?'] */
	while (1) {
		r = input_get(in);

		if (r < 0)
			break;

		if (!is_suffix(r)) {
			input_unget(in, r);
			break;
		}

		chunklist_append(buffer, r);
	}

	/* finalize buffer */
	token_t* tok = token_new(T_IDENTIFIER, ln, col,
	                         chunklist_to_string(buffer));

	chunklist_free(buffer);

	/* fix in case this identifier is a keyword */
	token_fix_identifier(tok);

	return tok;
}

token_t* input_seperator(input_t* in) {
	size_t ln = in->line,
	       col = in->column;

	int r = input_get(in);

	if (r < 0)
		return NULL;

	/* first line feed */
	if (r != '\n') {
		input_unget(in, r);
		return NULL;
	}

	in->line++;
	in->column = 0;

	/* consume the following whitespaces and line feeds.
	   why? because two empty lines are semantically equal to one empty line */
	while (1) {
		r = input_get(in);

		if (r < 0)
			break;

		if (r == '\n') {
			in->line++;
			in->column = 0;
		} else if (!is_whitespace(r)) {
			input_unget(in, r);
			break;
		}
	}

	return token_new(T_SEPERATOR, ln, col, NULL);
}

token_t* input_string(input_t* in) {
	size_t ln = in->line,
	       col = in->column;

	int r = input_get(in);

	if (r < 0)
		return NULL;

	if (r != '"') {
		input_unget(in, r);
		return NULL;
	}

	chunklist_t buffer = chunklist_new(TOK_DATA_CHUNKS);

	while (1) {
		r = input_get(in);

		if (r == '"') {
			break;
		} else if (r == '\\') {
			/* escape sequence */

			r = input_get(in);

			if (r < 0)
				return token_error(ln, col, "Unterminated string literal");

			/* convert special characters */
			switch (r) {
				case 'n': r = '\n'; break;
				case 'r': r = '\r'; break;
				case 't': r = '\t'; break;
				case 'f': r = '\f'; break;
				case 'v': r = '\v'; break;
				case 'a': r = '\a'; break;
				case 'b': r = '\b'; break;
				case 'e': r = 27; break;
				default: break;
			}

			chunklist_append(buffer, r);
		} else if (r < 0) {
			return token_error(ln, col, "Unterminated string literal");
		} else {
			if (r == '\n') {
				in->line++;
				in->column = 0;
			}

			chunklist_append(buffer, r);
		}
	}

	token_t* tok = token_new(T_STRING, ln, col, chunklist_to_string(buffer));
	chunklist_free(buffer);

	return tok;
}

token_t* input_number(input_t* in) {
	size_t ln = in->line,
	       col = in->column;

	int r = input_get(in);

	if (r < 0)
		return NULL;

	if (!is_digit(r)) {
		input_unget(in, r);
		return NULL;
	}

	chunklist_t buffer = chunklist_new(TOK_DATA_CHUNKS);
	chunklist_append(buffer, r);

	char cont = 0;

	/* read prefix */
	while (1) {
		r = input_get(in);

		if (r < 0)
			break;

		if (r == '.' || r == 'e' || r == 'E') {
			chunklist_append(buffer, r);

			if (r == 'E')
				r = 'e';

			cont = r;
			break;
		}

		if (!is_digit(r)) {
			input_unget(in, r);
			break;
		}

		chunklist_append(buffer, r);
	}

	/* read fraction */
	if (cont == '.') {
		cont = 0;

		while (1) {
			r = input_get(in);

			if (r < 0)
				break;

			if (r == 'e' || r == 'E') {
				chunklist_append(buffer, r);
				cont = 'e';
				break;
			}

			if (!is_digit(r)) {
				input_unget(in, r);
				break;
			}

			chunklist_append(buffer, r);
		}
	}

	/* read exponent */
	if (cont == 'e') {
		while (1) {
			r = input_get(in);

			if (r < 0)
				break;

			if (!is_digit(r)) {
				input_unget(in, r);
				break;
			}

			chunklist_append(buffer, r);
		}
	}

	token_t* tok = token_new(T_NUMBER, ln, col, chunklist_to_string(buffer));
	chunklist_free(buffer);

	return tok;
}

token_t* input_operator(input_t* in) {
	size_t ln = in->line,
	       col = in->column;

	int r = input_get(in);

	if (r < 0)
		return NULL;

	if (!is_operator(r)) {
		input_unget(in, r);
		return NULL;
	}

	chunklist_t buffer = chunklist_new(TOK_DATA_CHUNKS);
	chunklist_append(buffer, r);

	while (1) {
		r = input_get(in);

		if (r < 0)
			break;

		if (!is_operator(r)) {
			input_unget(in, r);
			break;
		}

		chunklist_append(buffer, r);
	}

	token_t* tok = token_new(T_OPERATOR, ln, col, chunklist_to_string(buffer));
	chunklist_free(buffer);

	return tok;
}

token_t* input_tokenize(input_t* in) {
	input_skip_whitespace(in);

	if (feof(in->source))
		return NULL;

	token_t* tok = input_seperator(in);

	if (tok)
		return tok;

	if ((tok = input_identifier(in)))
		return tok;

	if ((tok = input_string(in)))
		return tok;

	if ((tok = input_number(in)))
		return tok;

	if ((tok = input_operator(in)))
		return tok;

	return token_error(in->line, in->column, "Unknown input");
}
