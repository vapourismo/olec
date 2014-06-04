#include "../lib/chunklist.h"
#include "../lib/tokenizer.h"
#include "../lib/session.h"
#include "../lib/layout.h"

#include <stdio.h>
#include <malloc.h>
#include <string.h>
#include <regex.h>

#define T_SPACE      0
#define T_IDENTIFIER 1
#define T_STRING     2
#define T_NUMBER     3
#define T_SEPERATOR  4
#define T_OPERATOR   5
#define T_INVALID    6

void fill_rect(const rect_t* rect, char c) {
	for (size_t i = 0; i < rect->h; i++) {
		move(rect->y + i, rect->x);

		for (size_t j = 0; j < rect->w; j++)
			addch(c);
	}
}

int main(void) {
	// tokenizer_t* tok = tokenizer_new("src/example.olec");

	// if (tok) {
	// 	tokenizer_add(tok, tokpattern_new(T_SPACE,      "[\a\b\t\f\v ]+"));
	// 	tokenizer_add(tok, tokpattern_new(T_IDENTIFIER, "[a-zA-Z_][a-zA-Z_]*['?!]*"));
	// 	tokenizer_add(tok, tokpattern_new(T_STRING,     "\"((\\\\.)|[^\"])*\""));
	// 	tokenizer_add(tok, tokpattern_new(T_NUMBER,     "[0-9]+(\\.[0-9]+)?([eE][0-9]+)?"));
	// 	tokenizer_add(tok, tokpattern_new(T_SEPERATOR,  "[\n\r]+[\\s\n\r]*"));
	// 	tokenizer_add(tok, tokpattern_new(T_OPERATOR,   "[\\+\\-\\*/%\\^=<>!?\\.:,;]+"));
	// 	tokenizer_add(tok, tokpattern_new(T_INVALID,    "."));

	// 	ssize_t r;
	// 	token_t token;

	// 	while ((r = tokenizer_do(tok, &token)) > 0) {
	// 		if (token.id != T_SPACE)
	// 			printf("[%03zi: %03lu]: %s\n", token.offset, token.id, token.contents);

	// 		token_free_contents(&token);
	// 	}

	// 	if (r < 0) {
	// 		printf("Unknown token at index %lu\n", token.offset);
	// 	}
	// }

	// tokenizer_free(tok);

	session_t s;
	session_start(&s);

	rect_t base = {(size_t) s.root->_begx, (size_t) s.root->_begy,
	               (size_t) s.root->_maxx + 1, (size_t) s.root->_maxy + 1};

	rect_t a, b;

	rect_vsplit_rel(&base, &a, &b, 0.25);

	fill_rect(&a, 'A');
	fill_rect(&b, 'B');

	refresh();

	fgetc(stdin);

	session_stop(&s);

	return 0;
}
