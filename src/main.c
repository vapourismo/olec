// #include "../lib/chunklist.h"
// #include "../lib/tokenizer.h"
#include "../lib/session.h"
#include "../lib/rect.h"
#include "../lib/window.h"

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

void fill_window(const window_t* window, char c) {
	rect_t tmp;
	window_get_bounds(window, &tmp);
	fill_rect(&tmp, c);
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

	session_start();

	window_t* win = window_new(0, 0, 10, 10);
	fill_window(win, '1');

	refresh();
	fgetc(stdin);

	window_free(win);
	session_stop();

	return 0;
}
