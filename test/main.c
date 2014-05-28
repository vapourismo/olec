#include "../src/tokenizer.h"
#include "../src/chunklist.h"

#include <stdio.h>
#include <malloc.h>
#include <string.h>

int main(void) {
	input_t in = input_new("test/example.olec");

	token_t tok;

	while ((tok = input_identifier(in)) != NULL) {
		printf("[%lu, %lu]: %s\n", tok->line, tok->column, tok->contents);
		token_free(tok);
	}

	input_free(in);

	return 0;
}
