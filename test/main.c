#include "../src/tokenizer.h"
#include "../src/chunklist.h"

#include <stdio.h>
#include <malloc.h>
#include <string.h>

int main(void) {
	input_t* in = input_new("test/example.olec");
	token_t* tok;

	while ((tok = input_tokenize(in)) != NULL) {
		printf("%s [%lu, %lu]: %s\n", token_type_name(tok->type), tok->line, tok->column, tok->contents);
		token_free(tok);
	}

	if (input_done(in)) {
		printf("> Done\n");
	} else {
		printf("> Failed (%lu, %lu)\n", in->line, in->column);
	}

	input_free(in);

	return 0;
}
