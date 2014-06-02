#include "../src/tokenizer.h"
#include "../src/chunklist.h"

#include <stdio.h>
#include <malloc.h>
#include <string.h>

int main(void) {
	input_t* in = input_new("test/example.olec");
	token_t* tok;

	while ((tok = input_tokenize(in)) != NULL && tok->type != T_ERROR) {
		printf("%s [%lu, %lu]: %s\n", token_type_name(tok->type),
		       tok->line, tok->column,
		       tok->data.token);
		token_free(tok);
	}

	if (tok && tok->type == T_ERROR) {
		printf("> Failed (%lu, %lu): %s\n",
		       tok->line, tok->column,
		       tok->data.error.message);
	}

	input_free(in);

	return 0;
}
