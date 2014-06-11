#include "../lib/text/tokenizer.h"
#include "../lib/log.h"
#include <string.h>

int main(int argc, char** argv) {
	log_open("application.log");

	tokenizer_t tok;
	if (tokenizer_new(&tok, "src/example.olec")) {
		tokenizer_add(&tok, tokpattern_new(0, "\\s+"));
		tokenizer_add(&tok, tokpattern_new(1, "[^ \t\n\r\a\b\v\f]+"));

		token_t out;
		char buffer[1024];

		while (tokenizer_do(&tok, &out) > 0) {
			memcpy(buffer, out.position, out.length);
			buffer[out.length] = 0;
			printf("%lu: %s\n", out.id, buffer);
		}

		tokenizer_free(&tok);
	}

	log_close();
	return 0;
}
