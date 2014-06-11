#include "../lib/tokenizer.h"
#include "../lib/log.h"

int main(int argc, char** argv) {
	log_open("application.log");

	tokenizer_t tok;
	if (tokenizer_new(&tok, "src/example.olec.404")) {
		tokenizer_free(&tok);
	}

	log_close();

	return 0;
}
