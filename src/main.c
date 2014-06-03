#include "../lib/chunklist.h"

#include <stdio.h>
#include <malloc.h>
#include <string.h>
#include <regex.h>

int main(void) {
	regex_t re[5];

	regcomp(re,     "^[a-zA-Z_][a-zA-Z_]*['?!]*",       REG_EXTENDED);
	regcomp(re + 1, "^\"((\\\\.)|[^\"])*\"",            REG_EXTENDED);
	regcomp(re + 2, "^[0-9]+(\\.[0-9]+)?([eE][0-9]+)?", REG_EXTENDED);
	regcomp(re + 3, "^[\n\r]+",                         REG_EXTENDED);
	regcomp(re + 4, "^\\s+",                            REG_EXTENDED);

	FILE* input = fopen("src/example.olec", "rb");

	if (input) {
		chunklist_t buffer = chunklist_new(1024);

		char read_buffer[1024];
		ssize_t r;

		while ((r = fread(read_buffer, 1, 1024, input)) > 0) {
			chunklist_append_multiple(buffer, read_buffer, r);
		}

		fclose(input);

		char* contents = chunklist_to_string(buffer);
		size_t len = chunklist_size(buffer);
		chunklist_free(buffer);

		size_t i = 0;

		while (i < len) {
			int m = -1;
			regmatch_t match;

			for (int g = 0; g < 5; g++) {
				if (regexec(re + g, contents + i, 1, &match, 0) == 0) {
					m = g;
					break;
				}
			}

			if (m < 0) {
				puts("Unknown token!");
				break;
			} else {
				printf("> Token [%i]: ", match.rm_eo);
				fwrite(contents + i, 1, match.rm_eo, stdout);
				putchar('\n');

				i += match.rm_eo;
			}
		}
	}

	return 0;
}
