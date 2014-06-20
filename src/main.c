#include "../lib/log.h"
#include "../lib/aux.h"
#include "../lib/text/chunk.h"

int main(void) {
	/* init app */
	log_open("application.log");

	chunk_t chunk1, chunk2;
	chunk_create(&chunk1);

	chunk_append_many(&chunk1, "Heo", 3);
	chunk_insert_many(&chunk1, 2, "ll", 2);

	chunk_split(&chunk1, 2, &chunk2);

	chunk_append(&chunk1, 0);
	chunk_append(&chunk2, 0);

	puts(chunk1.buffer);
	puts(chunk2.buffer);

	chunk_destroy(&chunk1);

	/* finalize */
	log_close();

	return 0;
}
