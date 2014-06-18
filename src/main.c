#include "../lib/log.h"
#include "../lib/aux.h"
#include "../lib/text/line.h"

int main(void) {
	/* init app */
	log_open("application.log");

	line_t line;
	line_create(&line);

	line_append_many(&line, "Heo", 3);
	line_insert_many(&line, 2, "ll", 2);

	line_append(&line, 0);
	puts(line.buffer);

	line_destroy(&line);

	/* finalize */
	log_close();

	return 0;
}
