#include "../lib/log.h"
#include "../lib/aux.h"
#include "../lib/text/line.h"

int main(void) {
	/* init app */
	log_open("application.log");

	line_t line1, line2;
	line_create(&line1);

	line_append_many(&line1, "Heo", 3);
	line_insert_many(&line1, 2, "ll", 2);

	line_split(&line1, 2, &line2);

	line_append(&line1, 0);
	line_append(&line2, 0);

	puts(line1.buffer);
	puts(line2.buffer);

	line_destroy(&line1);

	/* finalize */
	log_close();

	return 0;
}
