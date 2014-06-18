#include "../aux.h"
#include "line.h"
#include <malloc.h>
#include <string.h>

#define LINE_SEGMENT_SIZE 20

int line_check_buffer(line_t* line, size_t len) {
	if (!line->buffer) {
		/* allocate the buffer if there is none */
		line->buffer = (char*) malloc(max(LINE_SEGMENT_SIZE, len));
		line->tail = 0;
		line->length = LINE_SEGMENT_SIZE;
	} else if (line->tail + len > line->length) {
		/* preserve old buffer */
		char* old_buffer = line->buffer;

		/* allocate new buffer with increased size */
		line->length += LINE_SEGMENT_SIZE;
		line->buffer = (char*) malloc(line->length);

		/* if the buffer has been allocated copy the old contents */
		if (line->buffer)
			memcpy(line->buffer, old_buffer, line->tail);

		/* free the old buffer, since it's no longer needed */
		free(old_buffer);
	}

	return line->buffer != NULL;
}

void line_create(line_t* line) {
	line->buffer = NULL;
	line->tail = line->length = 0;
}

void line_destroy(line_t* line) {
	if (line->buffer) {
		free(line->buffer);
	}
}

int line_append(line_t* line, char val) {
	return line_append_many(line, &val, 1);
}

int line_append_many(line_t* line, const char* data, size_t len) {
	if (!line_check_buffer(line, len))
		return 0;

	/* append the given data to the end of the buffer */
	memcpy(line->buffer + line->tail, data, len);
	line->tail += len;

	return 1;
}

int line_insert(line_t* line, size_t pos, char val) {
	return line_insert_many(line, pos, &val, 1);
}

int line_insert_many(line_t* line, size_t pos, const char* data, size_t len) {
	if (!line_check_buffer(line, len))
		return 0;

	/* the insertion position may not exceed the end of line */
	pos = min(line->tail, pos);

	/* move the contents after 'pos' to the right
	   and copy the data into the newly created space */
	memmove(line->buffer + pos + len, line->buffer + pos, line->tail - pos);
	memcpy(line->buffer + pos, data, len);

	line->tail += len;

	return 1;
}
