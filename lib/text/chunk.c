#include "../aux.h"
#include "chunk.h"
#include <malloc.h>
#include <string.h>

#define CHUNK_SEGMENT_SIZE 20

int chunk_check_buffer(chunk_t* chunk, size_t len) {
	if (!chunk->buffer) {
		/* allocate the buffer if there is none */
		chunk->buffer = (char*) malloc(max(CHUNK_SEGMENT_SIZE, len));
		chunk->tail = 0;
		chunk->length = CHUNK_SEGMENT_SIZE;
	} else if (chunk->tail + len > chunk->length) {
		/* preserve old buffer */
		char* old_buffer = chunk->buffer;

		/* allocate new buffer with increased size */
		chunk->length += CHUNK_SEGMENT_SIZE;
		chunk->buffer = (char*) malloc(chunk->length);

		/* if the buffer has been allocated copy the old contents */
		if (chunk->buffer)
			memcpy(chunk->buffer, old_buffer, chunk->tail);

		/* free the old buffer, since it's no longer needed */
		free(old_buffer);
	}

	return chunk->buffer != NULL;
}

void chunk_create(chunk_t* chunk) {
	chunk->buffer = NULL;
	chunk->tail = chunk->length = 0;
}

void chunk_destroy(chunk_t* chunk) {
	if (chunk->buffer) {
		free(chunk->buffer);
	}
}

int chunk_append(chunk_t* chunk, char val) {
	return chunk_append_many(chunk, &val, 1);
}

int chunk_append_many(chunk_t* chunk, const char* data, size_t len) {
	if (!chunk_check_buffer(chunk, len))
		return 0;

	/* append the given data to the end of the buffer */
	memcpy(chunk->buffer + chunk->tail, data, len);
	chunk->tail += len;

	return 1;
}

int chunk_insert(chunk_t* chunk, size_t pos, char val) {
	return chunk_insert_many(chunk, pos, &val, 1);
}

int chunk_insert_many(chunk_t* chunk, size_t pos, const char* data, size_t len) {
	if (!chunk_check_buffer(chunk, len))
		return 0;

	/* the insertion position may not exceed the end of chunk */
	pos = min(chunk->tail, pos);

	/* move the contents after 'pos' to the right
	   and copy the data into the newly created space */
	memmove(chunk->buffer + pos + len, chunk->buffer + pos, chunk->tail - pos);
	memcpy(chunk->buffer + pos, data, len);

	chunk->tail += len;

	return 1;
}

int chunk_split(chunk_t* chunk, size_t pos, chunk_t* result) {
	chunk_create(result);

	if (pos < chunk->tail) {
		size_t len = chunk->tail - pos;
		chunk_append_many(result, chunk->buffer + pos, len);
		chunk->tail -= len;
	}

	return 1;
}
