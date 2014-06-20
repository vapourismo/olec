#ifndef OLEC_TEXT_CHUNK_H
#define OLEC_TEXT_CHUNK_H

#include <stddef.h>

/**
 *
 */
typedef enum {
	CT_INVALID,
	CT_NORMAL
} chunk_type_t;

/**
 * A Chunk Buffer
 */
typedef struct {
	char* buffer;
	size_t tail, length;
	chunk_type_t type;
} chunk_t;

/**
 * Initialize a chunk buffer.
 */
void chunk_create(chunk_t* chunk);

/**
 * Destroy a chunk buffer.
 */
void chunk_destroy(chunk_t* chunk);

/**
 * Append one character to the end of the chunk.
 */
int chunk_append(chunk_t* chunk, char val);

/**
 * Append many characters to the end of the chunk.
 */
int chunk_append_many(chunk_t* chunk, const char* data, size_t len);

/**
 * Insert one character at the given position within the chunk.
 */
int chunk_insert(chunk_t* chunk, size_t pos, char val);

/**
 * Insert many characters at the given position within the chunk.
 */
int chunk_insert_many(chunk_t* chunk, size_t pos, const char* data, size_t len);

/**
 * Split a chunk at a given position.
 */
int chunk_split(chunk_t* chunk, size_t pos, chunk_t* result);

#endif
