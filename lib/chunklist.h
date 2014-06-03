#ifndef OLEC_CHUNKLIST_H
#define OLEC_CHUNKLIST_H

#include <stddef.h>
#include <unistd.h>

typedef struct _chunkelem chunkelem_t;

typedef struct _chunklist {
	size_t chunk_size;
	size_t insert_pos;

	chunkelem_t* head;
	chunkelem_t* tail;
} chunklist_t;

/**
 * Create a new chunk list.
 */
chunklist_t* chunklist_new(size_t chunk_size);

/**
 * Free a chunk list.
 */
void chunklist_free(chunklist_t* list);

/**
 * Add a character to the list of characters managed by the list.
 */
int chunklist_append(chunklist_t* list, char c);

/**
 *
 */
int chunklist_append_multiple(chunklist_t* list, const char* source, size_t len);

/**
 *
 */
int chunklist_get(chunklist_t* list, size_t pos);

/**
 *
 */
ssize_t chunklist_get_multiple(chunklist_t* list, size_t pos, char* dest, size_t len);

/**
 * Calculate the number of chunks used.
 */
size_t chunklist_num_chunks(chunklist_t* list);

/**
 * Calculate the effective amount of characters held by the list.
 */
size_t chunklist_size(chunklist_t* list);

/**
 * Allocate a string containing all characters managed by the list.
 * The returned string is null-terminated and allocated using 'malloc'.
 */
char* chunklist_to_string(chunklist_t* list);

/**
 * Copy all characters managed by the list into 'dest'.
 * The string pointed to by dest is null-terminated.
 */
void chunklist_copy_to_string(chunklist_t* list, char* dest, size_t dest_size);

#endif
