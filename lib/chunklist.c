#include "chunklist.h"
#include "aux.h"
#include <malloc.h>
#include <string.h>

typedef struct _chunkelem {
	struct _chunkelem* next;
	char* chunk;
} chunkelem_t;

chunkelem_t* chunkelem_new(size_t chunk_size) {
	if (chunk_size == 0)
		chunk_size = 1;

	/* allocate memory for the base structure and the entire chunk */
	chunkelem_t* elem = (chunkelem_t*) malloc(sizeof(chunkelem_t) + chunk_size);

	if (!elem) return NULL;

	elem->next = NULL;
	elem->chunk = (char*) (elem + 1);

	return elem;
}

void chunkelem_free(chunkelem_t* elem) {
	free(elem);
}

chunklist_t* chunklist_new(size_t chunk_size) {
	chunklist_t* list = new(chunklist_t);

	if (!list) return NULL;

	list->chunk_size = chunk_size;
	list->insert_pos = 0;
	list->head = list->tail = chunkelem_new(chunk_size);

	/* at least one chunkelem should exist */
	if (!list->tail) {
		free(list);
		return NULL;
	}

	return list;
}

void chunklist_free(chunklist_t* list) {
	chunkelem_t* tmp = list->head;

	/* free each element seperately */
	while (tmp) {
		chunkelem_t* delete_me = tmp;
		tmp = tmp->next;

		chunkelem_free(delete_me);
	}

	free(list);
}

int chunklist_ensure_space(chunklist_t* list) {
	if (list->insert_pos >= list->chunk_size) {
		/* tail  is full, so we need to create another */
		chunkelem_t* new_elem = chunkelem_new(list->chunk_size);

		if (!new_elem)
			return 0;

		/* insert new chunk */
		list->tail = (list->tail->next = new_elem);
		list->insert_pos = 0;
	}

	return 1;
}

int chunklist_append(chunklist_t* list, char c) {
	/* check if the current chunk if full;
	   allocate a new one if that's the case */
	if (!chunklist_ensure_space(list))
		return 0;

	list->tail->chunk[list->insert_pos++] = c;

	return 1;
}

int chunklist_append_multiple(chunklist_t* list, const char* source, size_t len) {
	if (len == 0)
		return 1;

	/* check if the current chunk if full;
	   allocate a new one if that's the case */
	if (!chunklist_ensure_space(list))
		return 0;

	/* fill the current chunk (if possible) */
	size_t space = min(len, list->chunk_size - list->insert_pos);
	memcpy(list->tail->chunk + list->insert_pos, source, space);

	/* set the insertion position  */
	list->insert_pos += space;

	size_t rest = len - space;
	if (rest > 0) {
		/* append the rest (if existing) */
		return chunklist_append_multiple(list, source + space, rest);
	} else {
		return 1;
	}
}

int chunklist_get(chunklist_t* list, size_t pos) {
	chunkelem_t* it = list->head;
	size_t chunk_size = list->chunk_size;

	while (it && pos >= chunk_size) {
		it = it->next;
		pos -= chunk_size;
	}

	if (pos >= chunk_size)
		return -1;

	return it->chunk[pos];
}

size_t chunklist_num_chunks(chunklist_t* list) {
	size_t counter = 0;
	chunkelem_t* it = list->head;

	while (it) {
		counter++;
		it = it->next;
	}

	return counter;
}

size_t chunklist_size(chunklist_t* list) {
	return (chunklist_num_chunks(list) - 1) * list->chunk_size
	       + list->insert_pos;
}

char* chunklist_to_string(chunklist_t* list) {
	/* allocate enough space */
	size_t size = chunklist_size(list) + 1;
	char* buffer = (char*) malloc(size);

	if (buffer)
		chunklist_copy_to_string(list, buffer, size);

	return buffer;
}

void chunklist_copy_to_string(chunklist_t* list, char* dest, size_t dest_size) {
	if (dest) {
		chunkelem_t* it = list->head;
		size_t offset = 0;

		/* copy the chunks into 'dest' as long as possible */
		while (offset < dest_size - 1 && it) {
			/* calulate the bytes left to be copied */
			size_t left = min(it->next == NULL ? list->insert_pos : list->chunk_size,
			                  (dest_size - 1) - offset);

			memcpy(dest + offset, it->chunk, left);

			offset += left;
			it = it->next;
		}

		dest[offset] = 0;
	}
}
