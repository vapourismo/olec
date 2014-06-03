#include "chunklist.h"

#include <malloc.h>
#include <string.h>

#define min(x, y) ((x) < (y) ? (x) : (y))

struct _chunkelem {
	struct _chunkelem* next;
	char* chunk;
};

/* elements */
chunkelem_t chunkelem_new(size_t chunk_size) {
	if (chunk_size == 0)
		chunk_size = 1;

	chunkelem_t elem = (chunkelem_t) malloc(sizeof(struct _chunkelem));

	if (elem) {
		elem->next = NULL;
		elem->chunk = (char*) malloc(chunk_size);

		if (!elem->chunk) {
			free(elem);
			return NULL;
		}
	}

	return elem;
}

void chunkelem_free(chunkelem_t elem) {
	free(elem->chunk);
	free(elem);
}

/* list */
chunklist_t chunklist_new(size_t chunk_size) {
	chunklist_t list = (chunklist_t) malloc(sizeof(struct _chunklist));

	if (list) {
		list->chunk_size = chunk_size;
		list->insert_pos = 0;
		list->head = list->tail = chunkelem_new(chunk_size);

		if (!list->tail) {
			free(list);
			return NULL;
		}
	}

	return list;
}

void chunklist_free(chunklist_t list) {
	chunkelem_t tmp = list->head;

	while (tmp) {
		chunkelem_t delete_me = tmp;
		tmp = tmp->next;

		chunkelem_free(delete_me);
	}
}

int chunklist_ensure_space(chunklist_t list) {
	if (list->insert_pos >= list->chunk_size) {
		/* tail chunk is full, so we need to create another */
		chunkelem_t new_elem = chunkelem_new(list->chunk_size);

		if (!new_elem)
			return 0;

		list->tail = (list->tail->next = new_elem);
		list->insert_pos = 0;
	}

	return 1;
}

int chunklist_append(chunklist_t list, char c) {
	if (!chunklist_ensure_space(list))
		return 0;

	list->tail->chunk[list->insert_pos++] = c;

	return 1;
}

int chunklist_append_multiple(chunklist_t list, const char* source, size_t len) {
	if (len == 0)
		return 1;

	if (!chunklist_ensure_space(list))
		return 0;

	size_t space = min(len, list->chunk_size - list->insert_pos);
	memcpy(list->tail->chunk + list->insert_pos, source, space);

	list->insert_pos += space;

	size_t rest = len - space;

	if (rest > 0) {
		return chunklist_append_multiple(list, source + space, rest);
	} else {
		return 1;
	}
}

int chunklist_get(chunklist_t list, size_t pos) {
	chunkelem_t it = list->head;

	while (it && pos >= list->chunk_size)
		it = it->next;

	if (pos >= list->chunk_size)
		return -1;

	return it->chunk[pos];
}

size_t chunklist_num_chunks(chunklist_t list) {
	size_t counter = 0;
	chunkelem_t it = list->head;

	while (it) {
		counter++;
		it = it->next;
	}

	return counter;
}

size_t chunklist_size(chunklist_t list) {
	return (chunklist_num_chunks(list) - 1) * list->chunk_size
	       + list->insert_pos;
}

char* chunklist_to_string(chunklist_t list) {
	size_t size = chunklist_size(list) + 1;
	char* buffer = (char*) malloc(size);

	if (buffer) {
		chunklist_copy_to_string(list, buffer, size);
	}

	return buffer;
}

void chunklist_copy_to_string(chunklist_t list, char* dest, size_t dest_size) {
	if (dest) {
		chunkelem_t it = list->head;
		size_t offset = 0;

		while (offset < dest_size - 1 && it) {
			size_t left = min(it->next == NULL ? list->insert_pos : list->chunk_size,
			                  (dest_size - 1) - offset);

			memcpy(dest + offset, it->chunk, left);
			offset += left;
			it = it->next;
		}

		dest[offset] = 0;
	}
}
