#include "../lib/log.h"
#include "../lib/aux.h"
#include "../lib/text/chunk.h"

typedef struct _source_element {
	struct _source_element* prev;
	struct _source_element* next;
	chunk_t chunk;
} source_element_t;

source_element_t* source_element_new() {
	source_element_t* elem = new(source_element_t);

	if (elem) {
		chunk_create(&elem->chunk);
		elem->next = elem->prev = NULL;
	}

	return elem;
}

void source_element_free(source_element_t* elem) {
	chunk_destroy(&elem->chunk);
	free(elem);
}

typedef struct {
	source_element_t* head;
	source_element_t* tail;

	size_t target_column, target_row, target_offset;
	source_element_t* target;
} source_t;

void source_create(source_t* src) {
	src->target = src->head = src->tail = source_element_new();
	src->target_column = src->target_row = src->target_offset = 0;
}

void source_destroy(source_t* src) {
	source_element_t* it = src->head;

	while (it) {
		source_element_t* to_delete = it;
		it = it->next;
		source_element_free(to_delete);
	}
}

void source_insert(source_t* src, const char* data, size_t len) {
	chunk_insert_many(&src->target->chunk, src->target_offset, data, len);
}

int main(void) {
	/* init app */
	log_open("application.log");

	source_t src;
	source_create(&src);

	source_insert(&src, "Hello World", 12);
	puts(src.target->chunk.buffer);

	source_destroy(&src);

	/* finalize */
	log_close();

	return 0;
}
