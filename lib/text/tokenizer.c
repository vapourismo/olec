#include "tokenizer.h"
#include "chunklist.h"
#include "../aux.h"
#include "../log.h"
#include <string.h>
#include <errno.h>

#define TOK_CHUNK_SIZE 1024

tokpattern_t* tokpattern_new(size_t id, const char* preg) {
	tokpattern_t* tp = new(tokpattern_t);

	if (!tp) return NULL;

	/* compile the regular expression */
	if (regcomp(&tp->pattern, preg, REG_EXTENDED) != 0) {
		free(tp);
		return NULL;
	}

	tp->id = id;

	return tp;
}

void tokpattern_free(tokpattern_t* tp) {
	if (tp) {
		/* the regex needs to be freed seperately */
		regfree(&tp->pattern);

		free(tp);
	}
}

ssize_t tokpattern_check(const tokpattern_t* tp, const char* input) {
	regmatch_t m;

	if (regexec(&tp->pattern, input, 1, &m, 0) == 0) {
		/* since we do not want to skip any input,
		   we have to make sure the match starts at offset 0 */
		return m.rm_so == 0 && m.rm_eo > 0 ? m.rm_eo : -1;
	} else {
		return -1;
	}
}

typedef struct _tokelem {
	tokpattern_t* pattern;
	struct _tokelem* next;
} tokelem_t;

tokelem_t* tokelem_new(tokpattern_t* pattern, tokelem_t* next) {
	tokelem_t* te = new(tokelem_t);

	if (te) {
		te->pattern = pattern;
		te->next = next;
	}

	return te;
}

void tokelem_free(tokelem_t* te) {
	if (te) {
		if (te->pattern) tokpattern_free(te->pattern);
		free(te);
	}
}

int tokenizer_new(tokenizer_t* tok, const char* file_path) {
	FILE* file = fopen(file_path, "rt");

	if (!file) {
		errorf("Cannot open '%s': %s", file_path, strerror(errno));
		return 0;
	}

	chunklist_t* buffer = chunklist_new(TOK_CHUNK_SIZE);

	if (!buffer) {
		fclose(file);
		return 0;
	}

	/* fill the chunklist with the file contents */
	char read_buffer[TOK_CHUNK_SIZE];
	ssize_t r;
	while ((r = fread(read_buffer, 1, TOK_CHUNK_SIZE, file)) > 0)
		chunklist_append_multiple(buffer, read_buffer, r);

	/* submit the file contents */
	tok->position = tok->contents = chunklist_to_string(buffer);
	tok->last_position = tok->position + chunklist_size(buffer);

	/* setup token pattern list */
	tok->head = tok->tail = NULL;

	/* free unecessary resources */
	chunklist_free(buffer);
	fclose(file);

	return 1;
}

void tokenizer_free(tokenizer_t* tok) {
	if (!tok) return;

	if (tok->head) {
		/* iterate through the list and free every element */
		tokelem_t* it = tok->head;
		while (it) {
			tokelem_t* to_delete = it;
			it = it->next;
			tokelem_free(to_delete);
		}
	}

	/* free file contents and lastly the structure */
	free(tok->contents);
}

int tokenizer_add(tokenizer_t* tok, tokpattern_t* pattern) {
	if (!pattern)
		return 0;

	tokelem_t* new_elem = tokelem_new(pattern, NULL);

	if (!new_elem)
		return 0;

	/* insert the new element in the back */
	if (!tok->tail)
		tok->head = tok->tail = new_elem;
	else
		tok->tail = tok->tail->next = new_elem;

	return 1;
}

ssize_t tokenizer_do(tokenizer_t* tok, token_t* token) {
	if (tok->position >= tok->last_position)
		return 0;

	tokelem_t* it = tok->head;
	ssize_t m = -1;

	/* iterate through all patterns */
	while (it) {
		if ((m = tokpattern_check(it->pattern, tok->position)) > 0) {
			/* submit the token in case the token pattern applied */
			if (token) {
				token->id = it->pattern->id;
				token->position = tok->position;
				token->length = m;
			}

			/* move to new position */
			tok->position += m;

			break;
		}

		it = it->next;
	}

	if (m < 0) {
		token->position = tok->position;
		token->length = token->id = 0;
	}

	return m;
}
