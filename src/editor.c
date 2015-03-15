#include "editor.h"

#include <malloc.h>
#include <string.h>
#include <ctype.h>

typedef struct _OlecLineEditor {
	char* contents;
	size_t length, max_length;
} OlecLineEditor;

#define LE_CHUNK_SIZE 80

static
OlecLineEditor* le_new() {
	OlecLineEditor* le = malloc(sizeof(OlecLineEditor));

	if (le) {
		le->contents = malloc(LE_CHUNK_SIZE);
		le->length = 0;
		le->max_length = LE_CHUNK_SIZE;
	}

	return le;
}

static
bool le_ensure_space(OlecLineEditor* le, size_t add_length) {
	if (le->length + add_length <= le->max_length)
		return true;

	// Save contents
	char storage[le->length];
	memcpy(storage, le->contents, le->length);

	// Reallocate memory
	char* new_contents = realloc(le->contents, le->max_length + LE_CHUNK_SIZE);

	if (!new_contents)
		return false;

	// Restore contents
	if (new_contents != le->contents) {
		memcpy(new_contents, storage, le->length);
		le->contents = new_contents;
	}

	le->max_length += LE_CHUNK_SIZE;

	return true;
}

static
bool le_insert_char(OlecLineEditor* le, size_t col, char c) {
	if (!isprint(c) || !le_ensure_space(le, 1))
		return false;

	if (col > le->length)
		col = le->length;

	if (col < le->length)
		memmove(le->contents + col + 1, le->contents + col, le->length - col);

	le->contents[col] = c;
	le->length++;

	return true;
}

static
void le_remove_char(OlecLineEditor* le, size_t col) {
	if (col >= le->length)
		return;

	memmove(le->contents + col, le->contents + col + 1, le->length - col - 1);
	le->length--;
}

static
void le_check_area(OlecLineEditor* le, size_t col, size_t len) {
	size_t col_end = col + len;

	while (col < le->length && col < col_end) {
		if (!isprint(le->contents[col])) {
			le_remove_char(le, col);
			col_end--;
		} else {
			col++;
		}
	}
}

static
bool le_insert_string(OlecLineEditor* le, size_t col, const char* str, size_t len) {
	if (!le_ensure_space(le, len))
		return false;

	if (col > le->length)
		col = le->length;

	if (col < le->length)
		memmove(le->contents + col + len, le->contents + col, le->length - col);

	memcpy(le->contents + col, str, len);
	le->length++;

	le_check_area(le, col, len);

	return true;
}

void olec_editor_init(OlecEditor* ed) {
	ed->lines = calloc(sizeof(OlecLineEditor*), 1);
	ed->lines[0] = le_new();
	ed->num_lines = 1;

	ed->cursor_line = 0;
	ed->cursor_col = 0;
}

void olec_editor_move_cursor(OlecEditor* ed, size_t line, size_t col) {
	if (line >= ed->num_lines)
		ed->cursor_line = ed->num_lines - 1;
	else
		ed->cursor_line = line;

	OlecLineEditor* le = ed->lines[ed->cursor_line];

	if (col > le->length)
		ed->cursor_col = le->length;
	else
		ed->cursor_col = col;
}

void olec_editor_move_cursor_relative(OlecEditor* ed, ssize_t line, ssize_t col) {
	if (line < 0 && ed->cursor_line < (size_t) -line) {
		ed->cursor_line = 0;
	} else if (line > 0 && ed->cursor_line + line >= ed->num_lines) {
		ed->cursor_line = ed->num_lines - 1;
	} else {
		ed->cursor_line += line;
	}

	OlecLineEditor* le = ed->lines[ed->cursor_line];

	if (col < 0 && ed->cursor_col < (size_t) -col) {
		ed->cursor_col = 0;
	} else if (col > 0 && ed->cursor_col + col > le->max_length) {
		ed->cursor_col = le->max_length;
	} else {
		ed->cursor_col += col;
	}
}

bool olec_editor_insert_string(OlecEditor* ed, const char* str, size_t len) {
	OlecLineEditor* le = ed->lines[ed->cursor_line];

	if (le_insert_string(le, ed->cursor_col, str, len)) {
		ed->cursor_col += len;
		return true;
	}

	return false;
}

bool olec_editor_insert_char(OlecEditor* ed, char chr) {
	OlecLineEditor* le = ed->lines[ed->cursor_line];

	if (le_insert_char(le, ed->cursor_col, chr)) {
		ed->cursor_col++;
		return true;
	}

	return false;
}

bool olec_editor_insert_lines(OlecEditor* ed, size_t num) {
	if (num == 0)
		return true;

	// Save old line handles
	OlecLineEditor* storage[ed->num_lines];
	memcpy(storage, ed->lines, ed->num_lines);

	// Reallocate memory
	OlecLineEditor** new_lines = calloc(sizeof(OlecLineEditor*), ed->num_lines + num);

	if (!new_lines)
		return false;

	free(ed->lines);
	ed->lines = new_lines;

	// Restore before cursor
	memcpy(new_lines, storage, ed->cursor_line);

	// Create new lines
	for (size_t i = 0; i < num; i++)
		new_lines[i + ed->cursor_line] = le_new();

	// Restore after cursor
	memcpy(new_lines + ed->cursor_line + num, storage + ed->cursor_line, ed->num_lines - ed->cursor_line);

	ed->cursor_line += num;
	ed->num_lines += num;

	return true;
}
