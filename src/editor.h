#ifndef OLEC_EDITOR_H
#define OLEC_EDITOR_H

#include <sys/types.h>
#include <stdbool.h>
#include <stddef.h>

/**
 *
 */
typedef struct {
	char* contents;
	size_t length, max_length;
} OlecLineEditor;

/**
 *
 */
typedef struct {
	OlecLineEditor** lines;
	size_t num_lines;

	size_t cursor_line, cursor_col;
} OlecEditor;

/**
 *
 */
void olec_editor_init(OlecEditor* ed);

/**
 *
 */
void olec_editor_move_cursor(OlecEditor* ed, size_t line, size_t col);

/**
 *
 */
bool olec_editor_insert_string(OlecEditor* ed, const char* str, size_t len);

/**
 *
 */
bool olec_editor_insert_char(OlecEditor* ed, char chr);

/**
 *
 */
void olec_editor_move_cursor_relative(OlecEditor* ed, ssize_t line, ssize_t col);

/**
 *
 */
bool olec_editor_insert_lines(OlecEditor* ed, size_t num);

/**
 *
 */
void olec_editor_remove_char(OlecEditor* ed);

/**
 *
 */
void olec_editor_remove_line(OlecEditor* ed);

/**
 *
 */
void olec_editor_join_lines(OlecEditor* ed, size_t add_lines);

#endif
