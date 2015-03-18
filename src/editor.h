#ifndef OLEC_EDITOR_H
#define OLEC_EDITOR_H

#include <sys/types.h>
#include <stdbool.h>
#include <stddef.h>

/**
 * Line editor structure
 */
typedef struct {
	char* contents;
	size_t length, max_length;
} OlecLineEditor;

/**
 * Editor structure
 */
typedef struct {
	OlecLineEditor** lines;
	size_t num_lines;

	size_t cursor_line, cursor_col;
} OlecEditor;

/**
 * Initialize the editor.
 */
void olec_editor_init(OlecEditor* ed);

/**
 * Move the cursor to an absolute position.
 */
void olec_editor_move_cursor(OlecEditor* ed, size_t line, size_t col);

/**
 * Move the cursor relative to its current position.
 */
void olec_editor_move_cursor_relative(OlecEditor* ed, ssize_t line, ssize_t col);

/**
 * Insert a string before the current cursor position.
 */
bool olec_editor_insert_string(OlecEditor* ed, const char* str, size_t len);

/**
 * Insert a character before the current cursor position.
 */
bool olec_editor_insert_char(OlecEditor* ed, char chr);

/**
 * Insert a line before the cursor line.
 */
bool olec_editor_insert_lines(OlecEditor* ed, size_t num);

/**
 * Remove a character at the current position.
 */
void olec_editor_remove_char(OlecEditor* ed);

/**
 * Remove the current cursor line.
 */
void olec_editor_remove_line(OlecEditor* ed);

/**
 * Join the current cursor line with the `add_lines` following lines.
 */
void olec_editor_join_lines(OlecEditor* ed, size_t add_lines);

#endif
