#ifndef OLEC_TEXT_LINE_H
#define OLEC_TEXT_LINE_H

#include <stddef.h>

/**
 * A Line Buffer
 */
typedef struct {
	char* buffer;
	size_t tail, length;
} line_t;

/**
 * Initialize a line buffer.
 */
void line_create(line_t* line);

/**
 * Destroy a line buffer.
 */
void line_destroy(line_t* line);

/**
 * Append one character to the end of the line.
 */
int line_append(line_t* line, char val);

/**
 * Append many characters to the end of the line.
 */
int line_append_many(line_t* line, const char* data, size_t len);

/**
 * Insert one character at the given position within the line.
 */
int line_insert(line_t* line, size_t pos, char val);

/**
 * Insert many characters at the given position within the line.
 */
int line_insert_many(line_t* line, size_t pos, const char* data, size_t len);

#endif
