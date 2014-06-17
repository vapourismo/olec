#ifndef OLEC_LOG_H
#define OLEC_LOG_H

#include <stdio.h>

extern FILE* _log_file;

#define _log_write(msg) { fprintf(_log_file, "[\033[33m%s\033[0m:\033[33m%i\033[0m] " msg "\n", __FILE__, __LINE__); fflush(_log_file); }
#define _log_writef(msg, ...) { fprintf(_log_file, "[\033[33m%s\033[0m:\033[33m%i\033[0m] " msg "\n", __FILE__, __LINE__, __VA_ARGS__); fflush(_log_file); }

/**
 * Start logging to 'file'
 */
#define log_open(file) { _log_file = fopen(file, "wt+"); }

/**
 * Stop logging
 */
#define log_close() { if (_log_file) fclose(_log_file); }

/* Error Macros */
#define error(msg) _log_write("\033[31mE\033[0m " msg)
#define errorf(msg, ...) _log_writef("\033[31mE\033[0m " msg, __VA_ARGS__)

/* Information Macros */
#define inform(msg) _log_write("\033[34mI\033[0m " msg)
#define informf(msg, ...) _log_writef("\033[34mI\033[0m " msg, __VA_ARGS__)

/* Debug Macros */
#ifdef DEBUG
	#define debug(msg) _log_write("\033[35mD\033[0m " msg)
	#define debugf(msg, ...) _log_writef("\033[35mD\033[0m " msg, __VA_ARGS__)
#else
	#define debug(msg)
	#define debugf(msg, ...)
#endif

#endif
