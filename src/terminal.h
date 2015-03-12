#ifndef OLEC_TERMINAL_H
#define OLEC_TERMINAL_H

#include <gtk/gtk.h>
#include <vte/vte.h>

#include <stdint.h>
#include <stdbool.h>

/**
 * Terminal configuration
 */
typedef struct {
	const char* palette[16];
} OlecTerminalConfig;

/**
 * Terminal
 */
typedef struct {
	GtkWindow* window;
	VteTerminal* terminal;

	char* ipc_path;
	int ipc_fifo;

	char** child_args;
} OlecTerminal;

/**
 * Initialize the terminal
 */
bool olec_terminal_init(OlecTerminal* term, const OlecTerminalConfig* config);

/**
 * Display the terminal
 */
void olec_terminal_show(const OlecTerminal* term);

/**
 * Spawn a child to run within the terminal
 */
bool olec_terminal_spawn(OlecTerminal* term, char** args);

/**
 * Clean resources after usage
 */
void olec_terminal_clean(const OlecTerminal* term);

#endif
