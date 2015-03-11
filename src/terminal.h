#ifndef OLEC_TERMINAL_H
#define OLEC_TERMINAL_H

#include <gtk/gtk.h>
#include <vte/vte.h>

#include <stdint.h>
#include <stdbool.h>

typedef struct {
	GtkWindow* window;
	VteTerminal* terminal;

	char* ipc_path;
	int ipc_fifo;

	char** child_args;
} OlecTerminal;

bool olec_terminal_init(OlecTerminal* term);

void olec_terminal_show(const OlecTerminal* term);

bool olec_terminal_spawn(OlecTerminal* term, char** args);

void olec_terminal_clean(const OlecTerminal* term);

#endif
