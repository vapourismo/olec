#ifndef OLEC_TERMINAL_H
#define OLEC_TERMINAL_H

#include <gtk/gtk.h>
#include <vte/vte.h>

#include <stdint.h>
#include <stdbool.h>

typedef struct {
	GtkWindow* window;
	VteTerminal* terminal;
	GPid child_pid;
} OlecTerminal;

bool olec_terminal_init(OlecTerminal* term);

void olec_terminal_show(const OlecTerminal* term);

bool olec_terminal_spawn(OlecTerminal* term, char** args, char** env);

int olec_terminal_terminate(OlecTerminal* term);

#endif
