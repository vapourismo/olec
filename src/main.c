#include "terminal.h"

#include <gtk/gtk.h>
#include <vte/vte.h>

#include <string.h>
#include <unistd.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

static bool olec_is_child() {
	return getenv("OLEC_ISCHILD") != NULL;
}

static int olec_lauch_as_parent(int argc, char** argv) {
	char* self_program = argv[0];
	gtk_init(&argc, &argv);

	OlecTerminal term;
	if (olec_terminal_init(&term)) {
		g_signal_connect(term.terminal, "child-exited", G_CALLBACK(gtk_main_quit), NULL);

		char* arguments[] = {self_program, NULL};
		char* environment[] = {"OLEC_ISCHILD=1", NULL};
		olec_terminal_spawn(&term, arguments, environment);

		olec_terminal_show(&term);
		gtk_main();

		olec_terminal_terminate(&term);

		return 0;
	} else {
		fputs("Failed to initialize terminal\n", stderr);
		return 1;
	}
}

static int olec_launch_as_child(int argc, char** argv) {
	puts("Hello! I am the child!");
	sleep(10);
	return 0;
}

int main(int argc, char** argv) {
	if (olec_is_child())
		return olec_launch_as_child(argc, argv);
	else
		return olec_lauch_as_parent(argc, argv);
}
