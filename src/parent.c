#include "parent.h"
#include "terminal.h"

#include <string.h>

int olec_parent_launch(int argc, char** argv) {
	char* self_program = argv[0];
	gtk_init(&argc, &argv);

	OlecTerminal term;
	if (olec_terminal_init(&term)) {
		// Environment for child
		char environ_var[strlen(term.ipc_path) + 14];
		environ_var[0] = 0;

		strcat(environ_var, "OLEC_IPC=");
		strcat(environ_var, term.ipc_path);

		char* environment[] = {environ_var, NULL};

		// Generate child program arguments
		char* arguments[argc + 2];
		arguments[0] = self_program;
		arguments[argc + 1] = NULL;

		for (int i = 1; i < argc; i++)
			arguments[i] = argv[i];

		// Spawn child process
		if (olec_terminal_spawn(&term, arguments, environment)) {
			// Give control to GTK
			olec_terminal_show(&term);
			gtk_main();
		} else {
			fputs("Failed to spawn child", stderr);
		}

		olec_terminal_clean(&term);

		return 0;
	} else {
		fputs("Failed to initialize terminal\n", stderr);
		return 1;
	}
}
