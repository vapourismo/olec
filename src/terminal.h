#ifndef OLEC_TERMINAL_H_
#define OLEC_TERMINAL_H_

#include <gtk/gtk.h>
#include <vte/vte.h>

namespace Olec {

struct TerminalConfig {
	const char* palette[16];
};

struct Terminal {
	GtkWindow* window;
	VteTerminal* terminal;

	Terminal(TerminalConfig& config);
};

}

#endif
