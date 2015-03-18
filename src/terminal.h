#ifndef OLEC_TERMINAL_H_
#define OLEC_TERMINAL_H_

#include "ipc.h"

#include <gtk/gtk.h>
#include <vte/vte.h>

namespace Olec {

struct TerminalConfig {
	const char* font_description;
	const char* palette[16];
};

extern
const TerminalConfig default_config;

struct Terminal {
	GtkWindow* window;
	VteTerminal* terminal;

	CommChannel channel;

	Terminal(const TerminalConfig& config = default_config);
};

}

#endif
