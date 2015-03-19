#ifndef OLEC_TERMINAL_H_
#define OLEC_TERMINAL_H_

#include "ipc.h"

#include <gtk/gtk.h>
#include <vte/vte.h>
#include <vector>
#include <string>

namespace olec {

/**
 * Terminal configuration
 */
struct TerminalConfig {
	const char* font_description;
	const char* palette[16];
};

/**
 * Default terminal configuration
 */
extern
const TerminalConfig default_config;

/**
 * Virtual terminal window
 */
struct Terminal {
	enum Error {
		/**
		 * Failed to create a communication channel
		 */
		CommInitFailed,

		/**
		 * Failed to instantiate a widget
		 */
		WidgetInitFailed,

		/**
		 * Failed to spawn a child process
		 */
		SpawnChildFailed
	};

	GtkWindow* window;
	VteTerminal* terminal;

	CommChannel* channel;
	std::vector<std::string> child_cmdline;

	/**
	 * Create a terminal window and configure the virtual terminal emulator
	 * using the given configuration.
	 */
	Terminal(const TerminalConfig& config = default_config) throw (Error);

	/**
	 * Free resources
	 */
	~Terminal();

	/**
	 * Show the window and its children.
	 */
	inline
	void show() {
		gtk_widget_show_all(GTK_WIDGET(window));
	}

	/**
	 * Spawn a process to be display by the terminal.
	 */
	void spawn(const std::vector<std::string>& cmdline) throw (Error);
};

}

#endif
