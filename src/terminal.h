#ifndef OLEC_TERMINAL_H_
#define OLEC_TERMINAL_H_

#include "events.h"

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
 * Terminal event hub
 */
struct TerminalEventHub: ThreadedEventHub<Event> {
	TerminalEventHub();

	virtual
	void handle(const Event& event);
};

/**
 * Virtual terminal window
 */
struct Terminal {
	enum Error {
		/**
		 * Failed to create a FIFO as communication channel
		 */
		CommCreateFailed,

		/**
		 * Failed to open the FIFO communication channel
		 */
		CommOpenFailed,

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

	std::vector<std::string> child_cmdline;

	TerminalEventHub event_hub;

	/**
	 * Create a terminal window and configure the virtual terminal emulator
	 * using the given configuration.
	 */
	Terminal(const TerminalConfig& config = default_config) throw (Error);

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
