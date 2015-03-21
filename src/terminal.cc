#include "terminal.h"

#include <iostream>
#include <string>
#include <algorithm>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>

using namespace std;

namespace olec {

static
gboolean cb_key_press(GtkWidget* widget, GdkEventKey* event, Terminal* term) {
	if (!event->is_modifier) {
		Event ev((KeyModifier) (event->state & GDK_MODIFIER_MASK), event->keyval);
		ipc_send(term->ipc_fd, ev);
	}

	return true;
}

static
void cb_child_exit(VteTerminal* terminal, gint status, Terminal* term) {
	if (term->ipc_fd >= 0) {
		close(term->ipc_fd);
		term->ipc_fd = -1;
	}

	switch (WEXITSTATUS(status)) {
		// Child wants to be reloaded
		// TODO: Resolve `case OLEC_CHILD_EXIT_RELOAD:`
		case 1:
			cout << "Reload child ... " << endl;
			term->spawn(term->child_cmdline);

			break;

		// Good or bad doesn't matter, we'll exit
		default:
			cout << "Child exited with status " << WEXITSTATUS(status) << endl;
			gtk_main_quit();

			break;
	}
}

extern
const TerminalConfig default_config = {
	"Inconsolata 10.5",
	{"#1A1A1A", "#D9715F", "#B2CC46", "#FFCB55", "#6486BC", "#AD7FA8", "#06989A", "#D5D5D5",
	 "#1A1A1A", "#D9715F", "#B2CC46", "#FFCB55", "#6486BC", "#AD7FA8", "#06989A", "#D5D5D5"}
};

Terminal::Terminal(const TerminalConfig& config) throw (Terminal::Error):
	window(GTK_WINDOW(gtk_window_new(GTK_WINDOW_TOPLEVEL))),
	terminal(VTE_TERMINAL(vte_terminal_new())),
	ipc_path("/tmp/olec-" + to_string(getpid()))
{
	if (!window || !terminal)
		throw WidgetInitFailed;

	// Set window details
	gtk_window_set_title(window, "Olec Text Editor");
	gtk_window_set_wmclass(window, "olec", "olec");

	// Connect signals
	g_signal_connect(window, "destroy", G_CALLBACK(gtk_main_quit), NULL);
	g_signal_connect(window, "key-press-event", G_CALLBACK(cb_key_press), this);
	g_signal_connect(terminal, "child-exited", G_CALLBACK(cb_child_exit), this);

	// Setup layout
	GtkWidget* box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	gtk_container_add(GTK_CONTAINER(window), box);
	gtk_box_pack_start(GTK_BOX(box), GTK_WIDGET(terminal), true, true, 0);

	// Configure font
	PangoFontDescription* font = pango_font_description_from_string(config.font_description);
	vte_terminal_set_font(terminal, font);

	// Configure colors
	GdkRGBA term_palette[16];
	for (size_t i = 0; i < 16; i++)
		gdk_rgba_parse(term_palette + i, config.palette[i] ? config.palette[i] : "#ffffff");

	vte_terminal_set_colors(terminal, NULL, NULL, term_palette, 16);

	// Configure miscellaneous settings
	vte_terminal_set_allow_bold(terminal, true);
	vte_terminal_set_encoding(terminal, "UTF-8", NULL);
	vte_terminal_set_cursor_shape(terminal, VTE_CURSOR_SHAPE_IBEAM);
	vte_terminal_set_cursor_blink_mode(terminal, VTE_CURSOR_BLINK_OFF);
	vte_terminal_set_scrollback_lines(terminal, 0);

	// Create FIFO for IPC
	if (mkfifo(ipc_path.c_str(), S_IFIFO | S_IWUSR | S_IRUSR) != 0)
		throw CommCreateFailed;
}

Terminal::~Terminal() {
	unlink(ipc_path.c_str());
}

void Terminal::spawn(const std::vector<std::string>& cmdline) throw (Terminal::Error) {
	// Transform vector as needed. This might be dangerous...
	std::vector<char*> cstr_cmdline(cmdline.size() + 1);
	transform(cmdline.begin(), cmdline.end(), cstr_cmdline.begin(), [](const std::string& str) {
		return const_cast<char*>(str.c_str());
	});
	cstr_cmdline[cmdline.size()] = NULL;

	// Setup environment
	string env_tag = "OLEC_IPC=" + ipc_path;
	char* environment[] = {
		const_cast<char*>(env_tag.c_str()),
		NULL
	};

	// Save command line for respawning
	child_cmdline = cmdline;

	// Spawn child process
	GPid child_pid;
	if (!vte_terminal_spawn_sync(terminal,
	                             VTE_PTY_DEFAULT,
	                             NULL,
	                             cstr_cmdline.data(),
	                             environment,
	                             G_SPAWN_DEFAULT,
	                             NULL, NULL,
	                             &child_pid,
	                             NULL, NULL))
		throw SpawnChildFailed;

	// Watch child so we get to handle a 'child-exited' event
	vte_terminal_watch_child(terminal, child_pid);

	// Open IPC channel
	ipc_fd = open(ipc_path.c_str(), O_WRONLY);

	if (ipc_fd < 0)
		throw CommOpenFailed;
}

}
