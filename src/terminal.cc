#include "terminal.h"

#include <iostream>
#include <string>
#include <unistd.h>

using namespace std;

namespace Olec {

static
gboolean cb_key_press(GtkWidget* widget, GdkEventKey* event, Terminal* term) {
	if (!event->is_modifier) {
		Event ev((KeyModifier) (event->state & GDK_MODIFIER_MASK), event->keyval);
		term->channel.send(ev);
	}

	return true;
}

static
void cb_child_exit(VteTerminal* terminal, gint status, Terminal* term) {
	switch (WEXITSTATUS(status)) {
		// Child wants to be reloaded
		// RESOLVE: case OLEC_CHILD_EXIT_RELOAD:
		case 1:
			// TODO: Respawn child

			break;

		// Good or bad doesn't matter, we'll exit
		default:
			cout << "Child exited with status " << WEXITSTATUS(status) << endl;
			gtk_main_quit();

			break;
	}
}

static string mk_new_ipc() {
	string path = "/tmp/olec-" + to_string(getpid());

	if (!CommChannel::create(path))
		throw;

	return path;
}

extern
const TerminalConfig default_config = {
	"Inconsolata 10.5",
	{"#1A1A1A", "#D9715F", "#B2CC46", "#FFCB55", "#6486BC", "#AD7FA8", "#06989A", "#D5D5D5",
	 "#1A1A1A", "#D9715F", "#B2CC46", "#FFCB55", "#6486BC", "#AD7FA8", "#06989A", "#D5D5D5"}
};

Terminal::Terminal(const TerminalConfig& config):
	window(GTK_WINDOW(gtk_window_new(GTK_WINDOW_TOPLEVEL))),
	terminal(VTE_TERMINAL(vte_terminal_new())),
	channel(mk_new_ipc())
{
	if (!window || !terminal)
		throw;

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
}

}
