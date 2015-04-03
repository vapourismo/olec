#include "terminal.h"

#include <iostream>
#include <string>
#include <algorithm>
#include <unistd.h>
#include <fcntl.h>
#include <pty.h>
#include <sys/stat.h>

using namespace std;

namespace olec {

static inline
bool send_entirely(int fd, const void* data, size_t rem) {
	const uint8_t* pos = (const uint8_t*) data;
	const uint8_t* end = pos + rem;

	while (pos != end) {
		ssize_t r = ::write(fd, pos, rem);

		if (r < 0)
			return false;

		pos += r;
		rem -= r;
	}

	return true;
}

static
gboolean cb_key_press(GtkWidget* widget, GdkEventKey* event, Terminal* self) {
	if (!event->is_modifier) {
		Event ev((KeyModifier) (event->state & GDK_MODIFIER_MASK), event->keyval);
		send_entirely(self->fifo_fd, &ev, sizeof(Event));
	}

	return true;
}

static
void cb_child_exit(VteTerminal* terminal, gint status, Terminal* self) {
	cout << "Child exited with status " << WEXITSTATUS(status) << endl;
	gtk_main_quit();
}

extern
const TerminalConfig default_config = {
	"Inconsolata 10.5",
	{"#1A1A1A", "#D9715F", "#B2CC46", "#FFCB55", "#6486BC", "#AD7FA8", "#06989A", "#D5D5D5",
	 "#1A1A1A", "#D9715F", "#B2CC46", "#FFCB55", "#6486BC", "#AD7FA8", "#06989A", "#D5D5D5"}
};

Terminal::Terminal(const Anchor& anchor, const TerminalConfig& config) throw (Terminal::Error):
	fifo_fd(anchor.fifo_fd),
	window(GTK_WINDOW(gtk_window_new(GTK_WINDOW_TOPLEVEL))),
	terminal(VTE_TERMINAL(vte_terminal_new()))
{
	if (!window || !terminal)
		throw WidgetInitFailed;

	// Set window details
	gtk_window_set_title(window, "Olec Text Editor");
	gtk_window_set_wmclass(window, "olec", "olec");

	// Connect signals
	g_signal_connect(window, "destroy", G_CALLBACK(gtk_main_quit), nullptr);
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

	vte_terminal_set_colors(terminal, nullptr, nullptr, term_palette, 16);

	// Configure miscellaneous settings
	vte_terminal_set_allow_bold(terminal, true);
	vte_terminal_set_encoding(terminal, "UTF-8", nullptr);
	vte_terminal_set_cursor_shape(terminal, VTE_CURSOR_SHAPE_IBEAM);
	vte_terminal_set_cursor_blink_mode(terminal, VTE_CURSOR_BLINK_OFF);
	vte_terminal_set_scrollback_lines(terminal, 0);

	// Set PTY target
	VtePty* pty = vte_pty_new_foreign_sync(anchor.pty_fd, nullptr, nullptr);
	vte_terminal_set_pty(terminal, pty);
	vte_terminal_watch_child(terminal, anchor.pid);
}

}
