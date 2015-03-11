#include "terminal.h"

#include <stdio.h>
#include <signal.h>
#include <sys/wait.h>

static gboolean olec_terminal_key_press(GtkWidget* widget, GdkEventKey* event, const OlecTerminal* term) {
	if (!event->is_modifier) {
		// if ((event->state & GDK_MODIFIER_MASK) == GDK_CONTROL_MASK && event->keyval == 'q') {
		// 	gtk_main_quit();
		// }
	}

	return true;
}

bool olec_terminal_init(OlecTerminal* term) {
	GtkWidget* window_ = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	GtkWidget* terminal_ = vte_terminal_new();

	if (!window_ || !terminal_) {
		term->window = NULL;
		term->terminal = NULL;
		return false;
	}

	term->window = GTK_WINDOW(window_);
	term->terminal = VTE_TERMINAL(terminal_);

	// Set window details
	gtk_window_set_title(term->window, "Olec Text Editor");
	gtk_window_set_wmclass(term->window, "olec", "olec");

	// Connect signals
	g_signal_connect(term->window, "destroy", G_CALLBACK(gtk_main_quit), NULL);
	g_signal_connect(term->terminal, "child-exited", G_CALLBACK(gtk_main_quit), NULL);
	g_signal_connect(term->window, "key-press-event", G_CALLBACK(olec_terminal_key_press), term);

	// Setup layout
	GtkWidget* box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	gtk_container_add(GTK_CONTAINER(term->window), box);
	gtk_box_pack_start(GTK_BOX(box), GTK_WIDGET(term->terminal), true, true, 0);

	// Configure font
	PangoFontDescription* font = pango_font_description_from_string("Inconsolata 10.5");
	vte_terminal_set_font(term->terminal, font);

	// Configure background color
	GdkRGBA term_bg_color;
	gdk_rgba_parse(&term_bg_color, "#1a1a1a");
	vte_terminal_set_color_background(term->terminal, &term_bg_color);
	vte_terminal_set_color_highlight(term->terminal, &term_bg_color);

	// Configure foreground color
	GdkRGBA term_fg_color;
	gdk_rgba_parse(&term_fg_color, "#d5d5d5");
	vte_terminal_set_color_foreground(term->terminal, &term_fg_color);
	vte_terminal_set_color_highlight_foreground(term->terminal, &term_fg_color);

	// Configure miscellaneous settings
	vte_terminal_set_allow_bold(term->terminal, true);
	vte_terminal_set_encoding(term->terminal, "UTF-8", NULL);
	vte_terminal_set_cursor_shape(term->terminal, VTE_CURSOR_SHAPE_IBEAM);
	vte_terminal_set_scrollback_lines(term->terminal, 0);

	// Clear pid field
	term->child_pid = -1;

	return true;
}

void olec_terminal_show(const OlecTerminal* term) {
	gtk_widget_show_all(GTK_WIDGET(term->window));
}

bool olec_terminal_spawn(OlecTerminal* term, char** args, char** env) {
	return vte_terminal_spawn_sync(term->terminal,
	                               VTE_PTY_DEFAULT,
	                               NULL,
	                               args,
	                               env,
	                               G_SPAWN_DEFAULT,
	                               NULL, NULL,
	                               &term->child_pid,
	                               NULL, NULL);
}

int olec_terminal_terminate(OlecTerminal* term) {
	if (term->child_pid < 0)
		return -1;

	kill(term->child_pid, SIGTERM);

	int result = 0;
	waitpid(term->child_pid, &result, 0);

	term->child_pid = -1;

	return result;
}
