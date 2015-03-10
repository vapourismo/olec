#include "terminal.h"

#include <stdio.h>

static gboolean olec_terminal_key_press(GtkWidget* widget, GdkEventKey* event, const OlecTerminal* term) {
	if (!event->is_modifier) {
		if ((event->state & GDK_MODIFIER_MASK) == GDK_CONTROL_MASK && event->keyval == 'q') {
			gtk_main_quit();
		}
	}

	// Capture all
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

	return true;
}

void olec_terminal_show(const OlecTerminal* term) {
	gtk_widget_show_all(GTK_WIDGET(term->window));
}

bool olec_terminal_spawn(const OlecTerminal* term, char** args, char** env) {
	return vte_terminal_spawn_sync(term->terminal,
	                               VTE_PTY_DEFAULT,
	                               NULL,
	                               args,
	                               env,
	                               G_SPAWN_DEFAULT,
	                               NULL, NULL,
	                               NULL,
	                               NULL, NULL);
}
