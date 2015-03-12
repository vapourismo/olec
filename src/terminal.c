#include "terminal.h"
#include "event.h"
#include "child.h"

#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <malloc.h>

static gboolean olec_terminal_key_press(GtkWidget* widget, GdkEventKey* event, const OlecTerminal* term) {
	if (!event->is_modifier) {
		OlecEvent ev = {
			.type = OLEC_KEY_PRESS,
			.info = {
				.key_press = {event->state & GDK_MODIFIER_MASK, event->keyval}
			}
		};

		olec_event_write(term->ipc_fifo, &ev);
	}

	return true;
}

static void olec_terminal_child_exited(VteTerminal* terminal, gint status, OlecTerminal* term) {
	switch (WEXITSTATUS(status)) {
		// Child wants to be reloaded
		case OLEC_CHILD_EXIT_RELOAD:
			close(term->ipc_fifo);
			term->ipc_fifo = -1;

			if (!olec_terminal_spawn(term, NULL)) {
				fputs("Failed to respawn child", stderr);
				gtk_main_quit();
			}

			break;

		// Good or bad doesn't matter, we'll exit
		default:
			printf("Child exited with status %i\n", WEXITSTATUS(status));
			gtk_main_quit();
			break;
	}
}

static
const OlecTerminalConfig olec_default_config = {
	{"#1A1A1A", "#D9715F", "#B2CC46", "#FFCB55", "#6486BC", "#AD7FA8", "#06989A", "#D5D5D5",
	 "#1A1A1A", "#D9715F", "#B2CC46", "#FFCB55", "#6486BC", "#AD7FA8", "#06989A", "#D5D5D5"}
};

bool olec_terminal_init(OlecTerminal* term, const OlecTerminalConfig* config) {
	GtkWidget* window_ = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	GtkWidget* terminal_ = vte_terminal_new();

	if (!window_ || !terminal_)
		return false;

	if (!config)
		config = &olec_default_config;

	term->child_args = NULL;
	term->ipc_fifo = -1;
	term->window = GTK_WINDOW(window_);
	term->terminal = VTE_TERMINAL(terminal_);

	// Set window details
	gtk_window_set_title(term->window, "Olec Text Editor");
	gtk_window_set_wmclass(term->window, "olec", "olec");

	// Connect signals
	g_signal_connect(term->window, "destroy", G_CALLBACK(gtk_main_quit), NULL);
	g_signal_connect(term->terminal, "child-exited", G_CALLBACK(olec_terminal_child_exited), term);
	g_signal_connect(term->window, "key-press-event", G_CALLBACK(olec_terminal_key_press), term);

	// Setup layout
	GtkWidget* box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	gtk_container_add(GTK_CONTAINER(term->window), box);
	gtk_box_pack_start(GTK_BOX(box), GTK_WIDGET(term->terminal), true, true, 0);

	// Configure font
	PangoFontDescription* font = pango_font_description_from_string("Inconsolata 10.5");
	vte_terminal_set_font(term->terminal, font);

	// Configure colors
	GdkRGBA term_palette[16];
	for (size_t i = 0; i < 15; i++)
		gdk_rgba_parse(term_palette + i, config->palette[i] ? config->palette[i] : "#ffffff");

	vte_terminal_set_colors(term->terminal, NULL, NULL, term_palette, 16);

	// vte_terminal_set_color_background(term->terminal, &term_bg_color);
	// vte_terminal_set_color_highlight(term->terminal, &term_bg_color);
	// vte_terminal_set_color_foreground(term->terminal, &term_fg_color);
	// vte_terminal_set_color_highlight_foreground(term->terminal, &term_fg_color);

	// Configure miscellaneous settings
	vte_terminal_set_allow_bold(term->terminal, true);
	vte_terminal_set_encoding(term->terminal, "UTF-8", NULL);
	vte_terminal_set_cursor_shape(term->terminal, VTE_CURSOR_SHAPE_IBEAM);
	vte_terminal_set_scrollback_lines(term->terminal, 0);

	// Setup IPC path
	if ((term->ipc_path = malloc(108)) == NULL)
		return false;

	snprintf(term->ipc_path, 108, "/tmp/olec-%i", getpid());

	// Setup IPC fifo
	if (mkfifo(term->ipc_path, S_IFIFO | S_IWUSR | S_IRUSR) != 0)
		return false;

	return true;
}

void olec_terminal_show(const OlecTerminal* term) {
	gtk_widget_show_all(GTK_WIDGET(term->window));
}

bool olec_terminal_spawn(OlecTerminal* term, char** args) {
	// Environment for child
	char environ_var[strlen(term->ipc_path) + 14];
	environ_var[0] = 0;

	strcat(environ_var, "OLEC_IPC=");
	strcat(environ_var, term->ipc_path);

	char* environment[] = {environ_var, NULL};

	// Setup arguments
	if (args) {
		if (term->child_args) {
			for (char** it = term->child_args; *it; it++)
				free(*it);

			free(term->child_args);
		}

		size_t counter = 1;
		for (char** it = args; *it; it++)
			counter++;

		term->child_args = (char**) malloc(sizeof(char*) * counter);
		for (size_t i = 0; i < counter - 1; i++)
			term->child_args[i] = strdup(args[i]);

		term->child_args[counter - 1] = NULL;
	}

	// Clear terminal screen
	vte_terminal_reset(term->terminal, true, true);

	GPid pid;
	if (!vte_terminal_spawn_sync(term->terminal,
	                             VTE_PTY_DEFAULT,
	                             NULL,
	                             term->child_args,
	                             environment,
	                             G_SPAWN_DEFAULT,
	                             NULL, NULL,
	                             &pid,
	                             NULL, NULL))
		return false;

	vte_terminal_watch_child(term->terminal, pid);

	return (term->ipc_fifo = open(term->ipc_path, O_WRONLY)) >= 0;
}

void olec_terminal_clean(const OlecTerminal* term) {
	// Close IPC channel
	if (term->ipc_fifo >= 0)
		close(term->ipc_fifo);

	// Remove FIFO
	if (term->ipc_path) {
		unlink(term->ipc_path);
		free(term->ipc_path);
	}
}
