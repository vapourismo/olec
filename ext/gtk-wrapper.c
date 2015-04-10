#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include <gtk/gtk.h>
#include <vte/vte.h>
#include <pty.h>

static
gboolean cb_key_press(GtkWidget* widget, GdkEventKey* event, void* self) {
    return true;
}

extern
int olec_begin(const char* font_descr, const char** color_palette, int num_colors) {
    setenv("TERM", "xterm-256color", true);
    gtk_init(NULL, NULL);

    int remote_pty;
    pid_t remote_pid = forkpty(&remote_pty, NULL, NULL, NULL);

    if (remote_pid <= 0)
        return remote_pid;

    // Initialize
    GtkWindow* window = GTK_WINDOW(gtk_window_new(GTK_WINDOW_TOPLEVEL));
    VteTerminal* terminal = VTE_TERMINAL(vte_terminal_new());

    assert(window != NULL);
    assert(terminal != NULL);

    // Set window details
    gtk_window_set_title(window, "Olec Text Editor");
    gtk_window_set_wmclass(window, "olec", "olec");

    // Connect signals
    g_signal_connect(window, "destroy", G_CALLBACK(gtk_main_quit), NULL);
    g_signal_connect(terminal, "child-exited", G_CALLBACK(gtk_main_quit), NULL);
    g_signal_connect(window, "key-press-event", G_CALLBACK(cb_key_press), NULL);

    // Setup layout
    GtkWidget* box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
    gtk_container_add(GTK_CONTAINER(window), box);
    gtk_box_pack_start(GTK_BOX(box), GTK_WIDGET(terminal), true, true, 0);

    // Configure font
    PangoFontDescription* font = pango_font_description_from_string(font_descr);
    vte_terminal_set_font(terminal, font);

    // Configure colors
    GdkRGBA term_palette[num_colors];

    for (int i = 0; i < num_colors; i++)
        gdk_rgba_parse(term_palette + i, color_palette[i] ? color_palette[i] : "#ffffff");

    vte_terminal_set_colors(terminal, NULL, NULL, term_palette, num_colors);

    // Configure miscellaneous settings
    vte_terminal_set_allow_bold(terminal, true);
    vte_terminal_set_encoding(terminal, "UTF-8", NULL);
    vte_terminal_set_cursor_shape(terminal, VTE_CURSOR_SHAPE_IBEAM);
    vte_terminal_set_cursor_blink_mode(terminal, VTE_CURSOR_BLINK_OFF);
    vte_terminal_set_scrollback_lines(terminal, 0);

    // Set PTY target
    VtePty* pty = vte_pty_new_foreign_sync(remote_pty, NULL, NULL);
    vte_terminal_set_pty(terminal, pty);
    vte_terminal_watch_child(terminal, remote_pid);

    // Display window
    gtk_widget_show_all(GTK_WIDGET(window));

    // Run main
    gtk_main();
    return remote_pid;
}
