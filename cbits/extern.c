#include <vte/vte.h>
#include <pango/pango.h>
#include <stdbool.h>

extern
VteTerminal* olec_make_vte(int ptm) {
	VteTerminal* term = VTE_TERMINAL(vte_terminal_new());

	// Configure font
	PangoFontDescription* font = pango_font_description_from_string("Inconsolata 10.5");
	vte_terminal_set_font(term, font);

	// // Configure colors
	// GdkRGBA term_palette[num_colors];

	// for (int i = 0; i < num_colors; i++)
	//     gdk_rgba_parse(term_palette + i, color_palette[i] ? color_palette[i] : "#ffffff");

	// vte_terminal_set_colors(term, NULL, NULL, term_palette, num_colors);

	// Configure miscellaneous settings
	vte_terminal_set_allow_bold(term, true);
	vte_terminal_set_encoding(term, "UTF-8", NULL);
	vte_terminal_set_cursor_shape(term, VTE_CURSOR_SHAPE_IBEAM);
	vte_terminal_set_cursor_blink_mode(term, VTE_CURSOR_BLINK_OFF);
	vte_terminal_set_scrollback_lines(term, 0);

	// Set PTY target
	VtePty* pty = vte_pty_new_foreign_sync(ptm, NULL, NULL);
	vte_terminal_set_pty(term, pty);

	return term;
}
