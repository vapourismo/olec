#include <vte/vte.h>
#include <pango/pango.h>
#include <stdbool.h>

extern
VteTerminal* olec_make_vte() {
	VteTerminal* term = VTE_TERMINAL(vte_terminal_new());

	// Configure font
	PangoFontDescription* font = pango_font_description_from_string("Inconsolata 10.5");
	vte_terminal_set_font(term, font);

	// Configure miscellaneous settings
	vte_terminal_set_allow_bold(term, true);
	vte_terminal_set_encoding(term, "UTF-8", NULL);
	vte_terminal_set_cjk_ambiguous_width(term, 1);
	vte_terminal_set_cursor_shape(term, VTE_CURSOR_SHAPE_IBEAM);
	vte_terminal_set_cursor_blink_mode(term, VTE_CURSOR_BLINK_OFF);
	vte_terminal_set_scrollback_lines(term, 0);

	return term;
}
