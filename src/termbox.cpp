#include <cstring>
#include <termbox.h>

#include "termbox.hpp"
#include "util.hpp"

OLEC_NS_BEGIN

void tbChangeCells(int x, int y, const char* string, uint16_t fg, uint16_t bg) {
	size_t string_len = strlen(string);

	int step;
	wchar_t ch;

	while (*string != 0 && (step = mbtowc(&ch, string, string_len)) > 0) {
		tb_change_cell(x, y, ch, fg, bg);
		x += wcharWidth(ch);
	}
}

void registerTermBox(luwra::State* state) {
	luwra::setGlobal(state, "TermBox", luwra::FieldVector {
		// Functions
		{"init",            LUWRA_WRAP(tb_init)},
		{"shutdown",        LUWRA_WRAP(tb_shutdown)},
		{"clear",           LUWRA_WRAP(tb_clear)},
		{"present",         LUWRA_WRAP(tb_present)},
		{"setCursor",       LUWRA_WRAP(tb_set_cursor)},
		{"width",           LUWRA_WRAP(tb_width)},
		{"height",          LUWRA_WRAP(tb_height)},
		{"changeCell",      LUWRA_WRAP(tb_change_cell)},

		// Constants
		{"Default",         TB_DEFAULT},
		{"Black",           TB_BLACK},
		{"Red",             TB_RED},
		{"Green",           TB_GREEN},
		{"Yellow",          TB_YELLOW},
		{"Blue",            TB_BLUE},
		{"Magenta",         TB_MAGENTA},
		{"Cyan",            TB_CYAN},
		{"White",           TB_WHITE},

		{"Bold",            TB_BOLD},
		{"Underline",       TB_UNDERLINE},
		{"Reverse",         TB_REVERSE},

		// Additions
		{"changeCells",     LUWRA_WRAP(tbChangeCells)}
	});
}

OLEC_NS_END
