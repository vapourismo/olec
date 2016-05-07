#include <locale>
#include <codecvt>
#include <termbox.h>

#include "termbox.hpp"

OLEC_NS_BEGIN

void tbChangeCells(int x, int y, const char* string, uint16_t fg, uint16_t bg) {
	std::wstring result =
		std::wstring_convert<std::codecvt_utf8<wchar_t>, wchar_t>().from_bytes(string);

	for (wchar_t ch: result) {
		int ch_width = wcwidth(ch);
		tb_change_cell(x, y, ch, fg, bg);

		if (ch_width < 1)
			x++;
		else
			x += ch_width;
	}
}

size_t tbWidthOf(const char* string) {
	std::wstring result =
		std::wstring_convert<std::codecvt_utf8<wchar_t>, wchar_t>().from_bytes(string);

	size_t width = 0;

	for (wchar_t ch: result) {
		int ch_width = wcwidth(ch);

		if (ch_width < 1)
			return width++;
		else
			width += ch_width;
	}

	return width;
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
		{"changeCells",     LUWRA_WRAP(tbChangeCells)},
		{"widthOf",         LUWRA_WRAP(tbWidthOf)}
	});
}

OLEC_NS_END
