#include <cstring>
#include <termbox.h>

#include "termbox.hpp"
#include "util.hpp"

OLEC_NS_BEGIN

// We need this as a 'wrapper' in order to properly retrieve the 'ch' parameter
void tbChangeCell(int x, int y, wchar_t ch, uint16_t fg, uint16_t bg) {
	tb_change_cell(x, y, ch, fg, bg);
}

void tbChangeCells(int x, int y, const char* string, uint16_t fg, uint16_t bg) {
	size_t string_len = strlen(string);

	int step;
	wchar_t ch;

	while (*string != 0 && (step = mbtowc(&ch, string, string_len)) > 0) {
		tb_change_cell(x, y, ch, fg, bg);
		x += wcharWidth(ch);
	}
}

luwra::Pushable tbPollEvent() {
	tb_event event;
	int event_type = tb_poll_event(&event);

	switch (event_type) {
		case TB_EVENT_KEY:
			return std::make_tuple(TB_EVENT_KEY, event.mod, event.key, event.ch);

		case TB_EVENT_RESIZE:
			return std::make_tuple(TB_EVENT_RESIZE, event.w, event.h);

		case TB_EVENT_MOUSE:
			return std::make_tuple(TB_EVENT_MOUSE, event.key, event.x, event.y);

		default:
			return nullptr;
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

		{"KeyEvent",        TB_EVENT_KEY},
		{"ResizeEvent",     TB_EVENT_RESIZE},
		{"MouseEvent",      TB_EVENT_MOUSE},

		// Additions
		{"changeCell",      LUWRA_WRAP(tbChangeCell)},
		{"changeCells",     LUWRA_WRAP(tbChangeCells)},
		{"pollEvent",       LUWRA_WRAP(tbPollEvent)},
	});
}

OLEC_NS_END
