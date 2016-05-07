#include <cstdlib>
#include <unistd.h>

#include "util.hpp"

OLEC_NS_BEGIN

size_t wcharWidth(wchar_t ch) {
	int w = wcwidth(ch);
	return w < 1 ? 1 : w;
}

size_t stringWidth(const char* string) {
	size_t string_len = strlen(string);

	if (string_len == 0)
		return 0;

	int step;
	wchar_t ch;
	size_t width = 0;

	while (*string != 0 && (step = mbtowc(&ch, string, string_len)) > 0)
		width += wcharWidth(width);

	return width;
}

void registerUtil(luwra::State* state) {
	luwra::setGlobal(state, "Util", luwra::FieldVector {
		{"wcharWidth",  LUWRA_WRAP(wcharWidth)},
		{"stringWidth", LUWRA_WRAP(stringWidth)},
		{"sleep",       LUWRA_WRAP(sleep)}
	});
}

OLEC_NS_END
