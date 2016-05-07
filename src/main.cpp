#include <iostream>
#include <unistd.h>
#include <termbox.h>

#include "../deps/luwra/lib/luwra.hpp"
#include "util.hpp"
#include "termbox.hpp"

void setupLocale() {
	const char* lang = std::getenv("LANG");

	if (lang)
		std::setlocale(LC_ALL, lang);
	else
		std::setlocale(LC_ALL, "en_GB.UTF-8");
}

int main() {
	setupLocale();

	luwra::StateWrapper wrapper;
	wrapper.loadStandardLibrary();

	olec::registerUtil(wrapper);
	olec::registerTermBox(wrapper);

	olec::logString(olec::LOG_INFO, "Starting Lua entry point");
	if (wrapper.runFile("ext/entry.lua") != LUA_OK) {
		if (tb_width() > -1)
			tb_shutdown();

		olec::logString(olec::LOG_ERROR, luwra::read<const char*>(wrapper, -1));
		return 1;
	}

	if (tb_width() > -1)
		tb_shutdown();

	return 0;
}
