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

	if (wrapper.runFile("ext/entry.lua") != LUA_OK)
		std::cerr << luwra::read<std::string>(wrapper, -1) << std::endl;

	return 0;
}
