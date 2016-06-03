#include "common.hpp"
#include "interface.hpp"
#include "keys.hpp"

#include <locale>
#include <iostream>

using namespace olec;

static inline
void setupLocale() {
	const char* lang = std::getenv("LANG");

	if (lang)
		std::setlocale(LC_ALL, lang);
	else
		std::setlocale(LC_ALL, "en_GB.UTF-8");
}

int main() {
	setupLocale();

	Manager mgr(nullptr);

	mgr.keys.bind({0, TB_KEY_CTRL_Q, 0}, [&](const KeyStroke&) {
		olec_log_debug("Exiting ...");
		mgr.keep_polling = false;
	});

	auto cxMap = mgr.keys.bindPrefix({0, 3, 0});
	cxMap->bind({0, TB_KEY_CTRL_Q, 0}, [&](const KeyStroke&) {
		olec_log_debug("Bye bye");
		mgr.keep_polling = false;
	});

	mgr.render();
	mgr.pollForever();

	return 0;
}
