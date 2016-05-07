#include <unistd.h>
#include <locale>

#include "common.hpp"

void setupLocale() {
	const char* lang = std::getenv("LANG");

	if (lang)
		std::setlocale(LC_ALL, lang);
	else
		std::setlocale(LC_ALL, "en_GB.UTF-8");
}

int main() {
	setupLocale();

	olec_log_debug("Hello");
	olec_log_info("World");

	return 0;
}
