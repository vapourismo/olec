#include <cstdlib>
#include <iostream>
#include <fstream>
#include <ctime>
#include <iomanip>
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

void logString(LogLevel level, const char* message) {
	static std::ofstream log_stream(".olec.log", std::ios_base::app);

	time_t tm = std::time(nullptr);
	log_stream << std::put_time(std::localtime(&tm), "%F %T") << " ";

	switch (level) {
		case LOG_INFO:
			log_stream << "\x1b[32mI\x1b[0m: ";
			break;

		case LOG_WARNING:
			log_stream << "\x1b[33mW\x1b[0m: ";
			break;

		case LOG_ERROR:
			log_stream << "\x1b[31mE\x1b[0m: ";
			break;

		default:
			log_stream << "\x1b[34mD\x1b[0m: ";
			break;
	}

	log_stream << message << std::endl;
}

void logStringWarning(const char* message) {
	logString(LOG_WARNING, message);
}

void logStringDebug(const char* message) {
	logString(LOG_DEBUG, message);
}

void logStringInfo(const char* message) {
	logString(LOG_INFO, message);
}

void logStringError(const char* message) {
	logString(LOG_ERROR, message);
}

void registerUtil(luwra::State* state) {
	luwra::setGlobal(state, "Util", luwra::FieldVector {
		{"wcharWidth",  LUWRA_WRAP(wcharWidth)},
		{"stringWidth", LUWRA_WRAP(stringWidth)},
		{"sleep",       LUWRA_WRAP(sleep)},
		{"inform",      LUWRA_WRAP(logStringInfo)},
		{"warn",        LUWRA_WRAP(logStringWarning)},
		{"debug",       LUWRA_WRAP(logStringDebug)},
		{"error",       LUWRA_WRAP(logStringError)}
	});
}

OLEC_NS_END
