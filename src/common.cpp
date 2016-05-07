#include <cstdio>
#include <ctime>
#include <iomanip>
#include <string>
#include <sstream>

#include "common.hpp"

OLEC_NS_BEGIN

namespace internal {
	std::string makeTimeStamp() {
		std::stringstream stream;
		std::time_t timestamp = std::time(nullptr);

		stream << std::put_time(std::localtime(&timestamp), "%F %T");

		return stream.str();
	}

	static struct _LogStream {
		FILE* file;

		_LogStream(): file(fopen(".olec.log", "a+")) {}

		~_LogStream() {
			if (file) fclose(file);
		}
	} logStream;

	void logMessage(
		LogLevel    level,
		const char* file,
		size_t      line,
		const char* func,
		const char* fmt,
		...
	) {
		auto timestamp = makeTimeStamp();

		flockfile(logStream.file);

		// Preamble
		fprintf(
			logStream.file,
			"%s %c %s:%zu [%s] ",
			timestamp.c_str(),
			char(level),
			file,
			line,
			func
		);

		// Push user-provided message
		va_list args;
		va_start(args, fmt);
		vfprintf(logStream.file, fmt, args);
		va_end(args);

		// Finish up
		fputc('\n', logStream.file);
		fflush(logStream.file);

		funlockfile(logStream.file);
	}
}

OLEC_NS_END
