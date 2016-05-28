#ifndef OLEC_COMMON_HPP_
#define OLEC_COMMON_HPP_

#include <cstddef>

// Namespaces
#define OLEC_NS_BEGIN namespace olec {
#define OLEC_NS_END }

OLEC_NS_BEGIN

namespace internal {
	enum class LogLevel {
		Debug = 'D',
		Info  = 'I',
		Warn  = 'W',
		Error = 'E'
	};

	void logMessage(
		LogLevel    level,
		const char* file,
		size_t      line,
		const char* fmt,
		...
	);
}

// Both must not be defined at the same time
#if defined(NDEBUG) && defined(DEBUG)
	#undef DEBUG
#endif

#ifndef NDEBUG // Debugging enabled
	#define olec_log_debug(...) \
		(::olec::internal::logMessage(::olec::internal::LogLevel::Debug, __FILE__, __LINE__, __VA_ARGS__))
#else
	#define olec_log_debug(...) ((void) 0)
#endif

#define olec_log_info(...) \
	(::olec::internal::logMessage(::olec::internal::LogLevel::Info, __FILE__, __LINE__, __VA_ARGS__))

#define olec_log_warn(...) \
	(::olec::internal::logMessage(::olec::internal::LogLevel::Warn, __FILE__, __LINE__, __VA_ARGS__))

#define olec_log_error(...) \
	(::olec::internal::logMessage(::olec::internal::LogLevel::Error, __FILE__, __LINE__, __VA_ARGS__))

OLEC_NS_END

#endif
