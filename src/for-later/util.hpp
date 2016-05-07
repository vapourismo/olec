#ifndef OLEC_UTIL_HPP_
#define OLEC_UTIL_HPP_

#include <cstring>
#include <wchar.h>

#include "../deps/luwra/lib/luwra.hpp"
#include "common.hpp"

LUWRA_NS_BEGIN

template <>
struct Value<wchar_t> {
	static inline
	wchar_t read(State* state, int index) {
		int type = lua_type(state, index);

		if (type == LUA_TSTRING) {
			const char* value = lua_tostring(state, index);
			size_t value_len = strlen(value);

			wchar_t output;
			if (value_len == 0 || mbtowc(&output, value, value_len) < 1)
				return 0;

			return output;
		} else if (type == LUA_TNUMBER) {
			return lua_tointeger(state, index);
		} else {
			luaL_argerror(state, index, "Expected string or number");
			return 0; // Unreachable
		}
	}

	static inline
	size_t push(State* state, wchar_t value) {
		luwra::push<size_t>(state, value);
		return 1;
	}
};

LUWRA_NS_END

OLEC_NS_BEGIN

/**
 * Retrieve the number of columns needed to display the given character.
 */
size_t wcharWidth(wchar_t ch);

/**
 * Retrieve the number of columns need to display the given UTF-8 encoded string.
 */
size_t stringWidth(const char* string);

enum LogLevel {
	LOG_DEBUG, LOG_INFO, LOG_WARNING, LOG_ERROR
};

/**
 * Write a message to the log file.
 */
void logString(LogLevel level, std::string message);

/**
 * Register the 'Util' table.
 */
void registerUtil(luwra::State* state);

OLEC_NS_END

#endif
