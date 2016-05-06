#include <iostream>
#include <string>
#include <locale>
#include <codecvt>
#include <algorithm>
#include <wchar.h>

#include "../deps/luwra/lib/luwra.hpp"

struct Text {
	static
	std::wstring from_byte_string(const char* input) {
		auto str = std::wstring_convert<std::codecvt_utf8<wchar_t>, wchar_t>().from_bytes(input);

		// Remove non-printable characters
		std::remove_if(str.begin(), str.end(), [](wchar_t c) {
			return wcwidth(c) < 1;
		});

		return str;
	}

	static
	size_t calculate_width(const std::wstring& string) {
		size_t width = 0;

		for (wchar_t c: string) {
			int w = wcwidth(c);
			if (w > 0) width += w;
		}

		return width;
	}

	const std::wstring contents;
	const size_t width;

	Text(const char* input):
		contents(from_byte_string(input)),
		width(calculate_width(contents))
	{}

	Text(const std::string& other):
		contents(from_byte_string(other.c_str())),
		width(calculate_width(contents))
	{}

	Text(const wchar_t* input):
		contents(input),
		width(calculate_width(contents))
	{}

	Text(const std::wstring& other):
		contents(other),
		width(calculate_width(contents))
	{}

	std::string toString() {
		return std::wstring_convert<std::codecvt_utf8<wchar_t>, wchar_t>().to_bytes(contents);
	}

	size_t length() {
		return contents.length();
	}
};

int main() {
	luwra::StateWrapper wrapper;
	wrapper.loadStandardLibrary();

	wrapper.registerUserType<Text(const char*)>(
		"Text",
		{
			{"width",      LUWRA_WRAP(Text::width)}
		},
		{
			{"__tostring", LUWRA_WRAP(Text::toString)},
			{"__len",      LUWRA_WRAP(Text::width)}
		}
	);

	if (wrapper.runFile("ext/entry.lua") != LUA_OK) {
		std::cerr << luwra::read<std::string>(wrapper, -1) << std::endl;
	}

	return 0;
}
