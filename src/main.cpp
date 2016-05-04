#include <iostream>
#include "../deps/luwra/lib/luwra.hpp"

int main() {
	luwra::StateWrapper wrapper;
	wrapper.loadStandardLibrary();

	wrapper.runFile("ext/entry.lua");

	return 0;
}
