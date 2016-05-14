#include "common.hpp"
#include "threading.hpp"

#include <locale>
#include <iostream>
#include <thread>
#include <unistd.h>

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

	olec::Channel<int> chan;

	olec::WorkerPool producers(4, [&chan]() {
		for (size_t i = 0; i < 1000000; i++)
			chan.write(i);
	});

	olec::Worker consumer([&chan]() {
		std::list<int> sink;
		while (sink.size() < 4000000) {
			int item = chan.read();

			std::cout << item << std::endl;
			sink.push_back(item);
		}
	});

	return 0;
}
