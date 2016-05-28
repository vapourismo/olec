#include "common.hpp"
#include "clip.hpp"

#include <locale>
#include <iostream>
#include <memory>
#include <string>
#include <set>
#include <termbox.h>
#include <wchar.h>

using namespace olec;

static inline
void setupLocale() {
	const char* lang = std::getenv("LANG");

	if (lang)
		std::setlocale(LC_ALL, lang);
	else
		std::setlocale(LC_ALL, "en_GB.UTF-8");
}

struct Manager {
	Manager(const Manager&) = delete;
	Manager(Manager&&) = delete;
	Manager& operator =(const Manager&) = delete;
	Manager& operator =(Manager&&) = delete;

	inline
	Manager() {
		tb_init();
	}

	inline
	~Manager() {
		tb_shutdown();
	}

	inline
	void present() {
		tb_present();
	}
};

int main() {
	setupLocale();

	Manager mgr;
	Clip root;

	Clip child1 = root.makeChild(10, 10, 10, 5);
	child1.fill(' ', TB_DEFAULT, TB_RED);

	Clip child2 = root.makeChild(12, 12, 10, 5);
	child2.fill(' ', TB_DEFAULT, TB_RED);

	Clip child3 = child1;

	child1.put(0, 0, L"Hello", TB_WHITE, TB_RED);
	child2.put(0, 0, L"Hello", TB_WHITE, TB_RED);
	child3.put(5, 1, L"World", TB_WHITE, TB_RED);

	mgr.present();

	tb_event ev;
	tb_poll_event(&ev);

	return 0;
}
