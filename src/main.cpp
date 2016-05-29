#include "common.hpp"
#include "interface.hpp"

#include <locale>
#include <algorithm>
#include <cmath>
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

struct VSplit: virtual Widget {
	const double ratio;
	std::unique_ptr<Clip> clip;
	std::unique_ptr<Widget> left, right;

	inline
	VSplit(double ratio, std::unique_ptr<Widget>&& left, std::unique_ptr<Widget>&& right):
		ratio(std::max(0.0, std::min(ratio, 1.0))),
		left(std::move(left)),
		right(std::move(right))
	{}

	virtual
	void update(std::unique_ptr<Clip>&& parent) {
		size_t left_width = llround(static_cast<double>(parent->width) * ratio);

		if (left)
			left->update(
				std::make_unique<Clip>(
					*parent,
					0, 0,
					left_width, parent->height
				)
			);

		if (right)
			right->update(
				std::make_unique<Clip>(
					*parent,
					left_width, 0,
					parent->width - left_width, parent->height
				)
			);

		clip = std::move(parent);
	}

	virtual
	void render() {
		if (left) left->render();
		if (right) right->render();
	}
};

struct Filler: virtual Widget {
	wchar_t ch;
	int fg, bg;

	std::unique_ptr<Clip> clip;

	inline
	Filler(wchar_t ch = ' ', int fg = TB_DEFAULT, int bg = TB_DEFAULT):
		ch(ch), fg(fg), bg(bg)
	{}

	virtual
	void update(std::unique_ptr<Clip>&& parent) {
		clip = std::move(parent);
	}

	virtual
	void render() {
		if (clip) clip->fill(ch, fg, bg);
	}
};

int main() {
	setupLocale();

	Manager mgr(
		Widget::create<VSplit>(
			0.5,
			Widget::create<Filler>(L'A', TB_RED, TB_BLACK),
			Widget::create<Filler>(L'B', TB_BLACK, TB_RED)
		)
	);

	mgr.render();
	mgr.pollForever();

	return 0;
}
