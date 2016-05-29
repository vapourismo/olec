#include "common.hpp"
#include "interface.hpp"

#include <locale>
#include <algorithm>
#include <cmath>
#include <iostream>
#include <string>
#include <termbox.h>
#include <wchar.h>
#include <cstdlib>

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

struct RandomizedFiller: virtual Widget {
	std::unique_ptr<Clip> clip;

	virtual
	void update(std::unique_ptr<Clip>&& parent) {
		clip = std::move(parent);
	}

	int randomColorSegment() {
		double f = static_cast<double>(std::rand()) / static_cast<double>(RAND_MAX);
		return lround(f * 5.0);
	}

	int randomColor() {
		return generateColor(randomColorSegment(), randomColorSegment(), randomColorSegment());
	}

	virtual
	void render() {
		if (!clip || !clip->isValid())
			return;

		for (size_t x = 0; x < clip->width; x++) {
			for (size_t y = 0; y < clip->height; y++) {
				clip->put(x, y, '+', randomColor(), randomColor());
			}
		}
	}
};

int main() {
	setupLocale();

	Manager mgr(
		Widget::create<RandomizedFiller>()
	);

	mgr.render();
	mgr.pollForever();

	return 0;
}
