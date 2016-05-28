#include "common.hpp"
#include "clip.hpp"

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

struct Widget {
	template <typename T, typename... A> static inline
	std::unique_ptr<T> create(A&&... args) {
		return std::make_unique<T>(std::forward<A>(args)...);
	}

	virtual
	void update(std::unique_ptr<Clip>&&) {};

	virtual
	void render() {};
};

struct VSplit: virtual Widget {
	const double ratio;
	std::unique_ptr<Clip> clip;
	std::unique_ptr<Widget> left, right;

	VSplit(double ratio, std::unique_ptr<Widget>&& left, std::unique_ptr<Widget>&& right):
		ratio(std::max(0.0, std::min(ratio, 1.0))),
		left(std::move(left)),
		right(std::move(right))
	{}

	virtual
	void update(std::unique_ptr<Clip>&& parent) {
		olec_log_debug("VSplit: w = %zu, h = %zu", parent->width, parent->height);

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

	Filler(wchar_t ch = ' ', int fg = TB_DEFAULT, int bg = TB_DEFAULT):
		ch(ch), fg(fg), bg(bg)
	{}

	virtual
	void update(std::unique_ptr<Clip>&& parent) {
		olec_log_debug("Filler: w = %zu, h = %zu", parent->width, parent->height);
		clip = std::move(parent);
	}

	virtual
	void render() {
		if (clip) clip->fill(ch, fg, bg);
	}
};

struct Manager {
	Manager(const Manager&) = delete;
	Manager(Manager&&) = delete;
	Manager& operator =(const Manager&) = delete;
	Manager& operator =(Manager&&) = delete;

	std::unique_ptr<Widget> root;

	inline
	Manager(std::unique_ptr<Widget>&& widget) {
		tb_init();
		root = std::move(widget);
		resize(tb_width(), tb_height());
	}

	inline
	~Manager() {
		root.reset();
		tb_shutdown();
	}

	void resize(size_t width, size_t height) {
		if (root)
			root->update(std::make_unique<Clip>(0, 0, width, height));
	}

	virtual
	void render() {
		if (root) root->render();
		tb_present();
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

	while (true) {
		tb_event ev;

		bool b = false;
		switch (tb_poll_event(&ev)) {
			case TB_EVENT_RESIZE:
				olec_log_debug("resize: w = %i, h = %i", ev.w, ev.h);
				mgr.resize(ev.w, ev.h);
				mgr.render();
				break;

			default:
				b = true;
				break;
		}

		if (b) break;
	}

	return 0;
}
