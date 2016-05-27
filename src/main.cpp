#include "common.hpp"
#include "utils/validity.hpp"

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

class Clip;

using SClip = std::shared_ptr<Clip>;

class Clip {
private:
	SValidity valid;

	inline
	Clip(SValidity&& valid, size_t x, size_t y, size_t width, size_t height):
		valid(valid), x(x), y(y), width(width), height(height)
	{}

public:
	const size_t x, y, width, height;

	inline
	Clip():
		valid(Validity::create()),
		x(0),
		y(0),
		width(tb_width()),
		height(tb_height())
	{}

	Clip(const Clip&) = delete;
	Clip(Clip&&) = delete;
	Clip& operator =(const Clip&) = delete;
	Clip& operator =(Clip&&) = delete;

	inline
	void invalidate() {
		valid->invalidate();
	}

	inline
	bool isValid() {
		return valid->isValid();
	}

	SClip makeChild(size_t x_offset, size_t y_offset, size_t new_width, size_t new_height) {
		x_offset = std::min(x_offset, width > 0 ? width - 1 : 0);
		y_offset = std::min(y_offset, height > 0 ? height - 1 : 0);

		new_width = std::min(new_width, width - x_offset);
		new_height = std::min(new_height, height - y_offset);

		return std::shared_ptr<Clip>(
			new Clip(
				valid->makeChild(),
				x + x_offset,
				y + y_offset,
				new_width,
				new_height
			)
		);
	}

	inline
	void put(size_t x, size_t y, wchar_t ch, int fg = TB_DEFAULT, int bg = TB_DEFAULT) const {
		if (!valid->isValid() || y >= height || x >= width)
			return;

		tb_change_cell(x, y, ch, fg, bg);
	}

	void put(size_t x_offset, size_t y_offset, const wchar_t* str, int fg = TB_DEFAULT, int bg = TB_DEFAULT) const {
		if (!valid->isValid() || y_offset >= height || x_offset >= width)
			return;

		size_t y_target = y + y_offset;
		size_t x_target = x + x_offset;
		size_t x_outside = x + width;

		while (*str != 0 && x_target < x_outside) {
			wchar_t ch = *str++;
			tb_change_cell(x_target, y_target, ch, fg, bg);

			int w = wcwidth(ch);
			x_target += w > 0 ? w : 0;
		}
	}

	void put(size_t x_offset, size_t y_offset, const std::wstring& str, int fg = TB_DEFAULT, int bg = TB_DEFAULT) const {
		if (!valid->isValid() || y_offset >= height || x_offset >= width)
			return;

		size_t y_target = y + y_offset;
		size_t x_target = x + x_offset;
		size_t x_outside = x + width;

		for (wchar_t ch: str) {
			if (x_target >= x_outside)
				break;

			tb_change_cell(x_target, y_target, ch, fg, bg);

			int w = wcwidth(ch);
			x_target += w > 0 ? w : 0;
		}
	}

	void fill(wchar_t ch = ' ', int fg = TB_DEFAULT, int bg = TB_DEFAULT) {
		if (!valid->isValid())
			return;

		size_t x_outside = x + width;
		size_t y_outside = y + height;

		int w = wcwidth(ch);
		size_t step = w > 0 ? w : 1;

		for (size_t xi = x; xi < x_outside; xi += step)
			for (size_t yi = y; yi < y_outside; yi++)
				tb_change_cell(xi, yi, ch, fg, bg);
	}
};

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

	SClip child1 = root.makeChild(10, 10, 10, 5);
	child1->fill(' ', TB_DEFAULT, TB_RED);

	SClip child2 = root.makeChild(12, 12, 10, 5);
	child2->fill(' ', TB_DEFAULT, TB_RED);

	child1->invalidate();

	olec_log_debug("child1->isValid() = %i", child1->isValid());
	olec_log_debug("child2->isValid() = %i", child2->isValid());

	child1->put(0, 0, L"Hello", TB_WHITE, TB_RED);
	child2->put(0, 0, L"Hello", TB_WHITE, TB_RED);

	mgr.present();

	tb_event ev;
	tb_poll_event(&ev);

	return 0;
}
