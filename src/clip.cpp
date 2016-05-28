#include "clip.hpp"

OLEC_NS_BEGIN

Clip::Clip(const Clip& other, size_t x, size_t y, size_t width, size_t height):
	valid(other.valid->makeChild()),

	// Make sure the clip's origin is within its parents bounds
	x(std::min(x, other.width > 0 ? other.width - 1 : 0)),
	y(std::min(y, other.height > 0 ? other.height - 1 : 0)),

	// Prevent the clip from exceeding its parent's bounds
	width(std::min(width, other.width - x)),
	height(std::min(height, other.height - x))
{}

void Clip::put(
	size_t         x_offset,
	size_t         y_offset,
	const wchar_t* str,
	int            fg,
	int            bg
) const {
	if (!valid->isValid() || y_offset >= height || x_offset >= width)
		return;

	// Starting position
	size_t y_target = y + y_offset;
	size_t x_target = x + x_offset;

	// This X-coordinate is outside this clip
	size_t x_outside = x + width;

	while (*str != 0 && x_target < x_outside) {
		wchar_t ch = *str++;
		tb_change_cell(x_target, y_target, ch, fg, bg);

		// To prevent TermBox from messing up the coordinates, we calculate the next position
		int w = wcwidth(ch);
		x_target += w > 0 ? w : 1;
	}
}

void Clip::put(
	size_t              x_offset,
	size_t              y_offset,
	const std::wstring& str,
	int                 fg,
	int                 bg
) const {
	if (!valid->isValid() || y_offset >= height || x_offset >= width)
		return;

	// Starting position
	size_t y_target = y + y_offset;
	size_t x_target = x + x_offset;

	// This X-coordinate is outside this clip
	size_t x_outside = x + width;

	for (wchar_t ch: str) {
		if (x_target >= x_outside)
			break;

		tb_change_cell(x_target, y_target, ch, fg, bg);

		// To prevent TermBox from messing up the coordinates, we calculate the next position
		int w = wcwidth(ch);
		x_target += w > 0 ? w : 1;
	}
}

void Clip::fill(wchar_t ch, int fg, int bg) const {
	if (!valid->isValid())
		return;

	// Theses coordinates are outside the clip
	size_t x_outside = x + width;
	size_t y_outside = y + height;

	// Character width gives us the number of columns we have to skip on the X-axis per character
	int w = wcwidth(ch);
	size_t step = w > 0 ? w : 1;

	for (size_t xi = x; xi < x_outside; xi += step)
		for (size_t yi = y; yi < y_outside; yi++)
			tb_change_cell(xi, yi, ch, fg, bg);
}

OLEC_NS_END
