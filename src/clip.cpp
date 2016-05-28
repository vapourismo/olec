#include "clip.hpp"

OLEC_NS_BEGIN

SClip Clip::makeChild(
	size_t x_offset,
	size_t y_offset,
	size_t new_width,
	size_t new_height
) const {
	// Make sure the child's origin is within its parent
	x_offset = std::min(x_offset, width > 0 ? width - 1 : 0);
	y_offset = std::min(y_offset, height > 0 ? height - 1 : 0);

	// Prevent the child clip to exceed its parent's bounds
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
