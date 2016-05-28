#ifndef OLEC_CLIP_H_
#define OLEC_CLIP_H_

#include "common.hpp"
#include "utils/validity.hpp"

#include <memory>
#include <termbox.h>

OLEC_NS_BEGIN

/**
 * \brief Area on the screen
 *
 * You can use this class to seperate rendering converns by dividing the provided clip into
 * smaller clips which can be used by different subroutines.
 */
class Clip {
private:
	// Holds the validity of this clip which determines whether this clip may be used or not.
	const SValidity valid;

public:
	// Absolute bounds
	const size_t x, y, width, height;

	/**
	 * Constructs a clip that spans the entire visible area.
	 */
	inline
	Clip():
		valid(Validity::create()), x(0), y(0), width(tb_width()), height(tb_height())
	{}

	/**
	 * Construct a clip as a child to another clip.
	 */
	Clip(const Clip& other, size_t x, size_t y, size_t width, size_t height);

	/**
	 * Construct floating clip.
	 */
	inline
	Clip(size_t x, size_t y, size_t width, size_t height):
		valid(Validity::create()), x(x), y(y), width(width), height(height)
	{}

	/**
	 * Invalidate this clip.
	 */
	inline
	void invalidate() const {
		valid->invalidate();
	}

	/**
	 * Is the clip still valid?
	 */
	inline
	bool isValid() const {
		return valid->isValid();
	}

	/**
	 * Draw a character at the given position using the provided attributes.
	 */
	inline
	void put(
		size_t  x_offset,
		size_t  y_offset,
		wchar_t ch,
		int     fg = TB_DEFAULT,
		int     bg = TB_DEFAULT
	) const {
		if (!valid->isValid() || y >= height || x >= width)
			return;

		tb_change_cell(x + x_offset, y + y_offset, ch, fg, bg);
	}

	/**
	 * Draw a string at the given position using the provided attributes.
	 */
	void put(
		size_t         x_offset,
		size_t         y_offset,
		const wchar_t* str,
		int            fg = TB_DEFAULT,
		int            bg = TB_DEFAULT
	) const;

	/**
	 * Draw a string at the given position using the provided attributes.
	 */
	void put(
		size_t              x_offset,
		size_t              y_offset,
		const std::wstring& str,
		int                 fg = TB_DEFAULT,
		int                 bg = TB_DEFAULT
	) const;

	/**
	 * Fill the entire clip using the given character and attributes.
	 */
	void fill(wchar_t ch = ' ', int fg = TB_DEFAULT, int bg = TB_DEFAULT) const;
};

OLEC_NS_END

#endif
