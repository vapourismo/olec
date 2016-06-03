#ifndef OLEC_INTERFACE_H_
#define OLEC_INTERFACE_H_

#include "common.hpp"
#include "clip.hpp"
#include "keys.hpp"

#include <memory>
#include <algorithm>
#include <termbox.h>

OLEC_NS_BEGIN

/**
 * Calculate the appropriate color value. Each base color ranges from 0 to 5.
 */
static inline
int generateColor(int r, int g, int b) {
	r = std::min(5, std::max(0, r));
	g = std::min(5, std::max(0, g));
	b = std::min(5, std::max(0, b));

	return 16 + (r * 36 + g * 6 + b);
}

/**
 * Widget interface
 */
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

/**
 * Interface manager
 */
struct Manager {
	// We have to retain an instance of the root clip, to prevent it from invalidating its children
	std::unique_ptr<Clip> clip;

	// Root widget
	std::unique_ptr<Widget> root;

	// Whether to process more events
	bool keep_polling = true;

	// Key map
	KeyMap keys;

	// Currently selected key prefix
	std::shared_ptr<KeyBinding> next;

	inline
	Manager(std::unique_ptr<Widget>&& widget) {
		tb_init();
		tb_select_input_mode(TB_INPUT_ESC | TB_INPUT_MOUSE);
		tb_select_output_mode(TB_OUTPUT_256);

		root = std::move(widget);
		resize(tb_width(), tb_height());
	}

	// Prevent copying and moving
	Manager(const Manager&) = delete;
	Manager(Manager&&) = delete;
	Manager& operator =(const Manager&) = delete;
	Manager& operator =(Manager&&) = delete;

	inline
	~Manager() {
		// Make sure they are destroyed before TermBox shuts down
		if (clip) {
			clip->invalidate();
			clip.reset();
		}

		root.reset();
		tb_shutdown();
	}

	/**
	 * Render widgets to the screen.
	 */
	inline
	void render() {
		if (root) root->render();
		tb_present();
	}

	/**
	 * Resize the screen and rerender widgets.
	 */
	void resize(size_t width, size_t height);

	// Process mouse event
	void mouse(uint16_t key, size_t x, size_t y);

	/**
	 * Poll events until `keep_polling` is set to `false`.
	 */
	void pollForever();
};

OLEC_NS_END

#endif
