#include "interface.hpp"

OLEC_NS_BEGIN

void Manager::resize(size_t width, size_t height) {
	if (clip) {
		// Not an actual resize
		if (clip->width == width && clip->height == height)
			return;

		// All existing clips shall be invalidated
		clip->invalidate();
	}

	clip = std::make_unique<Clip>();

	// Assign a clip to the root widget if it exists
	if (root)
		root->update(std::make_unique<Clip>(*clip, 0, 0, width, height));
}

// Process key event
void Manager::key(uint8_t mod, uint16_t key, uint32_t ch) {
	olec_log_debug("key: mod = %i, key = %i, ch = %i", mod, key, ch);

	if (mod == 0 && key == TB_KEY_CTRL_Q)
		keep_polling = false;
}

// Process mouse event
void Manager::mouse(uint16_t key, size_t x, size_t y) {
	olec_log_debug("mouse: key = %i, x = %zu, y = %zu", key, x, y);
}

/**
 * Poll events until `keep_polling` is set to `false`.
 */
void Manager::pollForever() {
	keep_polling = true;
	while (keep_polling) {
		tb_event ev;

		switch (tb_poll_event(&ev)) {
			case TB_EVENT_RESIZE:
				// We must call `tb_present` here to make TermBox' internal buffer resize to
				// the proper dimension
				tb_present();

				resize(ev.w, ev.h);
				render();

				break;

			case TB_EVENT_KEY:
				key(ev.mod, ev.key, ev.ch);
				break;

			case TB_EVENT_MOUSE:
				if (ev.x >= 0 && ev.y >= 0)
					mouse(ev.key, ev.x, ev.y);

				break;

			default: break;
		}
	}
}

OLEC_NS_END
