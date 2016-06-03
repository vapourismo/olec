#ifndef OLEC_KEYS_H_
#define OLEC_KEYS_H_

#include "common.hpp"

#include <cstdint>
#include <memory>
#include <map>

OLEC_NS_BEGIN

struct KeyStroke {
	uint8_t mod;
	uint16_t key;
	uint32_t ch;

	inline
	bool operator <(const KeyStroke& other) const {
		if (mod != other.mod)
			return mod < other.mod;

		if (key != other.key)
			return key < other.key;

		return ch < other.ch;
	}
};

struct KeyBinding {
	const bool isPrefix;

	inline
	KeyBinding(bool isPrefix):
		isPrefix(isPrefix)
	{}

	virtual
	std::shared_ptr<KeyBinding> onKey(const KeyStroke& stroke) = 0;
};

/**
 * \brief Simple key binding
 *
 * Simply calls the given `Callable` when invoked.
 */
struct KeyCallback: virtual KeyBinding {
	std::function<void(const KeyStroke&)> callback;

	template <typename T> inline
	KeyCallback(T&& funcobj):
		KeyBinding(false),
		callback(std::forward<T>(funcobj))
	{}

	virtual
	std::shared_ptr<KeyBinding> onKey(const KeyStroke& stroke);
};

/**
 * \brief Key mapping
 *
 * Maps keys to certain actions. This can be used as a key prefix aswell.
 */
struct KeyMap: virtual KeyBinding {
	std::map<KeyStroke, std::shared_ptr<KeyBinding>> bindings;

	inline
	KeyMap():
		KeyBinding(true)
	{}

	virtual
	std::shared_ptr<KeyBinding> onKey(const KeyStroke& stroke);

	/**
	 * Bind the given `Callable` to a key.
	 */
	template <typename T>
	void bind(const KeyStroke& stroke, T&& funcobj) {
		bindings[stroke] = std::make_shared<KeyCallback>(std::forward<T> (funcobj));
	}

	/**
	 * Create a new `KeyMap` and register it as a key prefix.
	 */
	std::shared_ptr<KeyMap> bindPrefix(const KeyStroke& stroke);
};

OLEC_NS_END

#endif
