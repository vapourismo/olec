#ifndef OLEC_KEYMAP_H
#define OLEC_KEYMAP_H

#include "events.h"

#include <functional>

namespace olec {

using KeyCallback = std::function<bool(KeyModifier, KeySymbol)>;

struct KeyBinding {
	KeyModifier mod;
	KeySymbol key;

	KeyBinding* higher;
	KeyBinding* lower;

	KeyCallback hook;

	inline
	bool invoke() {
		return hook && hook(mod, key);
	}

	inline
	void unbind() {
		hook = nullptr;
	}

	int compare(KeyModifier rhs_mod, KeySymbol rhs_key);

	KeyBinding* find(KeyModifier mod, KeySymbol key);

	KeyBinding* insert(KeyModifier mod, KeySymbol key, KeyCallback hook);

	void merge(KeyBinding* other);

	void overwrite(KeyBinding* other);

	void clear();
};

struct KeyMap {
	KeyBinding* root = nullptr;

	inline
	~KeyMap() {
		clear();
	}

	void bind(KeyModifier mod, KeySymbol key, KeyCallback hook);

	void unbind(KeyModifier mod, KeySymbol key);

	void clear();

	bool invoke(KeyModifier mod, KeySymbol key);
};

}

#endif
