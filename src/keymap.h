#ifndef OLEC_KEYMAP_H
#define OLEC_KEYMAP_H

#include "ipc.h"

namespace olec {

template <typename T>
using KeyCallback = bool (*)(KeyModifier, KeySymbol, T*);

struct KeyBinding {
	KeyModifier mod;
	KeySymbol key;

	KeyBinding* higher;
	KeyBinding* lower;

	KeyCallback<void> hook;
	void* data;

	inline
	bool invoke() {
		return hook && hook(mod, key, data);
	}

	inline
	void unbind() {
		hook = nullptr;
		data = nullptr;
	}

	int compare(KeyModifier rhs_mod, KeySymbol rhs_key);

	KeyBinding* find(KeyModifier mod, KeySymbol key);

	KeyBinding* insert(KeyModifier mod, KeySymbol key, KeyCallback<void> hook, void* data);

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

	void bind(KeyModifier mod, KeySymbol key, KeyCallback<void> hook, void* data);

	template <typename T> inline
	void bind(KeyModifier mod, KeySymbol key, KeyCallback<T> hook, T* data) {
		bind(mod, key, KeyCallback<void>(hook), (void*) data);
	}

	void unbind(KeyModifier mod, KeySymbol key);

	void clear();

	bool invoke(KeyModifier mod, KeySymbol key);
};

}

#endif
