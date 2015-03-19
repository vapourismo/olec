#include "keymap.h"

namespace olec {

/**
 * lhs > rhs: -1
 * lhs = rhs:  0
 * lhs < rhs:  1
 */
int KeyBinding::compare(KeyModifier rhs_mod, KeySymbol rhs_key) {
	if (mod > rhs_mod)
		return -1;
	else if (mod == rhs_mod)
		return key > rhs_key ? -1 : key < rhs_key;
	else
		return 1;
}

KeyBinding* KeyBinding::find(KeyModifier mod, KeySymbol key) {
	switch (compare(mod, key)) {
		// Higher than this
		case 1:
			if (higher)
				return higher->find(mod, key);
			else
				return nullptr;

			break;

		// Lower than this
		case -1:
			if (lower)
				return lower->find(mod, key);
			else
				return nullptr;

		// Equal to this
		default:
			return this;
	}
}

KeyBinding* KeyBinding::insert(KeyModifier m, KeySymbol k, KeyCallback<void> h, void* d) {
	switch (compare(m, k)) {
		// Higher than this
		case 1:
			if (higher)
				return higher->insert(m, k, h, d);
			else
				return (higher = new KeyBinding {
					m, k,
					nullptr, nullptr,
					h, d
				});

		// Lower than this
		case -1:
			if (lower)
				return lower->insert(m, k, h, d);
			else
				return (lower = new KeyBinding {
					m, k,
					nullptr, nullptr,
					h, d
				});

		// Equal to this
		default:
			hook = h;
			data = d;

			return this;
	}
}

void KeyBinding::merge(KeyBinding* other) {
	KeyBinding* inserted = insert(other->mod, other->key, other->hook, other->data);
	inserted->higher = other->higher;
	inserted->lower = other->lower;

	delete other;
}

void KeyBinding::overwrite(KeyBinding* other) {
	*this = *other;
	delete other;
}

void KeyBinding::clear() {
	if (lower) {
		lower->clear();
		delete lower;
		lower = nullptr;
	}

	if (higher) {
		higher->clear();
		delete higher;
		higher = nullptr;
	}
}

void KeyMap::bind(KeyModifier mod, KeySymbol key, KeyCallback<void> hook, void* data) {
	if (!root)
		root = new KeyBinding {
			mod, key,
			nullptr, nullptr,
			hook, data
		};
	else
		root->insert(mod, key, hook, data);
}

void KeyMap::unbind(KeyModifier mod, KeySymbol key) {
	if (!root)
		return;

	KeyBinding* kb = root->find(mod, key);

	if (!kb)
		return;

	if (kb->lower && kb->higher) {
		kb->higher->merge(kb->lower);
		kb->overwrite(kb->higher);
	} else if (kb->lower) {
		kb->overwrite(kb->lower);
	} else if (kb->higher) {
		kb->overwrite(kb->higher);
	} else {
		kb->hook = nullptr;
		kb->data = nullptr;
	}
}

void KeyMap::clear() {
	if (root) {
		root->clear();
		delete root;
		root = nullptr;
	}
}

}
