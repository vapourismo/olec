#include "keymap.h"
#include "event.h"

#include <malloc.h>

typedef struct _OlecKeyMapElement {
	struct _OlecKeyMapElement* lower;
	struct _OlecKeyMapElement* higher;

	OlecKeyModifier mod;
	OlecKeySymbol key;

	void* data;
} OlecKeyMapElement;

static
OlecKeyMapElement* km_element_new(OlecKeyModifier mod, OlecKeySymbol key, void* data) {
	OlecKeyMapElement* elem = malloc(sizeof(OlecKeyMapElement));

	if (elem) {
		elem->lower = elem->higher = NULL;
		elem->mod = mod;
		elem->key = key;
		elem->data = data;
	}

	return elem;
}

/**
 * lhs > rhs: -1
 * lhs = rhs:  0
 * lhs < rhs:  1
 */
static inline
int km_element_compare(OlecKeyModifier lhs_mod, OlecKeySymbol lhs_key,
                       OlecKeyModifier rhs_mod, OlecKeySymbol rhs_key) {
	if (lhs_mod > rhs_mod)
		return -1;
	else if (lhs_mod == rhs_mod)
		return lhs_key > rhs_key ? -1 : lhs_key < rhs_key;
	else
		return 1;
}

static
void km_element_insert(OlecKeyMapElement* elem, OlecKeyModifier mod,
                       OlecKeySymbol key, void* data) {
	switch (km_element_compare(mod, key, elem->mod, elem->key)) {
		// Higher than elem
		case -1:
			if (elem->higher)
				km_element_insert(elem->higher, mod, key, data);
			else
				elem->higher = km_element_new(mod, key, data);

			break;

		// Lower than elem
		case 1:
			if (elem->lower)
				km_element_insert(elem->lower, mod, key, data);
			else
				elem->lower = km_element_new(mod, key, data);
			break;

		// Equal to elem
		default:
			elem->data = data;
			break;
	}
}

static
void km_element_clear(OlecKeyMapElement* elem) {
	if (elem->lower)
		km_element_clear(elem->lower);

	if (elem->higher)
		km_element_clear(elem->higher);

	free(elem);
}

static
OlecKeyMapElement* km_element_find(OlecKeyMapElement* elem,
                                   OlecKeyModifier mod, OlecKeySymbol key) {
	switch (km_element_compare(mod, key, elem->mod, elem->key)) {
		// Higher than elem
		case -1:
			if (elem->higher)
				return km_element_find(elem->higher, mod, key);
			else
				return NULL;

			break;

		// Lower than elem
		case 1:
			if (elem->lower)
				return km_element_find(elem->lower, mod, key);
			else
				return NULL;

		// Equal to elem
		default:
			return elem;
	}
}

void olec_key_map_init(OlecKeyMap* keymap) {
	keymap->root = NULL;
}

void olec_key_map_clear(OlecKeyMap* keymap) {
	if (keymap->root) {
		km_element_clear(keymap->root);
		keymap->root = NULL;
	}
}

void olec_key_map_bind(OlecKeyMap* keymap, OlecKeyModifier mod, OlecKeySymbol key, void* data) {
	if (!keymap->root)
		keymap->root = km_element_new(mod, key, data);
	else
		km_element_insert(keymap->root, mod, key, data);
}

void olec_key_map_unbind(OlecKeyMap* keymap, OlecKeyModifier mod, OlecKeySymbol key) {
	if (!keymap->root)
		return;

	OlecKeyMapElement* elem = km_element_find(keymap->root, mod, key);

	if (elem)
		elem->data = NULL; // #yolo
}

void* olec_key_map_get(const OlecKeyMap* keymap, OlecKeyModifier mod, OlecKeySymbol key) {
	if (!keymap->root)
		return NULL;

	OlecKeyMapElement* elem = km_element_find(keymap->root, mod, key);
	return elem ? elem->data : NULL;
}
