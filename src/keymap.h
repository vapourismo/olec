#ifndef OLEC_KEYMAP_H
#define OLEC_KEYMAP_H

#include "event.h"

#include <stdbool.h>

/**
 * Key map
 */
typedef struct {
	struct _OlecKeyMapElement* root;
} OlecKeyMap;

/**
 * Key binding callback;
 */
typedef bool (* OlecKeyHook)(void*, OlecKeyModifier, OlecKeySymbol);

/**
 * Initialize key map
 */
void olec_key_map_init(OlecKeyMap* keymap);

/**
 * Remove every key binding
 */
void olec_key_map_clear(OlecKeyMap* keymap);

/**
 * Bind a key to some data
 */
void olec_key_map_bind(OlecKeyMap* keymap,
                       OlecKeyModifier mod, OlecKeySymbol key,
                       OlecKeyHook hook, void* data);
/**
 * Unbind a key
 */
void olec_key_map_unbind(OlecKeyMap* keymap, OlecKeyModifier mod, OlecKeySymbol key);

/**
 * Get the data for a binding
 */
bool olec_key_map_invoke(const OlecKeyMap* keymap, OlecKeyModifier mod, OlecKeySymbol key);

#endif