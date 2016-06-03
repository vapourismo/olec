#include "keys.hpp"

OLEC_NS_BEGIN

std::shared_ptr<KeyBinding> KeyCallback::onKey(const KeyStroke& stroke) {
	callback(stroke);
	return nullptr;
}

std::shared_ptr<KeyBinding> KeyMap::onKey(const KeyStroke& stroke) {
	if (auto& b = bindings[stroke])
		return b->isPrefix ? b : b->onKey(stroke);

	return nullptr;
}

std::shared_ptr<KeyMap> KeyMap::bindPrefix(const KeyStroke& stroke) {
	auto m = std::make_shared<KeyMap>();
	bindings[stroke] = m;
	return m;
}

OLEC_NS_END
