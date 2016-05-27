#include "validity.hpp"

OLEC_NS_BEGIN

void Validity::invalidate() {
	// No point in invalidating twice
	if (!isValid())
		return;

	// Mark this node as invalid
	valid.store(false, std::memory_order_release);

	// Invalidate children
	for (const auto& wptr: children)
		if (auto sptr = wptr.lock())
			sptr->invalidate();

	// We don't need to track these children anymore
	children.clear();
}

SValidity Validity::makeChild() {
	bool v = isValid();
	SValidity child(new Validity(v));

	// No point in tracking children that start off as invalid
	if (v) children.emplace(child);

	return child;
}

OLEC_NS_END
