#ifndef OLEC_UTILS_VALIDITY_H_
#define OLEC_UTILS_VALIDITY_H_

#include "../common.hpp"

#include <memory>
#include <set>
#include <atomic>

OLEC_NS_BEGIN

struct Validity;

/**
 * Reference to a `Validity` instance
 */
using SValidity = std::shared_ptr<Validity>;

/**
 * Weak reference to a `Validity` instance
 */
using WValidity = std::weak_ptr<Validity>;

OLEC_NS_END

namespace std {
	// This implementation is needed for the `set` instance to work, which `Validity` uses to track
	// its children.
	template <>
	struct less<olec::WValidity> {
		inline
		bool operator ()(
			const olec::WValidity& wlhs,
			const olec::WValidity& wrhs
		) const {
			auto slhs = wlhs.lock();
			auto srhs = wrhs.lock();

			return (slhs ? slhs.get() : nullptr) < (srhs ? srhs.get() : nullptr);
		}
	};
}

OLEC_NS_BEGIN

/**
 * \brief Member of validation network
 *
 * This class can be used to construct a tree whose members hold information about the validity of
 * something. Each member can be invalidated which marks the instance on which the invalidation has
 * been called on and its children as invalid. The parent of said instance is not affected by the
 * invalidation.
 */
struct Validity {
private:

	// Is it still valid?
	std::atomic<bool> valid;

	// Tracks the children of this node
	std::set<WValidity> children;

	inline
	Validity(bool predefined): valid(predefined) {}

public:
	static inline
	SValidity create() {
		return SValidity(new Validity);
	}

	inline
	Validity(): valid(true) {}

	// Forbid copying and moving instances of this type
	Validity(const Validity&) = delete;
	Validity& operator =(const Validity&) = delete;
	Validity(Validity&&) = delete;
	Validity& operator =(Validity&&) = delete;

	/**
	 * Destruction invalidates the node.
	 */
	inline
	~Validity() {
		invalidate();
	}

	/**
	 * Return whether this node is still valid.
	 */
	inline
	bool isValid() const {
		return valid.load(std::memory_order_acquire);
	}

	/**
	 * Invalidate this node and its children.
	 */
	void invalidate();

	/**
	 * Attach a child node to this node.
	 */
	SValidity makeChild();
};

OLEC_NS_END

#endif
