#ifndef OLEC_UTILS_VALIDITY_H_
#define OLEC_UTILS_VALIDITY_H_

#include "../common.hpp"

#include <memory>
#include <set>
#include <atomic>

OLEC_NS_BEGIN

struct Validity;

/**
 * Handle for a `Validity` instance
 */
struct SValidity: std::shared_ptr<Validity> {
	template <typename... A> inline
	SValidity(A&&... args);
};

OLEC_NS_END

namespace std {
	// This implementation is needed for the `set` instance to work, which `Validity` uses to track
	// its children.
	template <>
	struct less<std::weak_ptr<olec::Validity>> {
		inline
		bool operator ()(
			const std::weak_ptr<olec::Validity>& wlhs,
			const std::weak_ptr<olec::Validity>& wrhs
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
	std::set<std::weak_ptr<Validity>> children;

public:

	inline
	Validity(bool predefined = true): valid(predefined) {}

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
	bool isValid() {
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

template <typename... A> inline
SValidity::SValidity(A&&... args):
	std::shared_ptr<Validity>(new Validity(std::forward<A>(args)...))
{}

OLEC_NS_END

#endif
