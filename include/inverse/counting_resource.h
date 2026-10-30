#ifndef DIXELU_BITREVERSE_INVERSE_COUNTING_RESOURCE_H
#define DIXELU_BITREVERSE_INVERSE_COUNTING_RESOURCE_H

#include <algorithm>
#include <cstddef>
#include <limits>
#include <memory_resource>

namespace dixelu::bitreverse::inversion::detail
{

// Counts the bytes containers request through it, excluding allocator
// overhead. A request beyond the byte limit throws Limit, so containers built
// on this resource cannot outgrow a construction budget.
template<class Limit>
class counting_resource final : public std::pmr::memory_resource
{
	size_t limit_, current_{}, peak_{}, phase_peak_{};

	void* do_allocate(size_t bytes, size_t alignment) override
	{
		if (bytes > limit_ - current_)
			throw Limit("Byte budget exceeded");
		void* result = std::pmr::new_delete_resource()->allocate(bytes, alignment);
		current_ += bytes;
		peak_ = std::max(peak_, current_);
		phase_peak_ = std::max(phase_peak_, current_);
		return result;
	}

	void do_deallocate(void* pointer, size_t bytes, size_t alignment) override
	{
		std::pmr::new_delete_resource()->deallocate(pointer, bytes, alignment);
		current_ -= bytes;
	}

	bool do_is_equal(const std::pmr::memory_resource& other) const noexcept override
	{
		return this == &other;
	}

public:
	explicit counting_resource(size_t limit = std::numeric_limits<size_t>::max()) : limit_(limit) {}

	size_t current() const { return current_; }
	size_t peak() const { return peak_; }
	// The peak since the last begin_phase().
	size_t phase_peak() const { return phase_peak_; }
	void begin_phase() { phase_peak_ = current_; }
};

} // namespace dixelu::bitreverse::inversion::detail

#endif // DIXELU_BITREVERSE_INVERSE_COUNTING_RESOURCE_H
