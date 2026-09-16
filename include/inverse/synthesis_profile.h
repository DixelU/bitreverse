#ifndef DIXELU_BITREVERSE_SYNTHESIS_PROFILE_H
#define DIXELU_BITREVERSE_SYNTHESIS_PROFILE_H

#include <array>
#include <chrono>
#include <cstddef>
#include <memory_resource>
#include <string_view>

namespace dixelu::bitreverse::inversion
{
enum class synthesis_phase { forward, relation, witness, compaction, count };

inline constexpr std::string_view synthesis_phase_name(synthesis_phase phase)
{
	constexpr std::array names{"forward", "relation", "witness", "compaction", "none"};
	return names.at(static_cast<size_t>(phase));
}

struct synthesis_phase_statistics
{
	bool started{}, completed{};
	size_t created_nodes{}, operations{}, resident_nodes{};
	// Reachable from completed phase outputs; unavailable if the phase failed.
	// Cached/intermediate nodes are excluded, even if still allocated.
	size_t live_nodes{};
	// Failure snapshots are taken after temporary containers unwind; their
	// phase peak still includes allocations made before the failure.
	size_t tracked_bytes{}, peak_tracked_bytes{};
	std::chrono::nanoseconds elapsed{};
};

struct synthesis_statistics
{
	size_t created_nodes{}, operations{}, relation_nodes{}, function_nodes{};
	std::chrono::nanoseconds elapsed{}, profiling_elapsed{};
	std::array<synthesis_phase_statistics, 4> phases{};
	synthesis_phase failed_phase = synthesis_phase::count;
	// Allocator-requested bytes for the builder's diagrams, caches, and work
	// vectors using the tracking resource. Excludes allocator overhead, source
	// circuit, returned plan, recursion/function control objects, and all
	// reachability/indexing scratch (including the profiler's own scans).
	size_t peak_tracked_bytes{};
};

namespace detail
{
class synthesis_memory_resource final : public std::pmr::memory_resource
{
	size_t current_{}, peak_{}, phase_peak_{};
	void* do_allocate(size_t bytes, size_t alignment) override
	{
		void* result = std::pmr::new_delete_resource()->allocate(bytes, alignment);
		current_ += bytes;
		if (current_ > peak_) peak_ = current_;
		if (current_ > phase_peak_) phase_peak_ = current_;
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
	size_t current() const { return current_; }
	size_t peak() const { return peak_; }
	size_t phase_peak() const { return phase_peak_; }
	void begin_phase() { phase_peak_ = current_; }
};
}
}
#endif
