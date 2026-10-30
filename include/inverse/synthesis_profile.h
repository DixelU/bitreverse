#ifndef DIXELU_BITREVERSE_SYNTHESIS_PROFILE_H
#define DIXELU_BITREVERSE_SYNTHESIS_PROFILE_H

#include <array>
#include <chrono>
#include <cstddef>
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
	size_t created_nodes{}, operations{};
	std::chrono::nanoseconds elapsed{};
	std::array<synthesis_phase_statistics, 4> phases{};
	synthesis_phase failed_phase = synthesis_phase::count;
	// Allocator-requested bytes for the builder's diagrams, caches, and work
	// vectors using the tracking resource. Excludes allocator overhead, source
	// circuit, returned plan, recursion frames, and all reachability/indexing
	// scratch (including the profiler's own scans).
	size_t peak_tracked_bytes{};
};
}
#endif
