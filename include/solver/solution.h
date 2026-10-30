#ifndef DIXELU_BITREVERSE_SOLVER_SOLUTION_H
#define DIXELU_BITREVERSE_SOLVER_SOLUTION_H

#include <compare>
#include <functional>
#include <map>
#include <set>

#include "../tracker.h"

namespace dixelu::bitreverse::collision_resolution
{

// One complete assignment of the unknown ('*') leaves that satisfies the
// constraints. Don't-care leaves are enumerated in both states.
struct solution
{
	std::map<const counted_ptr<details::bitstate>, bool> assignments;

	auto operator<=>(const solution&) const = default;
};

using solutions_t = std::set<solution>;

// Receives each solution as it is found; returning false stops the search.
using solution_callback = std::function<bool(const solution&)>;

} // namespace dixelu::bitreverse::collision_resolution

#endif // DIXELU_BITREVERSE_SOLVER_SOLUTION_H
