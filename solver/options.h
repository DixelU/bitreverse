#ifndef DIXELU_BITREVERSE_SOLVER_OPTIONS_H
#define DIXELU_BITREVERSE_SOLVER_OPTIONS_H

#include <cstddef>

namespace dixelu::bitreverse
{

struct solver_options
{
	// Native GF(2) propagation for XOR/NOT regions.
	bool affine_reasoning{true};
	std::size_t max_affine_atoms{4096};

	// Select conflict-driven clause learning instead of chronological DPLL.
	// CDCL currently requires affine reasoning to be inactive because affine
	// implications do not yet carry clauses suitable for conflict analysis.
	bool conflict_learning{false};

	// Bound first-UIP resolution work per conflict. When the bound is reached,
	// the solver falls back to chronological branch handling for that
	// conflict. Zero keeps conflict analysis unlimited for pure-CDCL
	// comparisons.
	std::size_t max_conflict_analysis_nodes{512};
};

} // namespace dixelu::bitreverse

#endif // DIXELU_BITREVERSE_SOLVER_OPTIONS_H
