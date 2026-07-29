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

	// Reserved for the CDCL engine. Enabling it before that engine is linked
	// raises a clear error instead of silently running baseline DPLL.
	bool conflict_learning{false};
};

} // namespace dixelu::bitreverse

#endif // DIXELU_BITREVERSE_SOLVER_OPTIONS_H
