#ifndef DIXELU_BITREVERSE_SOLVER_STATISTICS_H
#define DIXELU_BITREVERSE_SOLVER_STATISTICS_H

#include <chrono>
#include <cstddef>

namespace dixelu::bitreverse
{

struct solver_statistics
{
	std::size_t nodes{0};
	std::size_t variables{0};
	std::size_t decisions{0};
	std::size_t propagations{0};
	std::size_t affine_passes{0};
	std::size_t affine_atoms{0};
	std::size_t conflicts{0};
	std::size_t solutions{0};
	std::size_t learned_clauses{0};
	std::size_t backjumps{0};
	std::size_t peak_trail{0};
	bool affine_enabled{false};
	std::chrono::nanoseconds elapsed{};

	void reset()
	{
		*this = {};
	}
};

} // namespace dixelu::bitreverse

#endif // DIXELU_BITREVERSE_SOLVER_STATISTICS_H
