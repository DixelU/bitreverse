#ifndef DIXELU_BITREVERSE_SOLVER_SOLVE_H
#define DIXELU_BITREVERSE_SOLVER_SOLVE_H

// Internal facade: included by bitreverse.h inside
// dixelu::bitreverse::collision_resolution. This is the single dispatch point
// for optional search engines.

using solution_callback =
	std::function<bool(const crs_state&)>;

inline solutions_t solve(
	bit_tracker& bit,
	bool target,
	bool first_only = false,
	const solver_options& options = {},
	solver_statistics* statistics = nullptr)
{
	if (options.conflict_learning)
		throw std::logic_error(
			"Conflict learning engine is not implemented yet");

	return dpll::resolve(
		bit,
		target,
		first_only,
		options,
		statistics);
}

inline size_t solve_stream(
	bit_tracker& bit,
	bool target,
	solution_callback on_solution,
	const solver_options& options = {},
	solver_statistics* statistics = nullptr)
{
	if (options.conflict_learning)
		throw std::logic_error(
			"Conflict learning engine is not implemented yet");

	return dpll::resolve_stream(
		bit,
		target,
		std::move(on_solution),
		options,
		statistics);
}

#endif // DIXELU_BITREVERSE_SOLVER_SOLVE_H
