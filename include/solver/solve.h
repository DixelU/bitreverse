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
		return cdcl::resolve(
			bit,
			target,
			first_only,
			options,
			statistics);

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
		return cdcl::resolve_stream(
			bit,
			target,
			std::move(on_solution),
			options,
			statistics);

	return dpll::resolve_stream(
		bit,
		target,
		std::move(on_solution),
		options,
		statistics);
}

// Reuse compiled adjacency with fresh assignments and search state. Only
// bindings constrain node values: the first root has no implicit target.
// All retained unknowns, including otherwise unused input roots, are
// enumerated. Returning false from the callback stops after that model.
// An unsatisfiable query returns zero; invalid node IDs throw. Exhausting
// max_search_steps throws solver_limit, never an incomplete zero count.
inline size_t solve_compiled_stream(
	std::shared_ptr<const solver_core::compiled_circuit> circuit,
	const std::vector<std::pair<solver_core::node_id, bool>>& bindings,
	solution_callback on_solution,
	const solver_options& options = {},
	solver_statistics* statistics = nullptr)
{
	solver_core::validate_bindings(
		solver_core::require_compiled_circuit(circuit), bindings);
	if (options.conflict_learning)
	{
		cdcl::engine solver(
			std::move(circuit), bindings, false, false,
			std::move(on_solution), options, statistics);
		(void)solver.run();
		return solver.solution_count;
	}

	dpll::engine solver(
		std::move(circuit), bindings, false, false,
		std::move(on_solution), options, statistics);
	(void)solver.run();
	return solver.solution_count;
}

#endif // DIXELU_BITREVERSE_SOLVER_SOLVE_H
