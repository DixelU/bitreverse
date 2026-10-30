#ifndef DIXELU_BITREVERSE_SOLVER_SOLVE_H
#define DIXELU_BITREVERSE_SOLVER_SOLVE_H

#include <memory>
#include <utility>
#include <vector>

#include "compiled_circuit.h"
#include "options.h"
#include "search/cdcl.h"
#include "search/dpll.h"
#include "search/session.h"
#include "solution.h"
#include "statistics.h"

namespace dixelu::bitreverse::collision_resolution
{
namespace solver_core
{

struct search_result
{
	size_t solution_count{0};
	solutions_t solutions;
};

template<typename Engine>
search_result run_engine(search_request request)
{
	Engine engine(std::move(request));
	engine.run();
	return {engine.solution_count, std::move(engine.solutions)};
}

// The single dispatch point between the search engines.
inline search_result search(search_request request)
{
	if (request.options.conflict_learning)
		return run_engine<cdcl_engine>(std::move(request));
	return run_engine<dpll_engine>(std::move(request));
}

inline search_request request_for(const bit_tracker& bit, bool target)
{
	return {
		.circuit = std::make_shared<const compiled_circuit>(bit.bit_state),
		.bindings = {{0, target}}};
}

} // namespace solver_core

// All complete assignments of the unknowns that drive `bit` to `target`, or
// only the first one found.
inline solutions_t solve(
	const bit_tracker& bit,
	bool target,
	bool first_only = false,
	const solver_options& options = {},
	solver_statistics* statistics = nullptr)
{
	auto request = solver_core::request_for(bit, target);
	request.first_only = first_only;
	request.options = options;
	request.statistics = statistics;
	return solver_core::search(std::move(request)).solutions;
}

// Streams solutions without retaining them; returns how many were found.
inline size_t solve_stream(
	const bit_tracker& bit,
	bool target,
	solution_callback on_solution,
	const solver_options& options = {},
	solver_statistics* statistics = nullptr)
{
	auto request = solver_core::request_for(bit, target);
	request.collect_solutions = false;
	request.on_solution = std::move(on_solution);
	request.options = options;
	request.statistics = statistics;
	return solver_core::search(std::move(request)).solution_count;
}

// Reuse compiled adjacency with fresh assignments and search state. Only
// bindings constrain node values: the first root has no implicit target.
// All retained unknowns, including otherwise unused input roots, are
// enumerated. Returning false from the callback stops after that model.
// An unsatisfiable query returns zero; invalid node IDs throw. Exhausting
// max_search_steps throws solver_limit, never an incomplete zero count.
inline size_t solve_compiled_stream(
	std::shared_ptr<const solver_core::compiled_circuit> circuit,
	std::vector<std::pair<solver_core::node_id, bool>> bindings,
	solution_callback on_solution,
	const solver_options& options = {},
	solver_statistics* statistics = nullptr)
{
	return solver_core::search({
		.circuit = std::move(circuit),
		.bindings = std::move(bindings),
		.collect_solutions = false,
		.on_solution = std::move(on_solution),
		.options = options,
		.statistics = statistics}).solution_count;
}

} // namespace dixelu::bitreverse::collision_resolution

#endif // DIXELU_BITREVERSE_SOLVER_SOLVE_H
