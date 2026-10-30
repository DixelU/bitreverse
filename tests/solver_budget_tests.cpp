// max_search_steps: deterministic accounting, and solver_limit instead of a
// misleading result when the budget runs out.

#include <cstddef>
#include <memory>
#include <utility>
#include <vector>

#include "bitreverse.h"
#include "test_support.h"

namespace br = dixelu::bitreverse;
namespace cr = br::collision_resolution;
using circuit = cr::solver_core::compiled_circuit;
using bindings = std::vector<std::pair<size_t, bool>>;
using test::require;

namespace
{

template<class Function>
void require_limit(Function&& function)
{
	test::require_throws<br::solver_limit>(std::forward<Function>(function), "unfinished search must throw solver_limit");
}

void check_query(const std::shared_ptr<const circuit>& graph,
	const bindings& bound, size_t expected, br::solver_options options)
{
	br::solver_statistics unlimited;
	const auto stop_after_model = [](const cr::solution&) { return false; };
	require(cr::solve_compiled_stream(graph, bound, stop_after_model, options, &unlimited) == expected,
		"unlimited query returned incorrect SAT/UNSAT result");
	require(unlimited.search_steps > 1, "search steps must be recorded in unlimited mode");
	require(unlimited.solutions == expected, "solution statistics disagree with result");

	options.max_search_steps = unlimited.search_steps;
	br::solver_statistics exact;
	require(cr::solve_compiled_stream(graph, bound, stop_after_model, options, &exact) == expected,
		"exact sufficient budget changed SAT/UNSAT result");
	require(exact.search_steps == unlimited.search_steps, "budget units must be deterministic");
	require(cr::solve_compiled_stream(graph, bound, stop_after_model, options, nullptr) == expected,
		"statistics pointer changed a completed query");

	--options.max_search_steps;
	br::solver_statistics unfinished;
	require_limit([&] { (void)cr::solve_compiled_stream(graph, bound, stop_after_model, options, &unfinished); });
	require(unfinished.search_steps <= options.max_search_steps && unfinished.search_steps != 0,
		"interrupted search must preserve its consumed budget");
	require(unfinished.solutions == 0, "underbudget first-model query emitted a result");
	require(unfinished.elapsed.count() > 0, "interrupted search must preserve elapsed time");
	require_limit([&] { (void)cr::solve_compiled_stream(graph, bound, stop_after_model, options, nullptr); });

	options.max_search_steps = 1;
	require_limit([&] { (void)cr::solve_compiled_stream(graph, bound, stop_after_model, options, &unfinished); });
	require(unfinished.search_steps == 1, "each run must reset its budget and statistics");
}

void budgets(br::solver_options options)
{
	const br::bit_tracker a = br::unknown, b = br::unknown;
	std::vector<dixelu::counted_ptr<br::details::bitstate>> roots;
	for (const auto& clause : test::contradiction(a, b))
		roots.push_back(clause.bit_state);
	const auto graph = std::make_shared<const circuit>(roots);
	bindings bound;
	for (size_t i = 0; i < 3; ++i)
		bound.emplace_back(graph->root_ids[i], true);
	check_query(graph, bound, 1, options);
	bound.emplace_back(graph->root_ids[3], true);
	check_query(graph, bound, 0, options);

	// Exercise active Gaussian propagation, including its internal budget ticks.
	if (options.affine_reasoning)
	{
		const auto parity = a ^ b;
		const auto xor_graph = std::make_shared<const circuit>(parity.bit_state);
		check_query(xor_graph, {{xor_graph->root_id, true}}, 1, options);
	}

	// Stream cancellation proves SAT, whereas exhaustion during enumeration
	// must throw even when a previous model has already reached the callback.
	const auto free_graph = std::make_shared<const circuit>(a.bit_state);
	br::solver_statistics first;
	require(cr::solve_compiled_stream(free_graph, {}, [](const auto&) { return false; }, options, &first) == 1,
		"callback cancellation must return its first model");
	options.max_search_steps = first.search_steps;
	size_t callbacks = 0;
	require_limit([&]
	{
		(void)cr::solve_compiled_stream(free_graph, {}, [&](const auto&)
		{
			++callbacks;
			return true;
		}, options);
	});
	require(callbacks == 1, "enumeration budget must preserve already-delivered models");

	// Public root-query dispatch shares the same independently enforced budget.
	options.max_search_steps = 1;
	require_limit([&] { (void)cr::solve(a, true, true, options); });
	require_limit([&] { (void)cr::solve_stream(a, true, [](const auto&) { return false; }, options); });
}

} // namespace

int main()
{
	return test::run("solver_budget", {
		{"dpll", [] { budgets(test::dpll); }},
		{"dpll_affine", [] { budgets(test::dpll_affine); }},
		{"cdcl", [] { budgets(test::cdcl); }},
	});
}
