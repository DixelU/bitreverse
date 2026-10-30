// Model enumeration by the search engines, checked against truth tables.

#include <algorithm>
#include <array>
#include <bit>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <set>
#include <stdexcept>
#include <utility>
#include <vector>

#include "bitreverse.h"
#include "test_support.h"

namespace br = dixelu::bitreverse;
namespace cr = br::collision_resolution;
namespace core = cr::solver_core;
using test::require;
using bindings = std::vector<std::pair<size_t, bool>>;

namespace
{

// The models must be exactly the assignments of `inputs` set in the truth table
// `expected`, each reported once.
void check_models(const std::shared_ptr<const core::compiled_circuit>& graph,
	const std::vector<br::bit_tracker>& inputs, const bindings& bound,
	std::uint64_t expected, const br::solver_options& options)
{
	std::uint64_t actual = 0;
	const auto count = cr::solve_compiled_stream(graph, bound, [&](const cr::solution& model)
	{
		size_t assignment = 0;
		for (size_t bit = 0; bit < inputs.size(); ++bit)
			assignment |= static_cast<size_t>(model.assignments.at(inputs[bit].bit_state)) << bit;
		const auto mask = std::uint64_t{1} << assignment;
		require(!(actual & mask), "search emitted a duplicate model");
		actual |= mask;
		return true;
	}, options);
	require(actual == expected && count == static_cast<size_t>(std::popcount(expected)),
		"solver models disagree with exhaustive truth table");
}

void randomized_truth_tables()
{
	test::random_engine random(0x28367);
	for (size_t trial = 0; trial < 100; ++trial)
	{
		const test::random_circuit circuit(random, 30, true);
		const auto& masks = circuit.masks;
		std::vector<dixelu::counted_ptr<br::details::bitstate>> roots;
		for (size_t i = circuit.expressions.size() - 3; i < circuit.expressions.size(); ++i)
			roots.push_back(circuit.expressions[i].bit_state);
		for (const auto& input : circuit.inputs)
			roots.push_back(input.bit_state);
		const auto graph = std::make_shared<const core::compiled_circuit>(std::move(roots));
		for (unsigned target = 0; target < 8; ++target)
		{
			bindings bound;
			auto expected = ~std::uint64_t{0};
			for (size_t bit = 0; bit < 3; ++bit)
			{
				const bool value = (target >> bit) & 1;
				bound.emplace_back(graph->root_ids[bit], value);
				const auto mask = masks[masks.size() - 3 + bit];
				expected &= value ? mask : ~mask;
			}
			for (const auto& options : test::search_engines)
				check_models(graph, circuit.inputs, bound, expected, options);
		}
	}
}

void wide_affine()
{
	test::random_engine random(0x17289);
	const auto inputs = test::unknown_bits(130);
	const std::array<size_t, 6> free_indices{0, 1, 63, 64, 127, 129};
	std::vector<br::bit_tracker> free_inputs;
	for (const auto id : free_indices)
		free_inputs.push_back(inputs[id]);
	std::vector<dixelu::counted_ptr<br::details::bitstate>> roots;
	std::array<std::uint64_t, 8> masks{};
	for (size_t row = 0; row < masks.size(); ++row)
	{
		br::bit_tracker expression(false);
		for (size_t bit = 0; bit < inputs.size(); ++bit)
		{
			if (!(random() & 1))
				continue;
			expression ^= inputs[bit];
			const auto found = std::ranges::find(free_indices, bit);
			masks[row] ^= found != free_indices.end() ? test::input_mask(found - free_indices.begin())
				: (bit % 3 == 0 ? ~std::uint64_t{0} : 0);
		}
		roots.push_back(expression.bit_state);
	}
	for (const auto& input : inputs)
		roots.push_back(input.bit_state);
	const auto graph = std::make_shared<const core::compiled_circuit>(std::move(roots));
	bindings fixed;
	for (size_t bit = 0; bit < inputs.size(); ++bit)
		if (std::ranges::find(free_indices, bit) == free_indices.end())
			fixed.emplace_back(graph->node_ids.at(inputs[bit].bit_state.get()), bit % 3 == 0);
	for (unsigned target = 0; target < 256; ++target)
	{
		auto bound = fixed;
		auto expected = ~std::uint64_t{0};
		for (size_t row = 0; row < masks.size(); ++row)
		{
			const bool value = (target >> row) & 1;
			bound.emplace_back(graph->root_ids[row], value);
			expected &= value ? masks[row] : ~masks[row];
		}
		check_models(graph, free_inputs, bound, expected, {});
	}
}

void deep_search_and_adjacency()
{
	// No recursive search frames and no quadratic rescanning of the assigned
	// prefix: a large unconstrained input must still finish under this budget.
	std::vector<dixelu::counted_ptr<br::details::bitstate>> roots;
	for (const auto& input : test::unknown_bits(20000))
		roots.push_back(input.bit_state);
	const auto graph = std::make_shared<const core::compiled_circuit>(std::move(roots));
	const br::solver_options options{.affine_reasoning = false, .max_search_steps = 200000};
	require(cr::solve_compiled_stream(graph, {}, [](const auto& model)
	{
		require(model.assignments.size() == 20000, "deep search lost an input");
		return false;
	}, options) == 1, "deep search did not emit its first model");

	core::parent_adjacency adjacency;
	adjacency.build({{2, 2}, {2, core::no_node}, {core::no_node, core::no_node}});
	require(adjacency[0].empty() && adjacency[1].empty(), "unexpected parent");
	require(std::vector<size_t>(adjacency[2].begin(), adjacency[2].end()) == std::vector<size_t>{0, 0, 1},
		"parent packing lost duplicate edges or changed order");
	adjacency.build({});
	require(adjacency.edges.empty() && adjacency.offsets == std::vector<size_t>{0}, "empty adjacency failed");
}

// assert_equality over (x & y) | z, which is true for five of its eight
// assignments, through every overload and engine.
void assert_equality_enumeration()
{
	const br::bit_tracker x = br::unknown, y = br::unknown, z = br::unknown;
	const auto expression = (x & y) | z;
	std::set<unsigned> streamed;
	const auto stream = [&](const cr::solution& model)
	{
		const auto value = [&](const br::bit_tracker& bit) { return unsigned{model.assignments.at(bit.bit_state)}; };
		require(streamed.insert(value(x) | value(y) << 1 | value(z) << 2).second,
			"streaming emitted an assignment twice");
	};

	require(br::assert_equality(expression, true).size() == 5, "(x & y) | z must have five complete true assignments");
	require(br::assert_equality(expression, true, true).size() == 1, "first-only solving must return one assignment");
	require(br::assert_equality(expression, true, stream) == 5 && streamed.size() == 5,
		"streaming must emit every complete assignment exactly once");
	for (const auto& options : test::search_engines)
	{
		br::solver_statistics statistics;
		require(br::assert_equality(expression, true, options, false, &statistics).size() == 5,
			"every engine must enumerate the same five complete assignments");
		require(statistics.solutions == 5 && statistics.variables == 3 && statistics.nodes >= 3 &&
			statistics.decisions != 0 && statistics.propagations != 0 &&
			(options.affine_reasoning || statistics.affine_passes == 0), "solver statistics mismatch");
		require(br::assert_equality(expression, true, options, true).size() == 1,
			"first-only solving must stop after one model");
		streamed.clear();
		size_t callbacks = 0;
		require(br::assert_equality(expression, true, options, [&](const cr::solution& model)
		{
			stream(model);
			return ++callbacks < 3;
		}, &statistics) == 3 && streamed.size() == 3, "a false callback result must stop streaming immediately");
	}
}

void unsatisfiable_and_conflict_learning()
{
	const br::bit_tracker x = br::unknown, y = br::unknown;
	test::require_throws<br::unsatisfiable>([&] { (void)br::assert_equality(x & !x, true); },
		"constant-false equality with true must be unsatisfiable");

	br::bit_tracker contradiction = true;
	for (const auto& clause : test::contradiction(x, y))
		contradiction &= clause;
	br::solver_statistics learned;
	test::require_throws<br::unsatisfiable>([&] { (void)br::assert_equality(contradiction, true, test::cdcl, false, &learned); },
		"CDCL accepted an unsatisfiable formula");
	require(learned.conflicts != 0 && learned.learned_clauses != 0,
		"CDCL must learn from a non-trivial unsatisfiable formula");

	auto bounded = test::cdcl;
	bounded.max_conflict_analysis_nodes = 1;
	br::solver_statistics cutoff;
	test::require_throws<br::unsatisfiable>([&] { (void)br::assert_equality(contradiction, true, bounded, false, &cutoff); },
		"bounded CDCL analysis accepted an unsatisfiable formula");
	require(cutoff.conflict_analysis_cutoffs != 0, "bounded CDCL analysis must fall back without losing completeness");

	test::require_throws<std::logic_error>([&]
	{
		(void)br::assert_equality(x | y, true, br::solver_options{.conflict_learning = true}, true);
	}, "CDCL must reject active affine propagation without explanations");
}

// Fanout orders the decisions as a, c, d. Once a = d = true, the two trap
// clauses imply both x and !x, so the learned (!a | !d) clause can skip the
// irrelevant decision level of c.
void backjumping()
{
	const br::bit_tracker a = br::unknown, c = br::unknown, d = br::unknown, x = br::unknown;
	const br::bit_tracker p = br::unknown, q = br::unknown, r = br::unknown, s = br::unknown;
	const br::bit_tracker t = br::unknown, u = br::unknown, v = br::unknown;
	const auto padding_a = (a | p) & (a | q);
	const auto padding_c = (c | u) & (c | v) & (c | r) & (c | t);
	const auto padding_d = d | s;
	const auto trap = (~a | ~d | x) & (~a | ~d | ~x);
	const auto expression = padding_a & padding_c & padding_d & trap & (t | a);
	const br::solver_options options{.affine_reasoning = false, .conflict_learning = true, .max_conflict_analysis_nodes = 0};
	br::solver_statistics statistics;
	require(br::assert_equality(expression, true, options, true, &statistics).size() == 1 &&
		statistics.learned_clauses != 0 && statistics.backjumps != 0,
		"first-UIP learning must skip an irrelevant decision level");
}

} // namespace

int main()
{
	return test::run("solver_search", {
		{"randomized_truth_tables", randomized_truth_tables},
		{"wide_affine", wide_affine},
		{"deep_search_and_adjacency", deep_search_and_adjacency},
		{"assert_equality_enumeration", assert_equality_enumeration},
		{"unsatisfiable_and_conflict_learning", unsatisfiable_and_conflict_learning},
		{"backjumping", backjumping},
	});
}
