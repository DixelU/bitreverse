#include "bitreverse.h"
#include <iostream>
#include <random>
#include <stdexcept>

namespace br = dixelu::bitreverse;
namespace cr = br::collision_resolution;
namespace core = cr::solver_core;
using bindings = std::vector<std::pair<size_t, bool>>;

namespace
{
void require(bool condition, const char* message)
{
	if (!condition) throw std::runtime_error(message);
}

std::uint64_t input_mask(size_t bit)
{
	std::uint64_t result = 0;
	for (size_t assignment = 0; assignment < 64; ++assignment)
		if ((assignment >> bit) & 1) result |= std::uint64_t{1} << assignment;
	return result;
}

void check_models(const std::shared_ptr<const core::compiled_circuit>& graph,
	const std::vector<br::bit_tracker>& inputs, const bindings& bound,
	std::uint64_t expected, br::solver_options options)
{
	std::uint64_t actual = 0;
	const auto count = cr::solve_compiled_stream(graph, bound, [&](const cr::crs_state& model)
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
	std::mt19937 random(0x28367);
	for (size_t trial = 0; trial < 100; ++trial)
	{
		std::vector<br::bit_tracker> inputs(6), expressions;
		std::vector<std::uint64_t> masks;
		for (size_t bit = 0; bit < 6; ++bit)
		{
			inputs[bit] = br::unknown;
			expressions.push_back(inputs[bit]);
			masks.push_back(input_mask(bit));
		}
		expressions.emplace_back(false); masks.push_back(0);
		expressions.emplace_back(true); masks.push_back(~std::uint64_t{0});
		for (size_t gate = 0; gate < 30; ++gate)
		{
			const size_t a = random() % expressions.size(), b = random() % expressions.size();
			switch (random() % 4)
			{
				case 0: expressions.push_back(expressions[a] & expressions[b]); masks.push_back(masks[a] & masks[b]); break;
				case 1: expressions.push_back(expressions[a] | expressions[b]); masks.push_back(masks[a] | masks[b]); break;
				case 2: expressions.push_back(expressions[a] ^ expressions[b]); masks.push_back(masks[a] ^ masks[b]); break;
				case 3: expressions.push_back(~expressions[a]); masks.push_back(~masks[a]); break;
			}
		}
		std::vector<dixelu::counted_ptr<br::details::bitstate>> roots;
		for (size_t i = expressions.size() - 3; i < expressions.size(); ++i)
			roots.push_back(expressions[i].bit_state);
		for (const auto& input : inputs) roots.push_back(input.bit_state);
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
			for (unsigned mode = 0; mode < 3; ++mode)
			{
				br::solver_options options;
				options.affine_reasoning = mode == 1;
				options.conflict_learning = mode == 2;
				check_models(graph, inputs, bound, expected, options);
			}
		}
	}
}

void wide_affine()
{
	std::mt19937 random(0x17289);
	std::vector<br::bit_tracker> inputs(130), free_inputs;
	const std::array<size_t, 6> free_indices{0, 1, 63, 64, 127, 129};
	for (auto& input : inputs) input = br::unknown;
	for (auto id : free_indices) free_inputs.push_back(inputs[id]);
	std::vector<dixelu::counted_ptr<br::details::bitstate>> roots;
	std::array<std::uint64_t, 8> masks{};
	for (size_t row = 0; row < masks.size(); ++row)
	{
		br::bit_tracker expression(false);
		for (size_t bit = 0; bit < inputs.size(); ++bit)
		{
			if (!(random() & 1)) continue;
			expression ^= inputs[bit];
			const auto found = std::find(free_indices.begin(), free_indices.end(), bit);
			masks[row] ^= found != free_indices.end() ? input_mask(found - free_indices.begin())
				: (bit % 3 == 0 ? ~std::uint64_t{0} : 0);
		}
		roots.push_back(expression.bit_state);
	}
	for (const auto& input : inputs) roots.push_back(input.bit_state);
	const auto graph = std::make_shared<const core::compiled_circuit>(std::move(roots));
	bindings fixed;
	for (size_t bit = 0; bit < inputs.size(); ++bit)
		if (std::find(free_indices.begin(), free_indices.end(), bit) == free_indices.end())
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
	for (size_t i = 0; i < 20000; ++i)
	{
		br::bit_tracker input;
		input = br::unknown;
		roots.push_back(input.bit_state);
	}
	const auto graph = std::make_shared<const core::compiled_circuit>(std::move(roots));
	br::solver_options options;
	options.affine_reasoning = false;
	options.max_search_steps = 200000;
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
}

int main()
{
	try
	{
		randomized_truth_tables();
		wide_affine();
		deep_search_and_adjacency();
		std::cout << "All solver search tests passed\n";
	}
	catch (const std::exception& error)
	{
		std::cerr << error.what() << '\n';
		return 1;
	}
}
