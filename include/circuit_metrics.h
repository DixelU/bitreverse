#ifndef DIXELU_BITREVERSE_CIRCUIT_METRICS_H
#define DIXELU_BITREVERSE_CIRCUIT_METRICS_H

#include <algorithm>

#include "bitreverse.h"

namespace dixelu::bitreverse
{

// Shape of the shared expression DAG reachable from the given output bits.
// Each stored node is counted once; this is not an expanded expression tree.
// Measure forward outputs (not an equality against a target) to get the shape
// of the forward computation alone.
struct circuit_metrics
{
	size_t nodes{}, edges{}, roots{}, distinct_roots{};
	size_t inputs{}, constants{}, not_gates{}, and_gates{}, or_gates{}, xor_gates{}, gates{};
	size_t max_depth{}, max_width{}, max_gate_width{}, max_fanout{};
	size_t min_output_depth{}, max_output_depth{};
	double mean_output_depth{};

	// Depth is the longest dependency path to a leaf, with leaf depth zero.
	// Width counts nodes at that depth; gate_widths excludes inputs/constants.
	std::vector<size_t> widths, gate_widths;
	// One entry per supplied output, preserving order and repeated roots.
	std::vector<size_t> output_depths;
};

inline circuit_metrics measure_circuit(const std::vector<bit_tracker>& outputs)
{
	using namespace collision_resolution::solver_core;

	std::vector<counted_ptr<details::bitstate>> roots;
	roots.reserve(outputs.size());
	for (const auto& output : outputs)
	{
		if (!output.bit_state)
			throw std::invalid_argument("Circuit metrics require non-null output bits");
		roots.push_back(output.bit_state);
	}

	const compiled_circuit graph(std::move(roots));
	circuit_metrics result;
	result.roots = outputs.size();
	result.nodes = graph.nodes.size();

	std::vector<size_t> depths(result.nodes), remaining_inputs(result.nodes);
	std::vector<node_id> ready;
	ready.reserve(result.nodes);
	for (node_id id = 0; id < result.nodes; ++id)
	{
		const auto operation = graph.operations[id];
		switch (operation)
		{
			case '*': ++result.inputs; break;
			case '=': ++result.constants; break;
			case '!': ++result.not_gates; break;
			case '&': ++result.and_gates; break;
			case '|': ++result.or_gates; break;
			case '^': ++result.xor_gates; break;
		}

		// Edges and fanout count argument slots, including a repeated operand.
		const auto arity = details::operation_args_count[operation];
		remaining_inputs[id] = arity;
		result.edges += arity;
		result.max_fanout = std::max(result.max_fanout, graph.parents[id].size());
		if (!arity)
			ready.push_back(id);
	}
	result.gates = result.not_gates + result.and_gates + result.or_gates + result.xor_gates;

	// compiled_circuit IDs follow discovery order, which is not topological:
	// an output can also be a dependency of another output. Process leaves
	// first and release each parent only after all of its operands are ready.
	for (size_t cursor = 0; cursor < ready.size(); ++cursor)
	{
		const node_id id = ready[cursor];
		const size_t depth = depths[id];
		if (result.widths.size() <= depth)
		{
			result.widths.resize(depth + 1);
			result.gate_widths.resize(depth + 1);
		}

		++result.widths[depth];
		if (details::operation_args_count[graph.operations[id]])
			++result.gate_widths[depth];

		for (const node_id parent : graph.parents[id])
		{
			depths[parent] = std::max(depths[parent], depth + 1);
			if (--remaining_inputs[parent] == 0)
				ready.push_back(parent);
		}
	}

	if (!result.widths.empty())
	{
		result.max_depth = result.widths.size() - 1;
		result.max_width = std::ranges::max(result.widths);
		result.max_gate_width = std::ranges::max(result.gate_widths);
	}

	std::vector<bool> seen_roots(result.nodes);
	size_t output_depth_sum = 0;
	result.output_depths.reserve(result.roots);
	for (const node_id root : graph.root_ids)
	{
		result.distinct_roots += !seen_roots[root];
		seen_roots[root] = true;
		output_depth_sum += depths[root];
		result.output_depths.push_back(depths[root]);
	}

	if (result.roots)
	{
		const auto [shallowest, deepest] = std::ranges::minmax(result.output_depths);
		result.min_output_depth = shallowest;
		result.max_output_depth = deepest;
		result.mean_output_depth =
			static_cast<double>(output_depth_sum) / static_cast<double>(result.roots);
	}

	return result;
}

template<size_t N>
circuit_metrics measure_circuit(const int_tracker<N>& output)
{
	return measure_circuit(std::vector<bit_tracker>(output.bits.begin(), output.bits.end()));
}

} // namespace dixelu::bitreverse

#endif // DIXELU_BITREVERSE_CIRCUIT_METRICS_H
