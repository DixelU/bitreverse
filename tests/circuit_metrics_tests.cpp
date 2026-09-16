#include "circuit_metrics.h"

#include <cmath>
#include <iostream>
#include <numeric>
#include <stdexcept>

namespace br = dixelu::bitreverse;

namespace
{
void require(bool condition, const char* message)
{
	if (!condition) throw std::runtime_error(message);
}

void empty_and_constant_tests()
{
	const auto empty = br::measure_circuit(std::vector<br::bit_tracker>{});
	require(empty.nodes == 0 && empty.edges == 0 && empty.roots == 0 &&
		empty.max_depth == 0 && empty.max_width == 0 && empty.widths.empty() &&
		empty.output_depths.empty() && empty.mean_output_depth == 0,
		"empty output set must have zero metrics and no levels");
	const br::bit_tracker zero(false), one(true);
	const auto constants = br::measure_circuit({zero, one, zero});
	require(constants.nodes == 2 && constants.constants == 2 && constants.inputs == 0 &&
		constants.gates == 0 && constants.edges == 0 && constants.roots == 3 &&
		constants.distinct_roots == 2 && constants.max_fanout == 0 &&
		constants.widths == std::vector<size_t>{2} &&
		constants.gate_widths == std::vector<size_t>{0} &&
		constants.output_depths == std::vector<size_t>({0, 0, 0}),
		"constants and duplicate output roots must be measured by identity");
}

void shared_dag_test()
{
	br::bit_tracker a, b, c, unused;
	a = br::unknown; b = br::unknown; c = br::unknown; unused = br::unknown;
	const auto shared = a ^ b;
	const auto inverted = ~shared;
	const auto conjunction = shared & c;
	const auto root = inverted | conjunction;
	const auto root_before = root.bit_state.get();
	const auto metrics = br::measure_circuit({root, shared, root, false, true});
	require(metrics.nodes == 9 && metrics.edges == 7 && metrics.roots == 5 &&
		metrics.distinct_roots == 4 && metrics.inputs == 3 && metrics.constants == 2,
		"shared DAG must count each reachable node once and omit unused inputs");
	require(metrics.not_gates == 1 && metrics.and_gates == 1 &&
		metrics.or_gates == 1 && metrics.xor_gates == 1 && metrics.gates == 4,
		"all Boolean gate types must be counted separately");
	require(metrics.widths == std::vector<size_t>({5, 1, 2, 1}) &&
		metrics.gate_widths == std::vector<size_t>({0, 1, 2, 1}) &&
		metrics.max_depth == 3 && metrics.max_width == 5 && metrics.max_gate_width == 2 &&
		metrics.max_fanout == 2,
		"depth levels and fanout must retain DAG sharing");
	require(metrics.output_depths == std::vector<size_t>({3, 1, 3, 0, 0}) &&
		metrics.min_output_depth == 0 && metrics.max_output_depth == 3 &&
		std::abs(metrics.mean_output_depth - 1.4) < 1e-12,
		"output depths must preserve roots in order, including repeats");
	require(std::accumulate(metrics.widths.begin(), metrics.widths.end(), size_t{}) == metrics.nodes &&
		std::accumulate(metrics.gate_widths.begin(), metrics.gate_widths.end(), size_t{}) == metrics.gates,
		"depth histograms must partition nodes and gates");
	require(root.bit_state.get() == root_before && root.bit_state->operation == '|' &&
		a.bit_state->operation == '*', "measurement must not bind or mutate expressions");
}

void discovery_order_and_integer_tests()
{
	br::bit_tracker a, b;
	a = br::unknown; b = br::unknown;
	const auto intermediate = a ^ b;
	const auto last = ~intermediate;
	// A leaf and an interior node are also roots. Neither forward nor reverse
	// compiled node ID order is a valid dependency order for this root set.
	const auto metrics = br::measure_circuit({a, last, intermediate});
	require(metrics.output_depths == std::vector<size_t>({0, 2, 1}) &&
		metrics.widths == std::vector<size_t>({2, 1, 1}),
		"depth computation must not assume compiled node IDs are topological");
	br::int_tracker<4> output(0);
	output.bits[1] = a;
	output.bits[2] = intermediate;
	output.bits[3] = last;
	const auto integer = br::measure_circuit(output);
	require(integer.nodes == 5 && integer.inputs == 2 && integer.constants == 1 &&
		integer.roots == 4 && integer.distinct_roots == 4 &&
		integer.output_depths == std::vector<size_t>({0, 0, 1, 2}),
		"integer overload must preserve bit-array order and count reachable constants");
}

void repeated_operand_test()
{
	br::bit_tracker a;
	a = br::unknown;
	// Bypass simplification deliberately to exercise a repeated operand edge.
	auto state = dixelu::make_counted<br::details::bitstate>();
	state->operation = '^'; state->_1 = a.bit_state; state->_2 = a.bit_state;
	const br::bit_tracker root(std::move(state));
	const auto metrics = br::measure_circuit({root});
	require(metrics.nodes == 2 && metrics.edges == 2 && metrics.inputs == 1 &&
		metrics.xor_gates == 1 && metrics.max_fanout == 2 && metrics.max_depth == 1 &&
		metrics.widths == std::vector<size_t>({1, 1}),
		"repeated operand slots must count as two edges without duplicating nodes");
}
}

int main()
{
	try
	{
		empty_and_constant_tests();
		shared_dag_test();
		discovery_order_and_integer_tests();
		repeated_operand_test();
		std::cout << "All circuit metrics tests passed\n";
	}
	catch (const std::exception& error)
	{
		std::cerr << error.what() << '\n';
		return 1;
	}
}
