#include "bitreverse.h"
#include "circuit_metrics.h"
#include "text.h"
#include "timing.h"

#include <algorithm>
#include <array>
#include <cmath>
#include <iomanip>
#include <iostream>
#include <numeric>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

namespace br = dixelu::bitreverse;
using integer = br::int_tracker<256>;
using value = std::array<bool, 256>; // Least significant bit first.

value parse(std::string_view hex)
{
	if (hex.empty() || hex.size() > 64) throw std::invalid_argument("expected 1..64 hex digits");
	const auto bits = support::parse_hex_number(hex);
	value result{};
	std::copy(bits.begin(), bits.end(), result.begin());
	return result;
}

integer tracked(const value& bits)
{
	integer result{};
	for (size_t i = 0; i < bits.size(); ++i) result.bits[255 - i] = br::bit_tracker(bits[i]);
	return result;
}

// Independent Boolean schoolbook reference, without tracked integer operators.
value native_product(const value& lhs, const value& rhs)
{
	value result{};
	for (size_t shift = 0; shift < 256; ++shift)
		if (rhs[shift])
		{
			unsigned carry = 0;
			for (size_t bit = shift; bit < 256; ++bit)
			{
				const unsigned sum = unsigned(result[bit]) + unsigned(lhs[bit - shift]) + carry;
				result[bit] = sum & 1; carry = sum >> 1;
			}
		}
	return result;
}

long double log_add(long double a, long double b)
{
	if (a < b) std::swap(a, b);
	return a + std::log10(1.0L + std::pow(10.0L, b - a));
}

// log10 of the node count of the expression tree obtained by expanding all
// sharing in the DAG below `root`.
long double unfolded_log10(const br::bit_tracker& root)
{
	const br::collision_resolution::solver_core::compiled_circuit graph(root.bit_state);
	// Every operand has a smaller max_depth than its gate, so this order
	// visits operands first.
	std::vector<size_t> order(graph.nodes.size());
	std::iota(order.begin(), order.end(), size_t{0});
	std::ranges::sort(order, {}, [&](size_t id) -> size_t { return graph.nodes[id]->max_depth; });
	// log10 of each node's tree size; zero is a single node.
	std::vector<long double> occurrences(graph.nodes.size());
	for (const size_t id : order)
		for (unsigned i = 0; i < br::details::operation_args_count[graph.operations[id]]; ++i)
			occurrences[id] = log_add(occurrences[id], occurrences[graph.inputs[id][i]]);
	return occurrences[graph.root_id];
}

struct timing
{
	double wall_ms{}, search_ms{};
	br::solver_statistics statistics;
	size_t assigned{};
	std::string status;
};

void run(const std::string& label, const value& multiplier, const value& witness,
	bool known_left, bool affine, bool impossible, size_t repetitions)
{
	integer unknown = br::unknown;
	const auto known = tracked(multiplier);
	auto target_bits = native_product(multiplier, witness);
	if (impossible) target_bits[0] = !target_bits[0];
	const auto target = tracked(target_bits);
	const auto started = support::clock::now();
	const auto product = known_left ? known * unknown : unknown * known;
	const double build_ms = support::milliseconds_since(started);
	const auto forward = br::measure_circuit(product);
	const auto mismatch = br::differs(product, target);
	const auto equality = br::measure_circuit(std::vector{mismatch});
	const auto equality_unfolded_log10 = unfolded_log10(mismatch);
	std::vector<timing> runs;
	for (size_t repetition = 0; repetition < repetitions; ++repetition)
	{
		timing current;
		br::solver_options options;
		options.affine_reasoning = affine;
		options.max_search_steps = 20000000;
		const auto begin = support::clock::now();
		br::collision_resolution::solutions_t solutions;
		try
		{
			// false means ALL reachable-variable models, as in the default API.
			solutions = br::assert_equality<256>(product, target, options, false, &current.statistics);
			current.status = "complete";
		}
		catch (const br::solver_limit&) { current.status = "step_limit"; }
		catch (const br::unsatisfiable&) { current.status = "unsat"; }
		current.wall_ms = support::milliseconds_since(begin);
		current.search_ms = support::milliseconds(current.statistics.elapsed);
		for (const auto& solution : solutions)
		{
			value recovered{};
			for (size_t i = 0; i < 256; ++i)
			{
				const auto found = solution.assignments.find(unknown.bits[255 - i].bit_state);
				if (found != solution.assignments.end()) recovered[i] = found->second;
			}
			if (native_product(multiplier, recovered) != target_bits)
				throw std::runtime_error("model fails independent native multiplication");
			current.assigned = solution.assignments.size();
		}
		if (!impossible && current.status == "unsat") throw std::runtime_error("reachable target rejected");
		if (impossible && current.status == "complete") throw std::runtime_error("unreachable target accepted");
		runs.push_back(current);
	}
	std::sort(runs.begin(), runs.end(), [](const auto& a, const auto& b) { return a.wall_ms < b.wall_ms; });
	const auto& mid = runs[runs.size() / 2];
	const auto& stats = mid.statistics;
	size_t trailing = 0, weight = 0;
	while (trailing < 256 && !multiplier[trailing]) ++trailing;
	for (bool bit : multiplier) weight += bit;
	std::cout << label << ',' << (known_left ? "known_x_unknown" : "unknown_x_known") << ','
		<< affine << ',' << impossible << ',' << support::to_hex_number(multiplier) << ','
		<< support::to_hex_number(target_bits) << ',' << weight << ',' << trailing << ',' << forward.nodes << ','
		<< forward.gates << ',' << forward.max_depth << ',' << forward.max_width << ',' << forward.max_gate_width << ','
		<< equality.nodes << ',' << equality.max_depth << ',' << equality.max_width << ',' << equality.max_gate_width << ','
		<< equality_unfolded_log10 << ',' << build_ms << ',' << mid.wall_ms << ',' << mid.search_ms << ',' << runs.front().wall_ms << ','
		<< runs.back().wall_ms << ',' << mid.status << ',' << stats.variables << ',' << mid.assigned << ','
		<< stats.solutions << ',' << stats.decisions << ',' << stats.conflicts << ',' << stats.propagations
		<< ',' << stats.affine_enabled << ',' << stats.affine_passes << ',' << stats.search_steps << '\n';
	std::cout.flush();
}

int main(int argc, char** argv)
{
	try
	{
		const value witness = parse("0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef");
		std::cout << "# 256-bit product modulo 2^256 with one known factor: median of 5 all-model assert_equality calls of at most\n"
			<< "# 20M search steps; each model is checked by native multiplication, and unassigned bits are don't-cares.\n"
			<< "case,order,affine_requested,impossible,k_hex,target_hex,popcount,trailing_zeroes,product_nodes,product_gates,product_depth,product_width,product_gate_width,equality_nodes,equality_depth,equality_width,equality_gate_width,unfolded_equality_nodes_log10,build_ms,assert_median_ms,search_median_run_ms,assert_min_ms,assert_max_ms,status,solver_variables,assigned_bits,models,decisions,conflicts,propagations,affine_active,affine_passes,search_steps\n"
			<< std::fixed << std::setprecision(6);
		const std::vector<std::pair<std::string, std::string>> cases{
			{"three", "3"}, {"thirteen", "d"},
			{"dense_odd", "d6e8feb86659fd93a5a3564e27f8862b9e3779b97f4a7c15bf58476d1ce4e5b9"},
			{"all_ones", std::string(64, 'f')}, {"twelve", "c"}, {"shift8", "100"}, {"zero", "0"}};
		for (const auto& [name, text] : cases)
		{
			if (argc > 1 && name != argv[1]) continue;
			for (bool known_left : {false, true})
				for (bool affine : {true, false}) run(name, parse(text), witness, known_left, affine, false, 5);
			if (name == "twelve" || name == "zero") run(name, parse(text), witness, false, true, true, 5);
		}
	}
	catch (const std::exception& error) { std::cerr << error.what() << '\n'; return 1; }
}
