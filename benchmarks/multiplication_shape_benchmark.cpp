#include "bitreverse.h"
#include <algorithm>
#include <array>
#include <chrono>
#include <cmath>
#include <iomanip>
#include <iostream>
#include <stdexcept>
#include <string>
#include <vector>

namespace br = dixelu::bitreverse;
using integer = br::int_tracker<256>;
using value = std::array<bool, 256>; // Least significant bit first.
using clock_type = std::chrono::steady_clock;
using pointer = dixelu::counted_ptr<br::details::bitstate>;
using circuit = br::collision_resolution::solver_core::compiled_circuit;

value parse(const std::string& hex)
{
	if (hex.empty() || hex.size() > 64) throw std::invalid_argument("expected 1..64 hex digits");
	value result{};
	for (size_t digit = 0; digit < hex.size(); ++digit)
	{
		const char ch = hex[hex.size() - digit - 1];
		const unsigned v = ch >= '0' && ch <= '9' ? ch - '0' :
			ch >= 'a' && ch <= 'f' ? ch - 'a' + 10 : 16;
		if (v == 16) throw std::invalid_argument("expected lowercase hex digits");
		for (size_t bit = 0; bit < 4; ++bit) result[digit * 4 + bit] = (v >> bit) & 1;
	}
	return result;
}

std::string hex(const value& bits)
{
	std::string result(64, '0');
	for (size_t digit = 0; digit < 64; ++digit)
	{
		unsigned v = 0;
		for (size_t bit = 0; bit < 4; ++bit) v |= unsigned(bits[digit * 4 + bit]) << bit;
		result[63 - digit] = "0123456789abcdef"[v];
	}
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

struct shape
{
	size_t nodes{}, gates{}, variables{}, depth{}, width{}, gate_width{};
	// Log10 of the node occurrences if sharing were discarded, not stored nodes.
	long double unfolded_log10{};
};

long double log_add(long double a, long double b)
{
	if (a < b) std::swap(a, b);
	return a + std::log10(1.0L + std::pow(10.0L, b - a));
}

shape measure(const std::vector<pointer>& roots)
{
	circuit graph(roots);
	std::vector<size_t> depths(graph.nodes.size()), levels, gate_levels;
	std::vector<long double> occurrences(graph.nodes.size());
	std::vector<unsigned char> ready(graph.nodes.size());
	for (const size_t root : graph.root_ids)
	{
		std::vector<std::pair<size_t, bool>> pending{{root, false}};
		while (!pending.empty())
		{
			const auto [id, expanded] = pending.back(); pending.pop_back();
			if (ready[id] == 2) continue;
			const unsigned arity = br::details::operation_args_count[graph.nodes[id]->operation];
			if (!expanded)
			{
				if (ready[id]) throw std::logic_error("cyclic graph");
				ready[id] = 1; pending.emplace_back(id, true);
				for (unsigned i = arity; i > 0; --i) pending.emplace_back(graph.inputs[id][i - 1], false);
				continue;
			}
			for (unsigned i = 0; i < arity; ++i)
			{
				const size_t child = graph.inputs[id][i];
				depths[id] = std::max(depths[id], depths[child] + 1);
				occurrences[id] = log_add(occurrences[id], occurrences[child]);
			}
			if (levels.size() <= depths[id]) { levels.resize(depths[id] + 1); gate_levels.resize(levels.size()); }
			++levels[depths[id]];
			if (arity) ++gate_levels[depths[id]];
			ready[id] = 2;
		}
	}
	shape result;
	result.nodes = graph.nodes.size(); result.variables = graph.variables.size();
	for (size_t i = 0; i < levels.size(); ++i)
	{
		result.width = std::max(result.width, levels[i]);
		result.gate_width = std::max(result.gate_width, gate_levels[i]);
		result.gates += gate_levels[i];
	}
	result.depth = levels.empty() ? 0 : levels.size() - 1;
	bool first = true;
	for (size_t root : graph.root_ids)
	{
		result.unfolded_log10 = first ? occurrences[root] : log_add(result.unfolded_log10, occurrences[root]);
		first = false;
	}
	return result;
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
	const auto started = clock_type::now();
	const auto product = known_left ? known * unknown : unknown * known;
	const double build_ms = std::chrono::duration<double, std::milli>(clock_type::now() - started).count();
	std::vector<pointer> roots;
	for (const auto& bit : product.bits) roots.push_back(bit.bit_state);
	const auto forward = measure(roots);
	br::bit_tracker mismatch(false);
	for (size_t i = 0; i < 256; ++i) mismatch |= product.bits[i] ^ target.bits[i];
	const auto equality = measure({mismatch.bit_state});
	std::vector<timing> runs;
	for (size_t repetition = 0; repetition < repetitions; ++repetition)
	{
		timing current;
		br::solver_options options;
		options.affine_reasoning = affine;
		options.max_search_steps = 20000000;
		const auto begin = clock_type::now();
		br::collision_resolution::solutions_t solutions;
		try
		{
			// false means ALL reachable-variable models, as in the default API.
			solutions = br::assert_equality<256>(product, target, options, false, &current.statistics);
			current.status = "complete";
		}
		catch (const br::solver_limit&) { current.status = "step_limit"; }
		catch (const std::runtime_error& error)
		{
			if (std::string(error.what()) != "Unsatisfiable constraints") throw;
			current.status = "unsat";
		}
		current.wall_ms = std::chrono::duration<double, std::milli>(clock_type::now() - begin).count();
		current.search_ms = std::chrono::duration<double, std::milli>(current.statistics.elapsed).count();
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
		<< affine << ',' << impossible << ',' << hex(multiplier) << ',' << hex(target_bits) << ','
		<< weight << ',' << trailing << ',' << forward.nodes << ',' << forward.gates << ',' << forward.depth
		<< ',' << forward.width << ',' << forward.gate_width << ',' << equality.nodes << ',' << equality.depth
		<< ',' << equality.width << ',' << equality.gate_width << ',' << equality.unfolded_log10 << ','
		<< build_ms << ',' << mid.wall_ms << ',' << mid.search_ms << ',' << runs.front().wall_ms << ','
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
		std::cout << "# GCC release benchmark:256-bit product modulo2^256; native shift/add reference verifies every returned model.\n"
			<< "# Depth is longest leaf-to-root edge count; width is max node count at equal leaf-based depth (DAG, not unfolded tree).\n"
			<< "# Median of5 ALL-model assert_equality calls;20m search-step budget per call. Graph/build timings excluded from assertion.\n"
			<< "# Missing unknown bits in raw assertion models are don't-cares, NOT enumerated complete256-bit candidates.\n"
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
