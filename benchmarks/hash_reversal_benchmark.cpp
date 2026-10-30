#ifdef BITREVERSE_TRACK_ALLOCATIONS
#include "search_allocations.h"
#endif
#include "circuit_metrics.h"
#include "crc32.h"
#include "md5.h"
#include "sha1.h"
#include "sha256.h"
#include "text.h"
#include "timing.h"

#include <cstdint>
#include <iomanip>
#include <iostream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

namespace br = dixelu::bitreverse;

namespace
{
template<size_t N, class Hash>
void run(const std::string& algorithm, const std::vector<std::uint8_t>& bytes,
	size_t unknown_bits, std::string_view target_hex, const std::string& engine,
	size_t budget, Hash&& hash)
{
	if (target_hex.size() != N / 4) throw std::invalid_argument("wrong target digest length");
	const auto target_bytes = support::parse_hex_bytes(target_hex);
	std::vector<br::itu8> message;
	for (const auto byte : bytes) message.emplace_back(byte);
	// Prefix bytes, low bit first within each byte. The witness's unknown
	// values are discarded; only the remaining template bits reach the solver.
	for (size_t bit = 0; bit < unknown_bits; ++bit)
		message[bit / 8].bits[7 - bit % 8] = br::unknown;
	const auto build_begin = support::clock::now();
	const auto digest = hash(message);
	const double build_ms = support::milliseconds_since(build_begin);
	const auto metrics_begin = support::clock::now();
	const auto shape = br::measure_circuit(digest);
	const double metrics_ms = support::milliseconds_since(metrics_begin);
	// This record is flushed before even constructing the equality expression.
	// An external wall timeout can therefore retain the unconstrained shape.
	std::cout << std::fixed << std::setprecision(6)
		<< "{\"phase\":\"forward\",\"algorithm\":\"" << algorithm
		<< "\",\"message_hex\":\"" << support::to_hex(bytes) << "\",\"unknown_bits\":" << unknown_bits
		<< ",\"engine\":\"" << engine << "\",\"budget\":" << budget
		<< ",\"target_hex\":\"" << support::to_hex(target_bytes) << "\",\"output_bits\":" << N
		<< ",\"nodes\":" << shape.nodes << ",\"gates\":" << shape.gates
		<< ",\"inputs\":" << shape.inputs << ",\"constants\":" << shape.constants
		<< ",\"edges\":" << shape.edges << ",\"depth\":" << shape.max_depth
		<< ",\"width\":" << shape.max_width << ",\"gate_width\":" << shape.max_gate_width
		<< ",\"max_fanout\":" << shape.max_fanout << ",\"xor_gates\":" << shape.xor_gates
		<< ",\"and_gates\":" << shape.and_gates << ",\"or_gates\":" << shape.or_gates
		<< ",\"not_gates\":" << shape.not_gates << ",\"output_depth_min\":" << shape.min_output_depth
		<< ",\"output_depth_mean\":" << shape.mean_output_depth
		<< ",\"output_depth_max\":" << shape.max_output_depth
		<< ",\"build_ms\":" << build_ms << ",\"metrics_ms\":" << metrics_ms << "}" << std::endl;
	if (engine == "shape") return;

	br::int_tracker<N> target(0);
	for (size_t bit = 0; bit < N; ++bit)
		target.bits[bit] = br::bit_tracker((target_bytes[bit / 8] >> (7 - bit % 8)) & 1U);
	// The same expression that assert_equality<N> solves.
	const auto equality = br::measure_circuit(std::vector{br::differs(digest, target)});
	br::solver_options options;
	options.affine_reasoning = engine == "affine";
	options.conflict_learning = engine == "cdcl";
	options.max_search_steps = budget;
	br::solver_statistics stats;
	br::collision_resolution::solutions_t solutions;
	std::string status;
#ifdef BITREVERSE_TRACK_ALLOCATIONS
	const auto initial_bytes = search_allocations::live;
	const auto initial_calls = search_allocations::calls;
	search_allocations::peak = initial_bytes;
#endif
	const auto solve_begin = support::clock::now();
	try
	{
		solutions = br::assert_equality<N>(digest, target, options, true, &stats);
		status = "sat";
	}
	catch (const br::solver_limit&) { status = "step_limit"; }
	catch (const br::unsatisfiable&) { status = "unsat"; }
	const double solve_ms = support::milliseconds_since(solve_begin);
#ifdef BITREVERSE_TRACK_ALLOCATIONS
	const auto solve_peak_bytes = search_allocations::peak - initial_bytes;
	const auto solve_allocation_calls = search_allocations::calls - initial_calls;
#endif
	std::string recovered_hex;
	if (!solutions.empty())
	{
		auto recovered = bytes;
		for (size_t bit = 0; bit < unknown_bits; ++bit)
		{
			const auto found = solutions.begin()->assignments.find(message[bit / 8].bits[7 - bit % 8].bit_state);
			// An optimized-out input is a don't-care: choose zero explicitly,
			// never copy the original witness's value into a reported preimage.
			const bool value = found != solutions.begin()->assignments.end() && found->second;
			const auto mask = static_cast<std::uint8_t>(1U << (bit % 8));
			recovered[bit / 8] = static_cast<std::uint8_t>((recovered[bit / 8] & ~mask) | (value ? mask : 0));
		}
		recovered_hex = support::to_hex(recovered);
	}
	std::cout << "{\"phase\":\"solve\",\"equality_nodes\":" << equality.nodes
		<< ",\"equality_depth\":" << equality.max_depth << ",\"equality_width\":" << equality.max_width
		<< ",\"status\":\"" << status << "\",\"solve_ms\":" << solve_ms
		<< ",\"decisions\":" << stats.decisions << ",\"conflicts\":" << stats.conflicts
		<< ",\"propagations\":" << stats.propagations << ",\"search_steps\":" << stats.search_steps
		<< ",\"solutions\":" << stats.solutions << ",\"affine_active\":" << (stats.affine_enabled ? "true" : "false")
		<< ",\"recovered_hex\":\"" << recovered_hex << '"'
#ifdef BITREVERSE_TRACK_ALLOCATIONS
		<< ",\"solve_peak_requested_bytes\":" << solve_peak_bytes
		<< ",\"solve_allocation_calls\":" << solve_allocation_calls
#endif
		<< "}" << std::endl;
}
}

int main(int argc, char** argv)
{
	try
	{
		if (argc != 7) throw std::invalid_argument(
			"usage: hash_reversal_benchmark crc32|md5|sha1|sha256 INPUT_HEX UNKNOWN_BITS TARGET_HEX shape|dpll|affine|cdcl MAX_STEPS");
		const std::string algorithm = argv[1], engine = argv[5];
		const auto bytes = support::parse_hex_bytes(argv[2]);
		const size_t unknown_bits = support::parse_size(argv[3], "UNKNOWN_BITS");
		const size_t budget = support::parse_size(argv[6], "MAX_STEPS");
		if (bytes.size() > 128) throw std::invalid_argument("benchmark message cap is 128 bytes");
		if (unknown_bits > bytes.size() * 8) throw std::invalid_argument("more unknown bits than message bits");
		if (!budget) throw std::invalid_argument("search budget must be positive");
		if (engine != "shape" && engine != "dpll" && engine != "affine" && engine != "cdcl")
			throw std::invalid_argument("unknown search engine");
		if (algorithm == "crc32") run<32>(algorithm, bytes, unknown_bits, argv[4], engine, budget,
			[](const auto& message) { return br::hash::crc32(message); });
		else if (algorithm == "md5") run<128>(algorithm, bytes, unknown_bits, argv[4], engine, budget,
			[](const auto& message) { return br::hash::md5(message); });
		else if (algorithm == "sha1") run<160>(algorithm, bytes, unknown_bits, argv[4], engine, budget,
			[](const auto& message) { return br::hash::sha1(message); });
		else if (algorithm == "sha256") run<256>(algorithm, bytes, unknown_bits, argv[4], engine, budget,
			[](const auto& message) { return br::hash::sha256(message); });
		else throw std::invalid_argument("unknown hash algorithm");
		return 0;
	}
	catch (const std::exception& error)
	{
		std::cerr << "hash_reversal_benchmark: " << error.what() << '\n';
		return 1;
	}
}
