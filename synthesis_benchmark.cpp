#include <chrono>
#include <cstdint>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

#include "crc32.h"
#include "inverse.h"
#include "md5.h"

namespace br = dixelu::bitreverse;
namespace inv = br::inversion;

namespace
{
using clock_type = std::chrono::steady_clock;
std::uint64_t consumed = 0;

inv::values integer_bits(std::uint64_t value, size_t width)
{
	inv::values result(width);
	for (size_t bit = 0; bit < width; ++bit) result[bit] = ((value >> bit) & 1) != 0;
	return result;
}

inv::values concrete_bits(const inv::bits& value)
{
	inv::values result;
	for (const auto& bit : value)
	{
		if (bit.bit_state->operation != '=') throw std::runtime_error("benchmark expected concrete forward outputs");
		result.push_back(bit.bit_state->state != 0);
	}
	return result;
}

std::string artifact_of(const inv::program& program)
{
	std::ostringstream output;
	program.save(output);
	return output.str();
}

struct query_sample
{
	inv::values target;
	inv::values original_input;
};

double query_time(const inv::program& program, const std::vector<query_sample>& queries, bool require_unique)
{
	if (queries.empty()) throw std::runtime_error("benchmark has no queries");
	const auto free = program.is_synthesized() ? inv::values{} : inv::values(program.free_bit_count());
	// Validate once, outside the timed loop. Noninjective cases have independent
	// exhaustive coverage in synthesis_tests; their canonical input can differ.
	for (const auto& query : queries)
	{
		const auto result = program.evaluate(query.target, free);
		if (!result || (require_unique && *result != query.original_input))
			throw std::runtime_error("benchmark inverse failed recovery validation");
	}
	std::uint64_t checksum = 0;
	size_t count = 0;
	const auto started = clock_type::now();
	do
	{
		for (unsigned batch = 0; batch < 128; ++batch, ++count)
		{
			const auto result = program.evaluate(queries[count % queries.size()].target, free);
			if (!result) throw std::runtime_error("benchmark lost a valid input");
			for (const bool bit : *result) checksum = (checksum * 131U) ^ static_cast<unsigned>(bit);
		}
	} while (count < 32768 && clock_type::now() - started < std::chrono::milliseconds(30));
	const auto elapsed = std::chrono::duration<double, std::nano>(clock_type::now() - started).count();
	consumed ^= checksum + count;
	return elapsed / static_cast<double>(count);
}

template<class MakeQueries>
void measure(const std::string& name, const inv::program& original, MakeQueries&& make_queries,
	bool unique, bool synthesize = true)
{
	inv::synthesis_options options;
	inv::synthesis_statistics statistics;
	const auto started = clock_type::now();
	try
	{
		const auto built = synthesize ? original.synthesized(options, &statistics) : original;
		const auto build_ms = std::chrono::duration<double, std::milli>(clock_type::now() - started).count();
		const auto artifact = artifact_of(built);
		std::istringstream source(artifact);
		const auto loaded = inv::program::load(source);
		const auto queries = make_queries();
		const auto query_ns = query_time(loaded, queries, unique);
		std::cout << name << ',' << original.unknown_count() << ',' << original.output_count() << ','
			<< original.node_count() << ',' << statistics.created_nodes << ',';
		if (synthesize)
			std::cout << built.synthesized_node_count() << ',' << built.synthesized_relation_node_count() << ','
				<< built.synthesized_function_node_count() << ',' << built.synthesized_selector_node_count() << ','
				<< built.synthesized_validity_node_count();
		else std::cout << "NA,NA,NA,NA,NA";
		std::cout << ',' << build_ms << ',' << artifact.size() << ',' << query_ns << ','
			<< statistics.operations << ',' << options.max_nodes << ',' << options.max_operations << ",ok\n";
	}
	catch (const inv::synthesis_limit& error)
	{
		const auto build_ms = std::chrono::duration<double, std::milli>(clock_type::now() - started).count();
		std::cout << name << ',' << original.unknown_count() << ',' << original.output_count() << ','
			<< original.node_count() << ',' << statistics.created_nodes << ",NA,NA,NA,NA,NA," << build_ms
			<< ",NA,NA," << statistics.operations << ',' << options.max_nodes << ',' << options.max_operations
			<< ",limit\n";
		std::cerr << name << ": " << error.what() << '\n';
	}
}

void reversible_case()
{
	inv::bits input(3);
	for (auto& bit : input) bit = br::unknown;
	const auto original = inv::program::compile(input, {input[0] ^ (input[1] & input[2]), input[1], input[2]});
	measure("toffoli3", original, []
	{
		std::vector<query_sample> result;
		for (unsigned input = 0; input < 8; ++input)
		{
			const auto v = integer_bits(input, 3);
			result.push_back({{v[0] != (v[1] && v[2]), v[1], v[2]}, v});
		}
		return result;
	}, true);
}

template<size_t Width>
void arithmetic_cases()
{
	br::int_tracker<Width> input{br::unknown};
	constexpr unsigned mask = (1U << Width) - 1;
	const auto odd = inv::program::compile(inv::bits_of(input), inv::bits_of(input * br::int_tracker<Width>{3}));
	measure("multiply3_" + std::to_string(Width), odd, []
	{
		std::vector<query_sample> result;
		for (unsigned input = 0; input <= mask; ++input)
			result.push_back({integer_bits((input * 3U) & mask, Width), integer_bits(input, Width)});
		return result;
	}, true);
	const auto square = inv::program::compile(inv::bits_of(input), inv::bits_of(input * input));
	measure("square_" + std::to_string(Width), square, []
	{
		std::vector<query_sample> result;
		for (unsigned input = 0; input <= mask; ++input)
			result.push_back({integer_bits((input * input) & mask, Width), integer_bits(input, Width)});
		return result;
	}, false);
}

void crc_cases()
{
	std::vector<br::itu8> message;
	message.emplace_back(br::unknown);
	const auto original = inv::program::compile(inv::bits_of(message), inv::bits_of(br::hash::crc32(message)));
	const auto queries = []
	{
		std::vector<query_sample> result;
		for (unsigned input = 0; input < 256; ++input)
		{
			std::uint32_t checksum = 0xffffffffU ^ input;
			for (unsigned bit = 0; bit < 8; ++bit) checksum = (checksum >> 1) ^ ((checksum & 1) ? 0xedb88320U : 0);
			result.push_back({integer_bits(~checksum, 32), integer_bits(input, 8)});
		}
		return result;
	};
	measure("crc32_8_affine", original, queries, true, false);
	measure("crc32_8_synthesized", original, queries, true);
}

void md5_case(size_t unknown_count)
{
	std::vector<br::itu8> message{'m', 'd', '5', 0};
	for (size_t bit = 0; bit < unknown_count; ++bit) message[3 - bit / 8].bits[7 - bit % 8] = br::unknown;
	const auto original = inv::program::compile(inv::bits_of(message), inv::bits_of(br::hash::md5(message)));
	measure("md5_" + std::to_string(unknown_count), original, [unknown_count]
	{
		std::vector<query_sample> result;
		const auto domain = size_t{1} << unknown_count;
		// Sample evenly across the full specialized domain; all 256 digests of
		// the unknown-byte case are independently checked in synthesis_tests.
		const auto samples = std::min(domain, size_t{256});
		for (size_t sample = 0; sample < samples; ++sample)
		{
			const size_t assignment = sample * domain / samples;
			std::array<unsigned char, 4> bytes{'m', 'd', '5', 0};
			for (size_t bit = 0; bit < unknown_count; ++bit)
			{
				const auto mask = static_cast<unsigned char>(1U << (bit % 8));
				auto& byte = bytes[3 - bit / 8];
				byte = static_cast<unsigned char>((byte & ~mask) | ((assignment >> bit & 1) ? mask : 0));
			}
			std::vector<br::itu8> concrete;
			for (const auto byte : bytes) concrete.emplace_back(byte);
			result.push_back({concrete_bits(inv::bits_of(br::hash::md5(concrete))), concrete_bits(inv::bits_of(concrete))});
		}
		return result;
	}, true);
}
}

int main()
{
	try
	{
		std::cout << std::fixed << std::setprecision(3);
		std::cout << "# Reusable inverse synthesis: build once, save/load, then evaluate new outputs.\n"
			<< "# DD nodes are Boolean multiplexers, not the same cost unit as forward gates.\n"
			<< "# Saved DD total is the shared union of relation and canonical functions; artifact bytes also include the forward graph.\n"
			<< "# Function count includes the reachable-output guard; query time includes that guard and consumes every recovered bit.\n"
			<< "# Selector and validity counts are separate reachable subsets, which may share nodes.\n"
			<< "# Forward construction and concrete target generation are outside build/query timing; affine row uses an already-factorized baseline.\n"
			<< "# Limits report no completed inverse. Timings are measurements, not performance assertions.\n"
			<< "case,unknown_bits,output_bits,forward_nodes,created_DD_nodes,saved_DD_nodes,relation_DD_nodes,function_DD_nodes,selector_DD_nodes,validity_DD_nodes,build_ms,artifact_bytes,ns_per_query,operations,max_nodes,max_operations,status\n";
		reversible_case();
		arithmetic_cases<4>();
		arithmetic_cases<8>();
		crc_cases();
		for (size_t bits : {size_t{4}, size_t{8}, size_t{12}}) md5_case(bits);
		std::cout << "# consumed=" << consumed << '\n';
	}
	catch (const std::exception& error)
	{
		std::cerr << "Synthesis benchmark failure: " << error.what() << '\n';
		return 1;
	}
}
