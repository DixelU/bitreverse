#include <algorithm>
#include <array>
#include <bit>
#include <chrono>
#include <cstdint>
#include <iomanip>
#include <iostream>
#include <optional>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

#include "benchmark_native_md5.h"
#include "inverse.h"
#include "md5.h"

namespace br = dixelu::bitreverse;
namespace inv = br::inversion;

namespace
{
using clock_type = std::chrono::steady_clock;
using digest = std::array<std::uint32_t, 4>;
std::uint64_t consumed = 0;

inv::values digest_bits(const digest& state)
{
	inv::values result(128);
	for (size_t byte = 0; byte < 16; ++byte)
		for (size_t bit = 0; bit < 8; ++bit)
			result[(15 - byte) * 8 + bit] = (state[byte / 4] >> (8 * (byte % 4) + bit)) & 1;
	return result;
}

std::uint32_t recovered_word(const inv::values& bits)
{
	if (bits.size() != 32) throw std::runtime_error("inverse returned wrong message width");
	std::uint32_t word = 0;
	for (size_t bit = 0; bit < 32; ++bit) word |= static_cast<std::uint32_t>(bits[bit]) << bit;
	return word;
}

std::uint32_t message_word(size_t unknown_bits, size_t assignment)
{
	std::uint32_t word = 0x0035646dU;
	for (size_t bit = 0; bit < unknown_bits; ++bit)
	{
		const size_t shift = (3 - bit / 8) * 8 + bit % 8;
		const auto mask = std::uint32_t{1} << shift;
		word = (word & ~mask) | ((assignment >> bit & 1) ? mask : 0);
	}
	return word;
}

struct table_entry
{
	digest target;
	std::uint32_t input;
};

std::optional<std::uint32_t> table_lookup(const std::vector<table_entry>& table, const digest& target)
{
	const auto entry = std::lower_bound(table.begin(), table.end(), target,
		[](const table_entry& item, const digest& value) { return item.target < value; });
	if (entry != table.end() && entry->target == target) return entry->input;
	return std::nullopt;
}

struct query
{
	digest native_target;
	inv::values target;
	std::uint32_t expected_input{};
};

struct query_sets
{
	std::vector<table_entry> table;
	std::vector<query> valid;
	std::vector<query> random_invalid;
	std::vector<query> near_invalid;
	double table_build_ms{};
};

query_sets make_queries(size_t unknown_bits)
{
	query_sets result;
	const auto started = clock_type::now();
	const size_t domain = size_t{1} << unknown_bits;
	result.table.reserve(domain);
	for (size_t assignment = 0; assignment < domain; ++assignment)
	{
		const auto word = message_word(unknown_bits, assignment);
		result.table.push_back({benchmark_reference::md5_four_bytes(word), word});
	}
	std::sort(result.table.begin(), result.table.end(), [](const table_entry& a, const table_entry& b)
	{
		if (a.target != b.target) return a.target < b.target;
		const auto different = a.input ^ b.input;
		// Input bits are ordered LSB first, so canonical false-first selection
		// compares the lowest differing bit before any higher bit.
		return different != 0 && ((a.input >> std::countr_zero(different)) & 1U) == 0;
	});
	result.table_build_ms = std::chrono::duration<double, std::milli>(clock_type::now() - started).count();
	const size_t samples = std::min(domain, size_t{256});
	for (size_t sample = 0; sample < samples; ++sample)
	{
		const auto& entry = result.table[sample * domain / samples];
		result.valid.push_back({entry.target, digest_bits(entry.target), *table_lookup(result.table, entry.target)});
	}
	std::uint64_t random_state = 0xa16d95b830e7cf42ULL;
	const auto random_word = [&]()
	{
		random_state ^= random_state << 13;
		random_state ^= random_state >> 7;
		random_state ^= random_state << 17;
		return static_cast<std::uint32_t>(random_state);
	};
	// Include all zero, then independently prove every random candidate absent.
	if (!table_lookup(result.table, digest{})) result.random_invalid.push_back({{}, digest_bits({}), 0});
	for (size_t sample = 0; sample < 256; ++sample)
	{
		const digest target{random_word(), random_word(), random_word(), random_word()};
		if (!table_lookup(result.table, target)) result.random_invalid.push_back({target, digest_bits(target), 0});
	}
	for (size_t sample = 0; sample < samples; ++sample)
	{
		auto target = result.valid[sample].native_target;
		target[(sample / 32) % 4] ^= std::uint32_t{1} << (sample % 32);
		if (!table_lookup(result.table, target)) result.near_invalid.push_back({target, digest_bits(target), 0});
	}
	if (result.random_invalid.empty() || result.near_invalid.empty())
		throw std::runtime_error("benchmark failed to generate absent targets");
	return result;
}

template<class Query>
double time_queries(size_t count, Query&& query)
{
	if (count == 0) throw std::runtime_error("no benchmark queries");
	std::uint64_t checksum = 0;
	size_t completed = 0;
	const auto started = clock_type::now();
	do
	{
		for (size_t batch = 0; batch < 64; ++batch, ++completed) query(completed % count, checksum);
	} while (completed < 65536 && clock_type::now() - started < std::chrono::milliseconds(40));
	consumed ^= checksum + completed;
	return std::chrono::duration<double, std::nano>(clock_type::now() - started).count() / static_cast<double>(completed);
}

void consume(const std::optional<inv::values>& result, std::uint64_t& checksum)
{
	checksum = checksum * 131U + static_cast<unsigned>(result.has_value());
	if (result)
		for (const bool bit : *result) checksum = checksum * 131U + static_cast<unsigned>(bit);
}

double inverse_query_time(const inv::program& program, const std::vector<query>& queries)
{
	return time_queries(queries.size(), [&](size_t index, std::uint64_t& checksum)
	{
		consume(program.evaluate(queries[index].target, {}), checksum);
	});
}

double table_query_time(const std::vector<table_entry>& table, const std::vector<query>& queries)
{
	return time_queries(queries.size(), [&](size_t index, std::uint64_t& checksum)
	{
		const auto result = table_lookup(table, queries[index].native_target);
		checksum = checksum * 131U + static_cast<unsigned>(result.has_value());
		if (result) checksum = checksum * 131U + *result;
	});
}

void validate(const inv::program& loaded, const query_sets& queries)
{
	for (const auto& entry : queries.table)
	{
		const auto recovered = loaded.evaluate(digest_bits(entry.target), {});
		if (!recovered || recovered_word(*recovered) != *table_lookup(queries.table, entry.target))
			throw std::runtime_error("saved selector inverse disagrees with independent native MD5 image");
	}
	for (const auto* group : {&queries.random_invalid, &queries.near_invalid})
		for (const auto& item : *group)
			if (loaded.evaluate(item.target, {})) throw std::runtime_error("saved selector inverse accepted an absent MD5 digest");
}

struct query_profile
{
	double decisions{};
	double forward_gates{};
};

query_profile profile_queries(const inv::program& program, const std::vector<query>& queries)
{
	query_profile profile;
	for (const auto& item : queries)
	{
		inv::selector_evaluation_statistics statistics;
		if (program.evaluate_selected(item.target, &statistics) != program.evaluate(item.target, {}))
			throw std::runtime_error("profiled selector differs from default evaluation");
		profile.decisions += statistics.decision_visits;
		profile.forward_gates += statistics.forward_gate_evaluations;
	}
	profile.decisions /= static_cast<double>(queries.size());
	profile.forward_gates /= static_cast<double>(queries.size());
	return profile;
}

void md5_case(size_t unknown_bits)
{
	const auto queries = make_queries(unknown_bits);
	std::vector<br::itu8> message{'m', 'd', '5', 0};
	for (size_t bit = 0; bit < unknown_bits; ++bit) message[3 - bit / 8].bits[7 - bit % 8] = br::unknown;
	const auto forward = inv::program::compile(inv::bits_of(message), inv::bits_of(br::hash::md5(message)));
	inv::selector_options options;
	if (unknown_bits == 20)
	{
		options.max_assignments = size_t{1} << 20;
		options.max_nodes = (size_t{1} << 21) - 1;
		options.max_operations = 2000000000;
	}
	std::cout << "# budgets,md5_" << unknown_bits << ',' << options.max_assignments << ','
		<< options.max_nodes << ',' << options.max_operations << ',' << options.max_bytes << '\n';
	inv::selector_statistics statistics;
	const auto started = clock_type::now();
	const auto selected = forward.selected(options, &statistics);
	const double build_ms = std::chrono::duration<double, std::milli>(clock_type::now() - started).count();
	std::ostringstream saved;
	selected.save(saved);
	const auto artifact = saved.str();
	std::istringstream serialized(artifact);
	const auto loaded = inv::program::load(serialized);
	if (!loaded.is_selected()) throw std::runtime_error("save/load lost selector backend");
	const auto validation_started = clock_type::now();
	validate(loaded, queries);
	const auto validation_ms = std::chrono::duration<double, std::milli>(clock_type::now() - validation_started).count();
	const auto valid_ns = inverse_query_time(loaded, queries.valid);
	const auto random_ns = inverse_query_time(loaded, queries.random_invalid);
	const auto near_ns = inverse_query_time(loaded, queries.near_invalid);
	const auto table_valid_ns = table_query_time(queries.table, queries.valid);
	const auto table_random_ns = table_query_time(queries.table, queries.random_invalid);
	const auto table_near_ns = table_query_time(queries.table, queries.near_invalid);
	const auto native_ns = time_queries(queries.valid.size(), [&](size_t index, std::uint64_t& checksum)
	{
		for (const auto word : benchmark_reference::md5_four_bytes(queries.valid[index].expected_input)) checksum = checksum * 131U + word;
	});
	const auto valid_profile = profile_queries(loaded, queries.valid);
	const auto random_profile = profile_queries(loaded, queries.random_invalid);
	const auto near_profile = profile_queries(loaded, queries.near_invalid);
	std::cout << "md5_" << unknown_bits << ',' << unknown_bits << ',' << forward.node_count() << ',' << queries.table.size()
		<< ',' << build_ms << ',' << selected.selector_node_count() << ',' << selected.selector_leaf_count() << ','
		<< artifact.size() << ',' << valid_ns << ',' << random_ns << ',' << near_ns << ',' << native_ns << ','
		<< queries.table_build_ms << ',' << queries.table.size() * sizeof(table_entry) << ',' << table_valid_ns << ','
		<< table_random_ns << ',' << table_near_ns << ',' << validation_ms << ",ok,"
		<< std::chrono::duration<double, std::milli>(statistics.enumeration_elapsed).count() << ','
		<< std::chrono::duration<double, std::milli>(statistics.tree_elapsed).count() << ','
		<< std::chrono::duration<double, std::milli>(statistics.validation_elapsed).count() << ','
		<< statistics.domain_assignments << ',' << statistics.allowed_assignments << ',' << statistics.distinct_outputs << ','
		<< statistics.operations << ',' << statistics.peak_bytes << ','
		<< valid_profile.decisions << ',' << random_profile.decisions << ',' << near_profile.decisions << ','
		<< valid_profile.forward_gates << ',' << random_profile.forward_gates << ',' << near_profile.forward_gates << '\n';
}
}

int main(int argc, char** argv)
{
	try
	{
		std::vector<size_t> widths{8, 10, 12, 16};
		if (argc > 2) throw std::invalid_argument("Usage: selector_benchmark [8|10|12|16|20]");
		if (argc == 2)
		{
			size_t requested = 0;
			for (const size_t width : {size_t{8}, size_t{10}, size_t{12}, size_t{16}, size_t{20}})
				if (argv[1] == std::to_string(width)) requested = width;
			if (!requested) throw std::invalid_argument("Usage: selector_benchmark [8|10|12|16|20]");
			widths = {requested};
		}
		if (benchmark_reference::md5_four_bytes(0x2135646dU) != digest{0x07c482b6U, 0xe8caa89aU, 0xbf864952U, 0x5c7fe48dU})
			throw std::runtime_error("native MD5 known vector md5! failed");
		std::cout << std::fixed << std::setprecision(3);
		std::cout << "# Exact bounded-domain selector plus forward-check baseline; no decision-diagram relation.\n"
			<< "# Every native domain digest is validated after save/load, outside query timing; random/near invalid targets are independently absent.\n"
			<< "# Candidate query timings use the allocating vector<bool> API and consume every recovered input bit.\n"
			<< "# Native forward and sorted-table lookup use uint32_t words, same build flags, no bitvector conversion; all output words consumed.\n"
			<< "# Table build includes independent native digest generation and sorting; payload bytes include table_entry padding but exclude allocator metadata.\n"
			<< "# Domain enumeration is exponential in unknown bits: successful bounded synthesis alone does not establish structural compression.\n"
			<< "# budgets,case,max_assignments,max_nodes,max_operations,max_bytes\n"
			<< "case,unknown_bits,forward_nodes,domain_rows,build_ms,selector_nodes,selector_leaves,artifact_bytes,valid_ns,random_invalid_ns,near_invalid_ns,native_forward_ns,table_build_ms,table_payload_bytes,table_valid_ns,table_random_invalid_ns,table_near_invalid_ns,exhaustive_validation_ms,status,enumeration_ms,tree_ms,certificate_validation_ms,domain_assignments,allowed_assignments,distinct_outputs,operations,peak_bytes,valid_decision_visits,random_decision_visits,near_decision_visits,valid_forward_gates,random_forward_gates,near_forward_gates\n";
		for (const size_t bits : widths) md5_case(bits);
		std::cout << "# consumed=" << consumed << '\n';
	}
	catch (const std::exception& error)
	{
		std::cerr << "Selector benchmark failure: " << error.what() << '\n';
		return 1;
	}
}
