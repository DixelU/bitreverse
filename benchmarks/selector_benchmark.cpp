#include <algorithm>
#include <array>
#include <bit>
#include <cstdint>
#include <iomanip>
#include <iostream>
#include <optional>
#include <stdexcept>
#include <string>
#include <vector>

#include "inverse.h"
#include "inverse_benchmark.h"
#include "reference_hashes.h"
#include "timing.h"

namespace br = dixelu::bitreverse;
namespace inv = br::inversion;

namespace
{
using digest = std::array<std::uint32_t, 4>;

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
	const auto started = support::clock::now();
	const size_t domain = size_t{1} << unknown_bits;
	result.table.reserve(domain);
	for (size_t assignment = 0; assignment < domain; ++assignment)
	{
		const auto word = bench::md5_message_word(unknown_bits, assignment);
		result.table.push_back({reference::md5_four_bytes(word), word});
	}
	std::sort(result.table.begin(), result.table.end(), [](const table_entry& a, const table_entry& b)
	{
		if (a.target != b.target) return a.target < b.target;
		const auto different = a.input ^ b.input;
		// Input bits are ordered LSB first, so canonical false-first selection
		// compares the lowest differing bit before any higher bit.
		return different != 0 && ((a.input >> std::countr_zero(different)) & 1U) == 0;
	});
	result.table_build_ms = support::milliseconds_since(started);
	const size_t samples = std::min(domain, size_t{256});
	for (size_t sample = 0; sample < samples; ++sample)
	{
		const auto& entry = result.table[sample * domain / samples];
		result.valid.push_back({entry.target, bench::md5_digest_values(entry.target), *table_lookup(result.table, entry.target)});
	}
	bench::xorshift64 random{0xa16d95b830e7cf42ULL};
	const auto random_word = [&] { return static_cast<std::uint32_t>(random()); };
	// The zero digest, random digests and one-bit mutations, each proven absent from the native table.
	if (!table_lookup(result.table, digest{})) result.random_invalid.push_back({{}, bench::md5_digest_values({}), 0});
	for (size_t sample = 0; sample < 256; ++sample)
	{
		const digest target{random_word(), random_word(), random_word(), random_word()};
		if (!table_lookup(result.table, target)) result.random_invalid.push_back({target, bench::md5_digest_values(target), 0});
	}
	for (size_t sample = 0; sample < samples; ++sample)
	{
		auto target = result.valid[sample].native_target;
		target[(sample / 32) % 4] ^= std::uint32_t{1} << (sample % 32);
		if (!table_lookup(result.table, target)) result.near_invalid.push_back({target, bench::md5_digest_values(target), 0});
	}
	if (result.random_invalid.empty() || result.near_invalid.empty())
		throw std::runtime_error("benchmark failed to generate absent targets");
	return result;
}

double inverse_query_time(const inv::program& program, const std::vector<query>& queries)
{
	return bench::time_queries(queries.size(), [&](size_t index, std::uint64_t& checksum)
	{
		bench::consume(checksum, program.evaluate(queries[index].target, {}));
	});
}

double table_query_time(const std::vector<table_entry>& table, const std::vector<query>& queries)
{
	return bench::time_queries(queries.size(), [&](size_t index, std::uint64_t& checksum)
	{
		const auto result = table_lookup(table, queries[index].native_target);
		bench::consume(checksum, result.has_value());
		if (result) bench::consume(checksum, *result);
	});
}

void validate(const inv::program& loaded, const query_sets& queries)
{
	for (const auto& entry : queries.table)
	{
		const auto recovered = loaded.evaluate(bench::md5_digest_values(entry.target), {});
		if (!recovered || bench::to_integer(*recovered, 32) != *table_lookup(queries.table, entry.target))
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
	const auto forward = bench::md5_program(unknown_bits);
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
	const auto started = support::clock::now();
	const auto selected = forward.selected(options, &statistics);
	const double build_ms = support::milliseconds_since(started);
	const auto [artifact, loaded] = bench::save_and_load(selected);
	if (!loaded.is_selected()) throw std::runtime_error("save/load lost selector backend");
	const auto validation_started = support::clock::now();
	validate(loaded, queries);
	const auto validation_ms = support::milliseconds_since(validation_started);
	const auto valid_ns = inverse_query_time(loaded, queries.valid);
	const auto random_ns = inverse_query_time(loaded, queries.random_invalid);
	const auto near_ns = inverse_query_time(loaded, queries.near_invalid);
	const auto table_valid_ns = table_query_time(queries.table, queries.valid);
	const auto table_random_ns = table_query_time(queries.table, queries.random_invalid);
	const auto table_near_ns = table_query_time(queries.table, queries.near_invalid);
	const auto native_ns = bench::time_queries(queries.valid.size(), [&](size_t index, std::uint64_t& checksum)
	{
		for (const auto word : reference::md5_four_bytes(queries.valid[index].expected_input)) bench::consume(checksum, word);
	});
	const auto valid_profile = profile_queries(loaded, queries.valid);
	const auto random_profile = profile_queries(loaded, queries.random_invalid);
	const auto near_profile = profile_queries(loaded, queries.near_invalid);
	std::cout << "md5_" << unknown_bits << ',' << unknown_bits << ',' << forward.node_count() << ',' << queries.table.size()
		<< ',' << build_ms << ',' << selected.selector_node_count() << ',' << selected.selector_leaf_count() << ','
		<< artifact.size() << ',' << valid_ns << ',' << random_ns << ',' << near_ns << ',' << native_ns << ','
		<< queries.table_build_ms << ',' << queries.table.size() * sizeof(table_entry) << ',' << table_valid_ns << ','
		<< table_random_ns << ',' << table_near_ns << ',' << validation_ms << ",ok,"
		<< support::milliseconds(statistics.enumeration_elapsed) << ','
		<< support::milliseconds(statistics.tree_elapsed) << ','
		<< support::milliseconds(statistics.validation_elapsed) << ','
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
		const std::invalid_argument usage("Usage: selector_benchmark [8|10|12|16|20]");
		std::vector<size_t> widths{8, 10, 12, 16};
		if (argc > 2) throw usage;
		if (argc == 2)
		{
			size_t requested = 0;
			for (const size_t width : {size_t{8}, size_t{10}, size_t{12}, size_t{16}, size_t{20}})
				if (argv[1] == std::to_string(width)) requested = width;
			if (!requested) throw usage;
			widths = {requested};
		}
		bench::check_md5_reference();
		std::cout << std::fixed << std::setprecision(3)
			<< "# Bounded-domain selector with forward check, validated over the whole domain after save/load;\n"
			<< "# baselines are native MD5 and a sorted native lookup table. Domain enumeration is exponential in unknown bits.\n"
			<< "# budgets,case,max_assignments,max_nodes,max_operations,max_bytes\n"
			<< "case,unknown_bits,forward_nodes,domain_rows,build_ms,selector_nodes,selector_leaves,artifact_bytes,valid_ns,random_invalid_ns,near_invalid_ns,native_forward_ns,table_build_ms,table_payload_bytes,table_valid_ns,table_random_invalid_ns,table_near_invalid_ns,exhaustive_validation_ms,status,enumeration_ms,tree_ms,certificate_validation_ms,domain_assignments,allowed_assignments,distinct_outputs,operations,peak_bytes,valid_decision_visits,random_decision_visits,near_decision_visits,valid_forward_gates,random_forward_gates,near_forward_gates\n";
		for (const size_t bits : widths) md5_case(bits);
		std::cout << "# consumed=" << bench::consumed << '\n';
	}
	catch (const std::exception& error)
	{
		std::cerr << "Selector benchmark failure: " << error.what() << '\n';
		return 1;
	}
}
