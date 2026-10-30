#include <algorithm>
#include <cstdint>
#include <iomanip>
#include <iostream>
#include <optional>
#include <set>
#include <stdexcept>
#include <string>
#include <vector>

#include "crc32.h"
#include "inverse.h"
#include "inverse_benchmark.h"
#include "reference_hashes.h"
#include "timing.h"

namespace br = dixelu::bitreverse;
namespace inv = br::inversion;

namespace
{
struct query_sample
{
	inv::values target;
	inv::values original_input;
};

struct query_set
{
	std::vector<query_sample> valid;
	std::vector<inv::values> invalid;
	std::optional<double> native_forward_ns;
};

// Zero, seeded random targets and one-bit mutations of reachable targets,
// each checked against the complete native image, never the inverse.
std::vector<inv::values> absent_targets(const std::set<inv::values>& image, size_t width)
{
	std::set<inv::values> selected;
	const inv::values zero(width, false);
	if (!image.contains(zero)) selected.insert(zero);
	bench::xorshift64 random{0x2f487539b1c6e90dULL};
	for (size_t sample = 0; sample < 128; ++sample)
	{
		inv::values random_target(width);
		for (size_t bit = 0; bit < width; ++bit) random_target[bit] = (random() & 1U) != 0;
		if (!image.contains(random_target)) selected.insert(std::move(random_target));
	}
	for (const auto& target : image)
	{
		for (size_t bit = 0; bit < width && selected.size() < 256; ++bit)
		{
			auto changed = target;
			changed[bit] = !changed[bit];
			if (!image.contains(changed)) selected.insert(std::move(changed));
		}
		if (selected.size() == 256) break;
	}
	return {selected.begin(), selected.end()};
}

struct evaluation_measurement
{
	double valid_ns{};
	std::optional<double> invalid_ns;
	std::optional<double> schedule_valid_ns;
	std::optional<double> schedule_invalid_ns;
	double valid_visits{};
	double invalid_visits{};
};

evaluation_measurement query_times(const inv::program& program, const query_set& queries, bool require_unique)
{
	if (queries.valid.empty()) throw std::runtime_error("benchmark has no valid queries");
	const auto free = program.is_synthesized() ? inv::values{} : inv::values(program.free_bit_count());
	evaluation_measurement measured;
	// Validation and the untimed visit profile run before any timed loop.
	for (const auto& query : queries.valid)
	{
		const auto result = program.evaluate(query.target, free);
		if (!result || (require_unique && *result != query.original_input))
			throw std::runtime_error("benchmark inverse failed recovery validation");
		if (program.is_synthesized())
		{
			if (program.evaluate_schedule(query.target) != result)
				throw std::runtime_error("schedule and lazy inverse disagree on a valid target");
			inv::evaluation_statistics statistics;
			if (program.evaluate_profiled(query.target, &statistics) != result)
				throw std::runtime_error("profiled and unprofiled inverse disagree");
			measured.valid_visits += statistics.decision_visits;
		}
	}
	for (const auto& target : queries.invalid)
	{
		if (program.evaluate(target, free)) throw std::runtime_error("inverse accepted an independently absent target");
		if (program.is_synthesized())
		{
			if (program.evaluate_schedule(target)) throw std::runtime_error("schedule accepted an absent target");
			inv::evaluation_statistics statistics;
			if (program.evaluate_profiled(target, &statistics)) throw std::runtime_error("profile accepted an absent target");
			if (statistics.selector_visits != 0) throw std::runtime_error("lazy rejection unnecessarily evaluated selector nodes");
			measured.invalid_visits += statistics.decision_visits;
		}
	}
	measured.valid_visits /= static_cast<double>(queries.valid.size());
	if (!queries.invalid.empty()) measured.invalid_visits /= static_cast<double>(queries.invalid.size());
	measured.valid_ns = bench::time_queries(queries.valid.size(), [&](size_t index, std::uint64_t& checksum)
	{
		bench::consume(checksum, program.evaluate(queries.valid[index].target, free));
	});
	if (!queries.invalid.empty())
		measured.invalid_ns = bench::time_queries(queries.invalid.size(), [&](size_t index, std::uint64_t& checksum)
		{
			bench::consume(checksum, program.evaluate(queries.invalid[index], free));
		});
	if (program.is_synthesized())
	{
		measured.schedule_valid_ns = bench::time_queries(queries.valid.size(), [&](size_t index, std::uint64_t& checksum)
		{
			bench::consume(checksum, program.evaluate_schedule(queries.valid[index].target));
		});
		if (!queries.invalid.empty())
			measured.schedule_invalid_ns = bench::time_queries(queries.invalid.size(), [&](size_t index, std::uint64_t& checksum)
			{
				bench::consume(checksum, program.evaluate_schedule(queries.invalid[index]));
			});
	}
	return measured;
}

void optional_number(const std::optional<double>& value)
{
	if (value) std::cout << *value;
	else std::cout << "NA";
}

void phase_records(const std::string& name, const inv::synthesis_statistics& statistics)
{
	for (size_t index = 0; index < static_cast<size_t>(inv::synthesis_phase::count); ++index)
	{
		const auto phase = static_cast<inv::synthesis_phase>(index);
		const auto& row = statistics.phases[index];
		std::cout << "# phase," << name << ',' << inv::synthesis_phase_name(phase) << ','
			<< row.started << ',' << row.completed << ',' << support::milliseconds(row.elapsed) << ','
			<< row.created_nodes << ',' << row.operations << ',' << row.resident_nodes << ',' << row.live_nodes << ','
			<< row.tracked_bytes << ',' << row.peak_tracked_bytes << '\n';
	}
}

// Without `synthesize`, measures the source program itself as a baseline.
template<class MakeQueries>
void measure(const std::string& name, const inv::program& original, MakeQueries&& make_queries,
	bool unique, bool synthesize = true)
{
	// Native forward timing is available even when inverse construction hits its cap.
	const auto queries = make_queries();
	inv::synthesis_options options;
	inv::synthesis_statistics statistics;
	const auto started = support::clock::now();
	try
	{
		const auto built = synthesize ? original.synthesized(options, &statistics) : original;
		const auto build_ms = support::milliseconds_since(started);
		const auto [artifact, loaded] = bench::save_and_load(built);
		const auto measured = query_times(loaded, queries, unique);
		std::cout << name << ',' << original.unknown_count() << ',' << original.output_count() << ','
			<< original.node_count() << ',' << statistics.created_nodes << ',';
		if (synthesize)
			std::cout << built.synthesized_node_count() << ',' << built.synthesized_relation_node_count() << ','
				<< built.synthesized_function_node_count() << ',' << built.synthesized_selector_node_count() << ','
				<< built.synthesized_validity_node_count();
		else std::cout << "NA,NA,NA,NA,NA";
		std::cout << ',' << build_ms << ',' << artifact.size() << ',' << measured.valid_ns << ','
			<< statistics.operations << ',' << options.max_nodes << ',' << options.max_operations << ",ok,";
		optional_number(measured.invalid_ns);
		std::cout << ','; optional_number(measured.schedule_valid_ns);
		std::cout << ','; optional_number(measured.schedule_invalid_ns);
		std::cout << ',';
		if (synthesize) std::cout << measured.valid_visits; else std::cout << "NA";
		std::cout << ',';
		if (synthesize && !queries.invalid.empty()) std::cout << measured.invalid_visits; else std::cout << "NA";
		std::cout << ','; optional_number(queries.native_forward_ns);
		std::cout << ',' << queries.valid.size() << ',' << queries.invalid.size() << ",none," << statistics.peak_tracked_bytes << '\n';
		if (synthesize) phase_records(name, statistics);
	}
	catch (const inv::synthesis_limit& error)
	{
		std::cout << name << ',' << original.unknown_count() << ',' << original.output_count() << ','
			<< original.node_count() << ',' << statistics.created_nodes << ",NA,NA,NA,NA,NA,"
			<< support::milliseconds_since(started) << ",NA,NA," << statistics.operations << ','
			<< options.max_nodes << ',' << options.max_operations << ",limit,NA,NA,NA,NA,NA,";
		optional_number(queries.native_forward_ns);
		std::cout << ',' << queries.valid.size() << ',' << queries.invalid.size() << ','
			<< inv::synthesis_phase_name(statistics.failed_phase) << ',' << statistics.peak_tracked_bytes << '\n';
		phase_records(name, statistics);
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
		query_set result;
		for (unsigned input = 0; input < 8; ++input)
		{
			const auto v = bench::to_values(input, 3);
			result.valid.push_back({{v[0] != (v[1] && v[2]), v[1], v[2]}, v});
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
		query_set result;
		for (unsigned input = 0; input <= mask; ++input)
			result.valid.push_back({bench::to_values((input * 3U) & mask, Width), bench::to_values(input, Width)});
		return result;
	}, true);
	const auto square = inv::program::compile(inv::bits_of(input), inv::bits_of(input * input));
	measure("square_" + std::to_string(Width), square, []
	{
		query_set result;
		std::set<inv::values> image;
		for (unsigned input = 0; input <= mask; ++input)
		{
			auto target = bench::to_values((input * input) & mask, Width);
			image.insert(target);
			result.valid.push_back({std::move(target), bench::to_values(input, Width)});
		}
		result.invalid = absent_targets(image, Width);
		return result;
	}, false);
}

std::uint32_t native_crc32(std::uint8_t byte)
{
	return ~reference::crc32_update(0xffffffffU, byte);
}

void crc_cases()
{
	std::vector<br::itu8> message;
	message.emplace_back(br::unknown);
	const auto original = inv::program::compile(inv::bits_of(message), inv::bits_of(br::hash::crc32(message)));
	const auto queries = []
	{
		query_set result;
		std::set<inv::values> image;
		for (unsigned input = 0; input < 256; ++input)
		{
			auto target = bench::to_values(native_crc32(static_cast<std::uint8_t>(input)), 32);
			image.insert(target);
			result.valid.push_back({std::move(target), bench::to_values(input, 8)});
		}
		result.invalid = absent_targets(image, 32);
		result.native_forward_ns = bench::time_queries(256, [](size_t index, std::uint64_t& checksum)
		{
			bench::consume(checksum, native_crc32(static_cast<std::uint8_t>(index)));
		});
		return result;
	};
	measure("crc32_8_affine", original, queries, true, false);
	measure("crc32_8_synthesized", original, queries, true);
}

void md5_case(size_t unknown_count)
{
	measure("md5_" + std::to_string(unknown_count), bench::md5_program(unknown_count), [unknown_count]
	{
		query_set result;
		std::set<inv::values> image;
		std::vector<std::uint32_t> sampled_words;
		const auto domain = size_t{1} << unknown_count;
		const auto samples = std::min(domain, size_t{256});
		for (size_t assignment = 0; assignment < domain; ++assignment)
		{
			const auto word = bench::md5_message_word(unknown_count, assignment);
			auto digest = bench::md5_digest_values(reference::md5_four_bytes(word));
			image.insert(digest);
			if (assignment % (domain / samples) == 0)
			{
				result.valid.push_back({std::move(digest), bench::to_values(word, 32)});
				sampled_words.push_back(word);
			}
		}
		result.invalid = absent_targets(image, 128);
		result.native_forward_ns = bench::time_queries(sampled_words.size(), [&](size_t index, std::uint64_t& checksum)
		{
			for (const auto word : reference::md5_four_bytes(sampled_words[index])) bench::consume(checksum, word);
		});
		return result;
	}, true);
}
}

int main()
{
	try
	{
		bench::check_md5_reference();
		std::cout << std::fixed << std::setprecision(3)
			<< "# Inverse synthesis: build once, save/load, then time valid and independently absent targets.\n"
			<< "# DD counts are decision-diagram nodes; native_forward times scalar CRC32/MD5. Timings are not assertions.\n"
			<< "# phase,case,phase,started,completed,elapsed_ms,created_nodes,operations,resident_nodes,live_nodes,tracked_bytes,peak_tracked_bytes\n"
			<< "case,unknown_bits,output_bits,forward_nodes,created_DD_nodes,saved_DD_nodes,relation_DD_nodes,function_DD_nodes,selector_DD_nodes,validity_DD_nodes,build_ms,artifact_bytes,ns_per_query,operations,max_nodes,max_operations,status,invalid_ns_per_query,schedule_valid_ns_per_query,schedule_invalid_ns_per_query,valid_node_visits,invalid_node_visits,native_forward_ns_per_query,valid_samples,invalid_samples,failed_phase,peak_tracked_bytes\n";
		reversible_case();
		arithmetic_cases<4>();
		arithmetic_cases<8>();
		crc_cases();
		for (size_t bits : {size_t{4}, size_t{8}, size_t{10}, size_t{12}}) md5_case(bits);
		std::cout << "# consumed=" << bench::consumed << '\n';
	}
	catch (const std::exception& error)
	{
		std::cerr << "Synthesis benchmark failure: " << error.what() << '\n';
		return 1;
	}
}
