#include <algorithm>
#include <array>
#include <bit>
#include <chrono>
#include <cstdint>
#include <iomanip>
#include <iostream>
#include <optional>
#include <set>
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

struct query_set
{
	std::vector<query_sample> valid;
	std::vector<inv::values> invalid;
	std::optional<double> native_forward_ns;
};

// Membership is checked against the complete native-forward image, independently
// of the synthesized inverse, before any rejection queries are timed.
std::vector<inv::values> absent_targets(const std::set<inv::values>& image, size_t width)
{
	std::set<inv::values> selected;
	const inv::values zero(width, false);
	if (!image.contains(zero)) selected.insert(zero);
	std::uint64_t random_state = 0x2f487539b1c6e90dULL;
	for (size_t sample = 0; sample < 128; ++sample)
	{
		inv::values random_target(width);
		for (size_t bit = 0; bit < width; ++bit)
		{
			random_state ^= random_state << 13;
			random_state ^= random_state >> 7;
			random_state ^= random_state << 17;
			random_target[bit] = (random_state & 1U) != 0;
		}
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

template<class Query>
double time_queries(size_t query_count, Query&& query)
{
	if (query_count == 0) throw std::runtime_error("benchmark has no timed queries");
	std::uint64_t checksum = 0;
	size_t count = 0;
	const auto started = clock_type::now();
	do
	{
		for (unsigned batch = 0; batch < 128; ++batch, ++count)
			query(count % query_count, checksum);
	} while (count < 65536 && clock_type::now() - started < std::chrono::milliseconds(40));
	const auto elapsed = std::chrono::duration<double, std::nano>(clock_type::now() - started).count();
	consumed ^= checksum + count;
	return elapsed / static_cast<double>(count);
}

void consume(const std::optional<inv::values>& result, std::uint64_t& checksum)
{
	checksum = checksum * 131U + static_cast<unsigned>(result.has_value());
	if (result)
		for (const bool bit : *result) checksum = checksum * 131U + static_cast<unsigned>(bit);
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
	// Validate independently and compare implementations outside timed loops.
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
	measured.valid_ns = time_queries(queries.valid.size(), [&](size_t index, std::uint64_t& checksum)
	{
		consume(program.evaluate(queries.valid[index].target, free), checksum);
	});
	if (!queries.invalid.empty())
		measured.invalid_ns = time_queries(queries.invalid.size(), [&](size_t index, std::uint64_t& checksum)
		{
			consume(program.evaluate(queries.invalid[index], free), checksum);
		});
	if (program.is_synthesized())
	{
		measured.schedule_valid_ns = time_queries(queries.valid.size(), [&](size_t index, std::uint64_t& checksum)
		{
			consume(program.evaluate_schedule(queries.valid[index].target), checksum);
		});
		if (!queries.invalid.empty())
			measured.schedule_invalid_ns = time_queries(queries.invalid.size(), [&](size_t index, std::uint64_t& checksum)
			{
				consume(program.evaluate_schedule(queries.invalid[index]), checksum);
			});
	}
	return measured;
}

void optional_number(const std::optional<double>& value)
{
	if (value) std::cout << *value;
	else std::cout << "NA";
}

// Scalar fixed-four-byte MD5: padding and message length are constants, native
// uint32_t arithmetic, no bitvector conversion in the timed baseline.
std::array<std::uint32_t, 4> native_md5(std::uint32_t message_word)
{
	constexpr std::array<unsigned, 64> shifts{
		7,12,17,22,7,12,17,22,7,12,17,22,7,12,17,22,
		5,9,14,20,5,9,14,20,5,9,14,20,5,9,14,20,
		4,11,16,23,4,11,16,23,4,11,16,23,4,11,16,23,
		6,10,15,21,6,10,15,21,6,10,15,21,6,10,15,21};
	constexpr std::array<std::uint32_t, 64> constants{
		0xd76aa478,0xe8c7b756,0x242070db,0xc1bdceee,0xf57c0faf,0x4787c62a,0xa8304613,0xfd469501,
		0x698098d8,0x8b44f7af,0xffff5bb1,0x895cd7be,0x6b901122,0xfd987193,0xa679438e,0x49b40821,
		0xf61e2562,0xc040b340,0x265e5a51,0xe9b6c7aa,0xd62f105d,0x02441453,0xd8a1e681,0xe7d3fbc8,
		0x21e1cde6,0xc33707d6,0xf4d50d87,0x455a14ed,0xa9e3e905,0xfcefa3f8,0x676f02d9,0x8d2a4c8a,
		0xfffa3942,0x8771f681,0x6d9d6122,0xfde5380c,0xa4beea44,0x4bdecfa9,0xf6bb4b60,0xbebfbc70,
		0x289b7ec6,0xeaa127fa,0xd4ef3085,0x04881d05,0xd9d4d039,0xe6db99e5,0x1fa27cf8,0xc4ac5665,
		0xf4292244,0x432aff97,0xab9423a7,0xfc93a039,0x655b59c3,0x8f0ccc92,0xffeff47d,0x85845dd1,
		0x6fa87e4f,0xfe2ce6e0,0xa3014314,0x4e0811a1,0xf7537e82,0xbd3af235,0x2ad7d2bb,0xeb86d391};
	const std::array<std::uint32_t, 16> words{message_word, 0x80U, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 32U, 0};
	std::array<std::uint32_t, 4> state{0x67452301,0xefcdab89,0x98badcfe,0x10325476};
	auto a = state[0], b = state[1], c = state[2], d = state[3];
	for (unsigned round = 0; round < 64; ++round)
	{
		std::uint32_t f;
		unsigned word;
		if (round < 16) { f = (b & c) | (~b & d); word = round; }
		else if (round < 32) { f = (d & b) | (~d & c); word = (5 * round + 1) % 16; }
		else if (round < 48) { f = b ^ c ^ d; word = (3 * round + 5) % 16; }
		else { f = c ^ (b | ~d); word = 7 * round % 16; }
		const auto next_b = b + std::rotl(a + f + constants[round] + words[word], static_cast<int>(shifts[round]));
		a = d; d = c; c = b; b = next_b;
	}
	state[0] += a; state[1] += b; state[2] += c; state[3] += d;
	return state;
}

inv::values digest_bits(const std::array<std::uint32_t, 4>& state)
{
	inv::values digest(128);
	for (size_t byte = 0; byte < 16; ++byte)
		for (size_t bit = 0; bit < 8; ++bit)
			digest[(15 - byte) * 8 + bit] = (state[byte / 4] >> (8 * (byte % 4) + bit)) & 1;
	return digest;
}

void validate_native_md5()
{
	// md5("md5!") = b682c4079aa8cae8524986bf8de47f5c.
	if (native_md5(0x2135646dU) != std::array<std::uint32_t, 4>{0x07c482b6U, 0xe8caa89aU, 0xbf864952U, 0x5c7fe48dU})
		throw std::runtime_error("native four-byte MD5 known-vector mismatch");
}

void phase_records(const std::string& name, const inv::synthesis_statistics& statistics)
{
	for (size_t index = 0; index < static_cast<size_t>(inv::synthesis_phase::count); ++index)
	{
		const auto phase = static_cast<inv::synthesis_phase>(index);
		const auto& row = statistics.phases[index];
		std::cout << "# phase," << name << ',' << inv::synthesis_phase_name(phase) << ','
			<< row.started << ',' << row.completed << ','
			<< std::chrono::duration<double, std::milli>(row.elapsed).count() << ','
			<< row.created_nodes << ',' << row.operations << ',' << row.resident_nodes << ',' << row.live_nodes << ','
			<< row.tracked_bytes << ',' << row.peak_tracked_bytes << '\n';
	}
}

template<class MakeQueries>
void measure(const std::string& name, const inv::program& original, MakeQueries&& make_queries,
	bool unique, bool synthesize = true)
{
	// Native forward timing is available even when inverse construction hits its cap.
	const auto queries = make_queries();
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
		const auto build_ms = std::chrono::duration<double, std::milli>(clock_type::now() - started).count();
		std::cout << name << ',' << original.unknown_count() << ',' << original.output_count() << ','
			<< original.node_count() << ',' << statistics.created_nodes << ",NA,NA,NA,NA,NA," << build_ms
			<< ",NA,NA," << statistics.operations << ',' << options.max_nodes << ',' << options.max_operations
			<< ",limit,NA,NA,NA,NA,NA,";
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
			const auto v = integer_bits(input, 3);
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
			result.valid.push_back({integer_bits((input * 3U) & mask, Width), integer_bits(input, Width)});
		return result;
	}, true);
	const auto square = inv::program::compile(inv::bits_of(input), inv::bits_of(input * input));
	measure("square_" + std::to_string(Width), square, []
	{
		query_set result;
		std::set<inv::values> image;
		for (unsigned input = 0; input <= mask; ++input)
		{
			auto target = integer_bits((input * input) & mask, Width);
			image.insert(target);
			result.valid.push_back({std::move(target), integer_bits(input, Width)});
		}
		result.invalid = absent_targets(image, Width);
		return result;
	}, false);
}

std::uint32_t native_crc32(unsigned input)
{
	std::uint32_t checksum = 0xffffffffU ^ input;
	for (unsigned bit = 0; bit < 8; ++bit) checksum = (checksum >> 1) ^ ((checksum & 1) ? 0xedb88320U : 0);
	return ~checksum;
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
			auto target = integer_bits(native_crc32(input), 32);
			image.insert(target);
			result.valid.push_back({std::move(target), integer_bits(input, 8)});
		}
		result.invalid = absent_targets(image, 32);
		result.native_forward_ns = time_queries(256, [](size_t index, std::uint64_t& checksum)
		{
			checksum = checksum * 131U + native_crc32(static_cast<unsigned>(index));
		});
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
		query_set result;
		std::set<inv::values> image;
		std::vector<std::uint32_t> sampled_words;
		const auto domain = size_t{1} << unknown_count;
		const auto samples = std::min(domain, size_t{256});
		for (size_t assignment = 0; assignment < domain; ++assignment)
		{
			std::uint32_t word = 0x0035646dU;
			for (size_t bit = 0; bit < unknown_count; ++bit)
			{
				const auto shift = (3 - bit / 8) * 8 + bit % 8;
				const auto mask = std::uint32_t{1} << shift;
				word = (word & ~mask) | ((assignment >> bit & 1) ? mask : 0);
			}
			auto digest = digest_bits(native_md5(word));
			image.insert(digest);
			if (assignment % (domain / samples) == 0)
			{
				result.valid.push_back({std::move(digest), integer_bits(word, 32)});
				sampled_words.push_back(word);
			}
		}
		result.invalid = absent_targets(image, 128);
		result.native_forward_ns = time_queries(sampled_words.size(), [&](size_t index, std::uint64_t& checksum)
		{
			for (auto word : native_md5(sampled_words[index])) checksum = checksum * 131U + word;
		});
		return result;
	}, true);
}
}

int main()
{
	try
	{
		validate_native_md5();
		std::cout << std::fixed << std::setprecision(3);
		std::cout << "# Reusable inverse synthesis: build once, save/load, then evaluate new outputs.\n"
			<< "# DD nodes are Boolean multiplexers, not the same cost unit as forward gates.\n"
			<< "# Saved DD total is the shared union of relation and canonical functions; artifact bytes also include the forward graph.\n"
			<< "# The first 17 columns retain the original schema; ns_per_query now measures lazy checked evaluation.\n"
			<< "# Lazy/schedule timings both include target validation and consume every recovered bit; visit profiling is untimed.\n"
			<< "# Selector and validity counts are separate reachable subsets, which may share nodes.\n"
			<< "# Invalid targets combine zero, seeded random targets and one-bit mutations, verified against the entire native image.\n"
			<< "# Native forward: scalar fixed-four-byte MD5 / one-byte CRC32, same executable and flags, all output words consumed.\n"
			<< "# Native timing excludes bitvector conversion; inverse timing includes the allocating vector-based public API.\n"
			<< "# Forward construction and native target generation are outside build/query timing; affine is already factorized.\n"
			<< "# Phase tracked bytes count builder PMR allocation requests, not process RSS; source/saved circuits and allocator overhead excluded.\n"
			<< "# Phase live nodes are reachable from phase outputs and are meaningful only for completed phases; peak includes transient data.\n"
			<< "# phase,case,phase,started,completed,elapsed_ms,created_nodes,operations,resident_nodes,live_nodes,tracked_bytes,peak_tracked_bytes\n"
			<< "# Limits report no completed inverse. Timings are measurements, not performance assertions.\n"
			<< "case,unknown_bits,output_bits,forward_nodes,created_DD_nodes,saved_DD_nodes,relation_DD_nodes,function_DD_nodes,selector_DD_nodes,validity_DD_nodes,build_ms,artifact_bytes,ns_per_query,operations,max_nodes,max_operations,status,invalid_ns_per_query,schedule_valid_ns_per_query,schedule_invalid_ns_per_query,valid_node_visits,invalid_node_visits,native_forward_ns_per_query,valid_samples,invalid_samples,failed_phase,peak_tracked_bytes\n";
		reversible_case();
		arithmetic_cases<4>();
		arithmetic_cases<8>();
		crc_cases();
		for (size_t bits : {size_t{4}, size_t{8}, size_t{10}, size_t{12}}) md5_case(bits);
		std::cout << "# consumed=" << consumed << '\n';
	}
	catch (const std::exception& error)
	{
		std::cerr << "Synthesis benchmark failure: " << error.what() << '\n';
		return 1;
	}
}
