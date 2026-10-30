#ifndef BITREVERSE_TESTS_INVERSE_SUPPORT_H
#define BITREVERSE_TESTS_INVERSE_SUPPORT_H

// Fixtures and checks shared by the tests of the inverse backends.

#include <cstddef>
#include <cstdint>
#include <map>
#include <set>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "inverse.h"
#include "inverse_benchmark.h"
#include "reference_hashes.h"
#include "test_support.h"

namespace test
{
namespace inv = br::inversion;

using models = std::set<inv::values>;
// Each reachable target with all of its preimages.
using table = std::map<inv::values, models>;

inline std::string save(const inv::program& program)
{
	std::ostringstream stream;
	program.save(stream);
	return stream.str();
}

inline inv::program load(const std::string& artifact)
{
	std::istringstream stream(artifact);
	return inv::program::load(stream);
}

inline inv::program roundtrip(const inv::program& program)
{
	return load(save(program));
}

inline const models& preimages(const table& expected, const inv::values& target)
{
	static const models none;
	const auto found = expected.find(target);
	return found == expected.end() ? none : found->second;
}

// Every preimage of `target`; none may be reported twice.
inline models collect(const inv::program& program, const inv::values& target,
	const br::solver_options& options = {}, br::solver_statistics* statistics = nullptr)
{
	models result;
	const auto count = program.solve(target, [&](const inv::values& value)
	{
		require(result.insert(value).second, "inverse emitted a duplicate input");
		return true;
	}, 0, options, statistics);
	require(count == result.size(), "inverse returned the wrong solution count");
	return result;
}

// collect, for the backends that answer without any search.
inline models collect_directly(const inv::program& program, const inv::values& target)
{
	br::solver_statistics statistics{.decisions = 999, .propagations = 999};
	auto result = collect(program, target, {}, &statistics);
	require(statistics.decisions == 0 && statistics.propagations == 0, "a direct inverse query invoked search");
	return result;
}

// `target` must have at least two preimages.
inline void check_solve_limits(const inv::program& program, const inv::values& target,
	const br::solver_options& options = {})
{
	require(program.solve(target, [](const inv::values&) { return true; }, 2, options) == 2,
		"solve did not honor its solution limit");
	require(program.solve(target, [](const inv::values&) { return false; }, 0, options) == 1,
		"solve did not stop when its callback asked");
}

// Whichever backend answers, malformed queries must fail.
inline void check_rejects_malformed_queries(const inv::program& program)
{
	const inv::values target(program.output_count()), wrong(program.output_count() + 1);
	require_throws<std::invalid_argument>([&] { (void)program.evaluate(wrong, {}); },
		"evaluate accepted a target of the wrong width");
	require_throws<std::invalid_argument>([&] { (void)program.solve(wrong, [](const inv::values&) { return true; }); },
		"solve accepted a target of the wrong width");
	require_throws<std::invalid_argument>([&] { (void)program.solve(target, {}); },
		"solve accepted an empty callback");
	if (program.is_affine())
	{
		require_throws<std::invalid_argument>([&] { (void)program.evaluate(target, inv::values(program.free_bit_count() + 1)); },
			"evaluate accepted free bits of the wrong width");
		return;
	}
	require_throws<std::logic_error>([&] { (void)program.free_bit_count(); },
		"free bits were claimed without a direct affine inverse");
	if (program.is_learned() || program.is_selected() || program.is_synthesized())
		require_throws<std::invalid_argument>([&] { (void)program.evaluate(target, {true}); },
			"evaluate accepted free bits for a single-witness backend");
	else
		require_throws<std::logic_error>([&] { (void)program.evaluate(target, {}); },
			"evaluate without a direct backend must explicitly require search");
}

// A saved artifact must reload to the same bytes, whatever the caller's stream
// flags, and every truncation, every changed digit before the checksum, and
// trailing data must make loading fail.
inline void check_artifact(const inv::program& program)
{
	const auto artifact = save(program);
	require(save(load(artifact)) == artifact, "artifact did not reload to the same bytes");
	std::ostringstream formatted;
	formatted << std::hex << std::showbase;
	program.save(formatted);
	require(formatted.str() == artifact, "saving depends on the stream's numeric flags");

	const auto end = artifact.find("END_INVERSE");
	const auto checksum = artifact.find("CHECKSUM ");
	require(end != std::string::npos && checksum != std::string::npos, "artifact lacks its checksum or end marker");
	for (size_t length = 0; length < end + std::string_view("END_INVERSE").size(); ++length)
		require_throws([&] { (void)load(artifact.substr(0, length)); },
			"artifact truncated to " + std::to_string(length) + " bytes was accepted");
	for (size_t offset = 0; offset < checksum; ++offset)
	{
		if (artifact[offset] < '0' || artifact[offset] > '9')
			continue;
		auto changed = artifact;
		changed[offset] = changed[offset] == '0' ? '1' : '0';
		require_throws([&] { (void)load(changed); },
			"artifact with a changed digit at offset " + std::to_string(offset) + " was accepted");
	}
	require_throws([&] { (void)load(artifact + "trailing garbage"); }, "artifact with trailing data was accepted");
}

struct relation
{
	inv::program program;
	table expected;
};

// What a relation over three unknowns does with one assignment of them: the
// declared input values, the outputs, and whether the requirements hold.
struct sample
{
	inv::values input, output;
	bool allowed = true;
};

template<class Model>
table tabulate(Model&& model)
{
	table expected;
	for (std::uint64_t word = 0; word < 8; ++word)
		if (const sample result = model(to_bits(word, 3)); result.allowed)
			expected[result.output].insert(result.input);
	return expected;
}

// Shared operands, repeated and constant outputs, aliased and constant
// inputs, requirements, and empty inputs or outputs.
inline std::vector<relation> small_relations()
{
	using assignment = const inv::values&;
	const auto x = unknown_bits(3);
	const br::bit_tracker yes(true), no(false);
	return {
		{inv::program::compile(x, {x[0] & x[1], x[0] | x[1], x[0] & x[1]}),
			tabulate([](assignment v) { return sample{v, {v[0] && v[1], v[0] || v[1], v[0] && v[1]}}; })},
		{inv::program::compile(x, {x[0] ^ (x[1] & x[2]), x[1], x[2]}),
			tabulate([](assignment v) { return sample{v, {v[0] != (v[1] && v[2]), v[1], v[2]}}; })},
		{inv::program::compile({x[0], x[0], x[1], yes, x[2]}, {x[0] ^ x[1], yes, x[0] ^ x[1]}),
			tabulate([](assignment v) { return sample{{v[0], v[0], v[1], true, v[2]}, {v[0] != v[1], true, v[0] != v[1]}}; })},
		{inv::program::compile({x[0], x[0], x[1], x[2], yes}, {x[0] & x[1]}),
			tabulate([](assignment v) { return sample{{v[0], v[0], v[1], v[2], true}, {v[0] && v[1]}}; })},
		{inv::program::compile(x, {x[0] ^ x[1]}, {x[0] | x[1], !x[2]}),
			tabulate([](assignment v) { return sample{v, {v[0] != v[1]}, (v[0] || v[1]) && !v[2]}; })},
		{inv::program::compile(x, {x[0] ^ x[0]}),
			tabulate([](assignment v) { return sample{v, {false}}; })},
		{inv::program::compile(x, {}),
			tabulate([](assignment v) { return sample{v, {}}; })},
		{inv::program::compile(x, {}, {x[0], !x[1], x[2]}),
			tabulate([](assignment v) { return sample{v, {}, v[0] && !v[1] && v[2]}; })},
		{inv::program::compile(x, {x[0], x[0] & x[1]}, {no}),
			tabulate([](assignment v) { return sample{v, {v[0], v[0] && v[1]}, false}; })},
		{inv::program::compile(x, {}, {x[0], !x[0]}),
			tabulate([](assignment v) { return sample{v, {}, false}; })},
		{inv::program::compile({}, {}),
			tabulate([](assignment) { return sample{{}, {}}; })},
		{inv::program::compile({}, {yes, no}),
			tabulate([](assignment) { return sample{{}, {true, false}}; })},
	};
}

// x * x, which is many-to-one, and the bijection x * 3 + 5.
template<size_t Width>
std::vector<relation> arithmetic_relations()
{
	using word = br::int_tracker<Width>;
	const word x = br::unknown;
	constexpr std::uint64_t mask = (std::uint64_t{1} << Width) - 1;
	table square, scaled;
	for (std::uint64_t value = 0; value <= mask; ++value)
	{
		square[to_bits(value * value & mask, Width)].insert(to_bits(value, Width));
		scaled[to_bits((value * 3 + 5) & mask, Width)].insert(to_bits(value, Width));
	}
	return {
		{inv::program::compile(inv::bits_of(x), inv::bits_of(x * x)), square},
		{inv::program::compile(inv::bits_of(x), inv::bits_of(x * word{3} + word{5})), scaled}};
}

// MD5 of "md5" and one unknown byte, tabulated with the native MD5.
inline relation md5_relation()
{
	table expected;
	for (std::uint64_t last = 0; last < 256; ++last)
	{
		const auto word = bench::md5_message_word(8, last);
		expected[bench::md5_digest_values(reference::md5_four_bytes(word))].insert(to_bits(word, 32));
	}
	return {bench::md5_program(8), std::move(expected)};
}

// The reachable MD5 targets, plus unreachable ones: the zero digest and every
// single-bit change of one reachable digest.
inline table md5_queries(const table& expected)
{
	auto queries = expected;
	queries.try_emplace(inv::values(128));
	for (size_t bit = 0; bit < 128; ++bit)
	{
		auto changed = expected.begin()->first;
		changed[bit] = !changed[bit];
		queries.try_emplace(changed);
	}
	return queries;
}

} // namespace test

#endif // BITREVERSE_TESTS_INVERSE_SUPPORT_H
