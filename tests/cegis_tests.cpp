// The counterexample-learned (CEGIS) backend: certified construction within
// its budgets, single-witness queries, and persistence.

#include <cstddef>
#include <cstdint>
#include <initializer_list>
#include <sstream>
#include <stdexcept>
#include <string>

#include "inverse.h"
#include "inverse_support.h"
#include "test_support.h"

namespace br = dixelu::bitreverse;
namespace inv = br::inversion;
using test::require;
using test::to_bits;

namespace
{

// A many-to-one inverse promises some valid witness, not a canonical one.
void check_target(const inv::program& learned, const inv::values& target, const test::models& expected)
{
	const auto witness = learned.evaluate(target, {});
	if (expected.empty())
		require(!witness, "learned inverse accepted an unreachable target");
	else
		require(witness && expected.contains(*witness), "learned inverse returned a nonmember witness");
	size_t callbacks = 0;
	br::solver_statistics statistics{.decisions = 999, .propagations = 999};
	const auto count = learned.solve(target, [&](const inv::values& value)
	{
		++callbacks;
		require(witness && value == *witness, "learned solve disagrees with evaluation");
		return true;
	}, 1, {}, &statistics);
	require(count == callbacks && count == static_cast<size_t>(witness.has_value()),
		"learned solve emitted the wrong number of witnesses");
	require(statistics.decisions == 0 && statistics.propagations == 0, "learned query invoked a search backend");
	if (witness)
	{
		require(learned.solve(target, [](const auto&) { return false; }) == 1,
			"learned solve mishandled callback cancellation");
		require(learned.solve(target, [](const auto&) { return true; }) == 1,
			"learned solve default is not one witness");
	}
}

void check_table(const inv::program& source, const test::table& expected, const inv::cegis_options& options = {})
{
	const auto before = test::save(source);
	inv::cegis_statistics statistics;
	const auto learned = source.learned(options, &statistics);
	require(test::save(source) == before && !source.is_learned(), "learning mutated the source program");
	require(learned.is_learned() && !learned.is_affine() && !learned.is_selected() && !learned.is_synthesized(),
		"learned inverse has more than one backend");
	require(statistics.certified && statistics.failed_phase == "none", "learned inverse has no complete proof");
	require(statistics.solver_calls > 0 && statistics.solver_steps <= options.max_solver_steps &&
		statistics.counterexamples <= options.max_counterexamples && statistics.features <= options.max_features,
		"learning statistics violate the construction bounds");
	require(statistics.candidate_terms == learned.learned_term_count() &&
		statistics.candidate_coefficients == learned.learned_coefficient_count(),
		"learned candidate size disagrees with its statistics");
	const auto loaded = test::roundtrip(learned);
	require(loaded.is_learned() && test::save(loaded) == test::save(learned), "learned artifact did not round trip");
	require(loaded.learned_term_count() == learned.learned_term_count() &&
		loaded.learned_coefficient_count() == learned.learned_coefficient_count(), "loaded candidate size changed");
	for (std::uint64_t target = 0; target < (std::uint64_t{1} << source.output_count()); ++target)
	{
		const auto value = to_bits(target, source.output_count());
		check_target(learned, value, test::preimages(expected, value));
		check_target(loaded, value, test::preimages(expected, value));
	}
	test::check_rejects_malformed_queries(loaded);
	for (const size_t limit : {0, 2, 10})
		test::require_throws<std::logic_error>([&]
		{
			(void)loaded.solve(inv::values(source.output_count()), [](const auto&) { return true; }, limit);
		}, "learned solve pretended to enumerate all preimages");
	std::ostringstream header;
	loaded.export_cpp(header);
	require(header.str().contains("bitreverse_inverse"), "learned export omitted its entry point");
}

// Toffoli gates on each group of three bits: output[2] ^= output[0] & output[1].
inv::program toffoli(size_t width)
{
	const auto input = test::unknown_bits(width);
	auto output = input;
	for (size_t group = 0; group < width; group += 3)
		output[group + 2] = input[group + 2] ^ (input[group] & input[group + 1]);
	return inv::program::compile(input, output);
}

void reversible_cases()
{
	for (const size_t width : {3, 6})
	{
		test::table expected;
		for (std::uint64_t word = 0; word < (std::uint64_t{1} << width); ++word)
		{
			const auto input = to_bits(word, width);
			auto output = input;
			for (size_t group = 0; group < width; group += 3)
				output[group + 2] = input[group + 2] != (input[group] && input[group + 1]);
			expected[output].insert(input);
		}
		check_table(toffoli(width), expected);
	}
	const auto input = test::unknown_bits(3);
	test::table expected;
	for (std::uint64_t word = 0; word < 8; ++word)
	{
		const auto value = to_bits(word, 3);
		expected[{!value[0], value[1] != value[2], value[2]}].insert(value);
	}
	check_table(inv::program::compile(input, {!input[0], input[1] ^ input[2], input[2]}), expected,
		inv::cegis_options{.max_degree = 1});
}

void small_relations()
{
	for (const auto& [program, expected] : test::small_relations())
		check_table(program, expected);
}

void budgets_and_grammar()
{
	const auto source = toffoli(3);
	const auto before = test::save(source);
	for (const auto& options : {inv::cegis_options{.max_features = 1}, inv::cegis_options{.max_counterexamples = 1},
		inv::cegis_options{.max_solver_steps = 1}, inv::cegis_options{.max_nodes = 1}})
	{
		inv::cegis_statistics statistics;
		test::require_throws<inv::cegis_limit>([&] { (void)source.learned(options, &statistics); },
			"CEGIS exceeded an explicit positive budget");
		require(!statistics.certified && statistics.failed_phase != "none", "failed learning reported a complete proof");
		require(test::save(source) == before, "budget failure changed the source");
	}
	for (const auto& options : {inv::cegis_options{.max_features = 0}, inv::cegis_options{.max_counterexamples = 0},
		inv::cegis_options{.max_solver_steps = 0}, inv::cegis_options{.max_nodes = 0},
		inv::cegis_options{.max_degree = 0}, inv::cegis_options{.max_degree = 3}})
	{
		test::require_throws<std::invalid_argument>([&] { (void)source.learned(options); }, "CEGIS accepted invalid options");
		require(test::save(source) == before, "invalid options changed the source");
	}

	inv::cegis_statistics statistics;
	test::require_throws<inv::cegis_limit>([&] { (void)source.learned({.max_degree = 1}, &statistics); },
		"linear grammar represented a Toffoli inverse");
	require(!statistics.certified && statistics.failed_phase != "none", "inconsistent linear grammar reported certification");
	const auto input = test::unknown_bits(4);
	const auto cubic = inv::program::compile(input, {input[0], input[1], input[2], input[3] ^ (input[0] & input[1] & input[2])});
	const auto cubic_before = test::save(cubic);
	test::require_throws<inv::cegis_limit>([&] { (void)cubic.learned({}, &statistics); },
		"quadratic grammar represented a cubic reversible inverse");
	require(!statistics.certified && test::save(cubic) == cubic_before,
		"grammar failure changed the source or certified a partial result");
	const auto learned = source.learned({}, &statistics);
	require(statistics.certified && statistics.failed_phase == "none" && learned.is_learned(),
		"statistics did not reset after a failed build");
}

void persistence_and_backends()
{
	const auto learned = toffoli(3).learned();
	const auto artifact = test::save(learned);
	require(artifact.contains("LEARNED\n") && artifact.contains("CEGIS 1"),
		"learned serialization has no versioned backend marker");
	test::check_artifact(learned);

	const auto selected = learned.selected();
	require(selected.is_selected() && !selected.is_learned() && !selected.is_synthesized() && !selected.is_affine(),
		"selection retained the learned backend");
	const auto synthesized = learned.synthesized();
	require(synthesized.is_synthesized() && !synthesized.is_learned() && !synthesized.is_selected() && !synthesized.is_affine(),
		"synthesis retained the learned backend");
	for (const auto& old : {selected, synthesized})
	{
		const auto relearned = old.learned();
		require(relearned.is_learned() && !relearned.is_selected() && !relearned.is_synthesized() && !relearned.is_affine(),
			"relearning retained another backend");
		for (std::uint64_t target = 0; target < 8; ++target)
			require(relearned.evaluate(to_bits(target, 3), {}) == learned.evaluate(to_bits(target, 3), {}),
				"backend recompilation changed the reversible inverse");
	}
}

void depth_guard()
{
	const auto input = test::unknown_bits(8);
	auto chain = input[0];
	for (size_t step = 0; step < 320; ++step)
		chain = (chain ^ input[(step + 1) % input.size()]) & input[(step + 4) % input.size()];
	const auto source = inv::program::compile(input, {chain});
	const auto before = test::save(source);
	inv::cegis_statistics statistics;
	test::require_throws_with<inv::cegis_limit>([&] { (void)source.learned({}, &statistics); }, "depth",
		"deep nonlinear miter bypassed its construction depth cap");
	require(!statistics.certified && statistics.failed_phase == "verification" && test::save(source) == before,
		"miter depth failure certified a result or changed the source");
}

} // namespace

int main()
{
	return test::run("cegis", {
		{"reversible_cases", reversible_cases},
		{"small_relations", small_relations},
		{"budgets_and_grammar", budgets_and_grammar},
		{"persistence_and_backends", persistence_and_backends},
		{"depth_guard", depth_guard},
	});
}
