// The synthesized backend: decision diagrams that select a preimage for any
// target directly, with lazy and scheduled evaluation and persistence.

#include <cstddef>
#include <cstdint>
#include <initializer_list>
#include <stdexcept>
#include <vector>

#include "inverse.h"
#include "inverse_support.h"
#include "test_support.h"

namespace br = dixelu::bitreverse;
namespace inv = br::inversion;
using test::require;

namespace
{

void check_target(const inv::program& program, const inv::values& target, const test::models& expected)
{
	const auto canonical = program.evaluate(target, {});
	inv::evaluation_statistics lazy{999, 999, 999}, schedule{999, 999, 999};
	require(program.evaluate_profiled(target, &lazy) == canonical &&
		program.evaluate_schedule(target, &schedule) == canonical,
		"lazy and scheduled evaluation must agree with the canonical result");
	require(lazy.decision_visits == lazy.validity_visits + lazy.selector_visits &&
		schedule.decision_visits == schedule.validity_visits + schedule.selector_visits,
		"evaluation statistics must reset and account for every visit");
	require(schedule.decision_visits == program.synthesized_function_node_count(),
		"schedule baseline must account for the full retained function graph");
	require(lazy.validity_visits <= program.output_count() &&
		lazy.selector_visits <= program.unknown_count() * program.output_count(),
		"lazy evaluation must follow bounded target paths without input search");
	if (expected.empty())
	{
		require(!canonical, "canonical synthesis accepted an unreachable output");
		require(lazy.selector_visits == 0, "unreachable targets must stop before input selection");
	}
	else
		require(canonical && *canonical == *expected.begin(),
			"canonical synthesis did not choose the false-first matching input");
	require(test::collect_directly(program, target) == expected,
		"synthesized preimages disagree with independent forward truth table");
}

void check_table(const inv::program& original, const test::table& expected)
{
	inv::synthesis_statistics statistics;
	const auto synthesized = original.synthesized({}, &statistics);
	size_t created = 0, operations = 0;
	for (const auto& phase : statistics.phases)
	{
		require(phase.started && phase.completed, "successful synthesis must complete every profiled phase");
		created += phase.created_nodes;
		operations += phase.operations;
		require(phase.resident_nodes == created && phase.live_nodes <= phase.resident_nodes,
			"phase graph counts must distinguish cumulative creation from reachable output nodes");
		require(phase.tracked_bytes <= phase.peak_tracked_bytes &&
			phase.peak_tracked_bytes <= statistics.peak_tracked_bytes,
			"tracked allocation peaks must bound phase allocations");
	}
	require(created == statistics.created_nodes && operations == statistics.operations &&
		statistics.failed_phase == inv::synthesis_phase::count,
		"phase statistics must account for the complete successful build");
	require(synthesized.is_synthesized(), "synthesis did not select the direct nonlinear backend");
	require(!original.is_synthesized(), "synthesis must leave the original program unchanged");
	const auto loaded = test::roundtrip(synthesized);
	require(loaded.is_synthesized(), "saved synthesis lost its backend");
	require(loaded.synthesized_node_count() == synthesized.synthesized_node_count() &&
		loaded.synthesized_function_node_count() == synthesized.synthesized_function_node_count() &&
		loaded.synthesized_relation_node_count() == synthesized.synthesized_relation_node_count() &&
		loaded.synthesized_selector_node_count() == synthesized.synthesized_selector_node_count() &&
		loaded.synthesized_validity_node_count() == synthesized.synthesized_validity_node_count(),
		"synthesis node counts changed after persistence");
	require(original.output_count() < 16, "exhaustive output test is too large");
	for (std::uint64_t output = 0; output < (std::uint64_t{1} << original.output_count()); ++output)
	{
		const auto target = test::to_bits(output, original.output_count());
		check_target(synthesized, target, test::preimages(expected, target));
		check_target(loaded, target, test::preimages(expected, target));
	}
	test::check_rejects_malformed_queries(loaded);
	const inv::values wrong(original.output_count() + 1);
	inv::evaluation_statistics rejected{999, 999, 999};
	test::require_throws<std::invalid_argument>([&] { (void)loaded.evaluate_profiled(wrong, &rejected); },
		"profiled evaluation must reject the wrong target width");
	require(rejected.decision_visits == 0, "rejected query must clear previous visit counts");
	test::require_throws<std::invalid_argument>([&] { (void)loaded.evaluate_schedule(wrong); },
		"schedule baseline must reject the wrong target width");
}

void small_relations()
{
	for (const auto& [program, expected] : test::small_relations())
		check_table(program, expected);
	const auto x = test::unknown_bits(3);
	const auto shared = test::roundtrip(inv::program::compile(x, {x[0] & x[1], x[0] | x[1], x[0] & x[1]}).synthesized());
	test::check_solve_limits(shared, {false, true, false});
}

template<size_t Width>
void arithmetic()
{
	for (const auto& [program, expected] : test::arithmetic_relations<Width>())
		check_table(program, expected);
}

void random_circuits()
{
	test::random_engine random(0x6fbd2731);
	for (unsigned trial = 0; trial < 24; ++trial)
	{
		const test::random_circuit circuit(random, 32, false);
		inv::bits outputs;
		std::vector<std::uint64_t> output_masks;
		for (unsigned output = 0; output < 5; ++output)
		{
			const auto pick = random() % circuit.expressions.size();
			outputs.push_back(circuit.expressions[pick]);
			output_masks.push_back(circuit.masks[pick]);
		}
		const auto restriction = random() % circuit.expressions.size();
		const bool restricted = trial % 3 == 0;
		test::table expected;
		for (std::uint64_t input = 0; input < 64; ++input)
		{
			if (restricted && !((circuit.masks[restriction] >> input) & 1U))
				continue;
			inv::values output;
			for (const auto mask : output_masks)
				output.push_back(((mask >> input) & 1U) != 0);
			expected[output].insert(test::to_bits(input, 6));
		}
		check_table(inv::program::compile(circuit.inputs, outputs,
			restricted ? inv::bits{circuit.expressions[restriction]} : inv::bits{}), expected);
	}
}

void limits_and_artifact()
{
	const auto x = test::unknown_bits(3);
	const auto original = inv::program::compile(x, {x[0] ^ (x[1] & x[2]), x[1], x[2]});
	for (const auto& options : {inv::synthesis_options{.max_nodes = 2}, inv::synthesis_options{.max_operations = 1}})
	{
		inv::synthesis_statistics statistics;
		test::require_throws<inv::synthesis_limit>([&] { (void)original.synthesized(options, &statistics); },
			"exceeding a synthesis budget must fail explicitly");
		require(statistics.failed_phase == inv::synthesis_phase::forward &&
			statistics.phases[0].started && !statistics.phases[0].completed && !statistics.phases[1].started,
			"budget failure must identify the interrupted phase without marking later phases started");
		require(!original.is_synthesized(), "failed synthesis must not modify the original");
		inv::values recovered;
		require(original.solve({true, true, true}, [&](const auto& value) { recovered = value; return true; }) == 1 &&
			recovered == inv::values{false, true, true}, "original solver broke after failed synthesis");
	}
	const auto synthesized = original.synthesized();
	require(test::save(synthesized).contains("SYNTHESIZED\n"), "saved synthesis is missing its backend marker");
	test::check_artifact(synthesized);
}

void md5()
{
	const auto [program, expected] = test::md5_relation();
	require(program.unknown_count() == 8 && program.input_count() == 32, "MD5 prefix must be baked into the circuit");
	const auto synthesized = program.synthesized();
	const auto loaded = test::roundtrip(synthesized);
	for (const auto& [target, inputs] : test::md5_queries(expected))
	{
		check_target(loaded, target, inputs);
		if (inputs.empty())
			continue;
		check_target(synthesized, target, inputs);
		inv::evaluation_statistics visits;
		(void)loaded.evaluate_profiled(target, &visits);
		require(visits.decision_visits < loaded.synthesized_function_node_count() / 10,
			"MD5 path evaluation must avoid scanning the much larger stored graph");
	}
}

} // namespace

int main()
{
	return test::run("synthesis", {
		{"small_relations", small_relations},
		{"arithmetic<4>", arithmetic<4>},
		{"arithmetic<8>", arithmetic<8>},
		{"random_circuits", random_circuits},
		{"limits_and_artifact", limits_and_artifact},
		{"md5", md5},
	});
}
