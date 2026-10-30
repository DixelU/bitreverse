// The selector backend: a decision tree over the target bits, certified
// against the entire input domain.

#include <cstddef>
#include <cstdint>
#include <initializer_list>
#include <ranges>

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
	inv::selector_evaluation_statistics visits{999, 999};
	const auto result = program.evaluate_selected(target, &visits);
	require(result == program.evaluate(target, {}), "profiled selector disagrees with default evaluation");
	if (expected.empty())
		require(!result, "selector accepted an unreachable target");
	else
		require(result && *result == *expected.begin(), "selector did not choose false-first canonical input");
	require(visits.decision_visits <= program.output_count(), "selector revisits target bits on a path");
	require(visits.forward_gate_evaluations <= program.selector_forward_node_count(),
		"selector evaluated more than one forward pass");
	require(test::collect_directly(program, target) == expected, "selector lost or invented preimages");
	if (!expected.empty())
	{
		require(program.solve(target, [](const auto&) { return false; }, 0) == 1,
			"selector ignored callback cancellation");
		require(program.solve(target, [](const auto&) { return true; }, 1) == 1,
			"selector ignored the result limit");
	}
}

void check_table(const inv::program& source, const test::table& expected)
{
	const auto original = test::save(source);
	inv::selector_statistics statistics;
	const auto selected = source.selected({}, &statistics);
	require(test::save(source) == original && !source.is_selected(), "selector compilation mutated the source");
	require(selected.is_selected() && !selected.is_synthesized() && !selected.is_affine(),
		"selector backend must be unambiguous");
	size_t allowed = 0;
	for (const auto& inputs : expected | std::views::values)
		allowed += inputs.size();
	require(statistics.domain_assignments == (size_t{1} << source.unknown_count()) &&
		statistics.allowed_assignments == allowed && statistics.distinct_outputs == expected.size(),
		"selector certification must cover the entire input domain");
	require(statistics.failed_phase == "none" && selected.selector_assignment_count() == allowed &&
		selected.selector_leaf_count() == expected.size(), "selector groups disagree with the exact forward image");
	const auto loaded = test::roundtrip(selected);
	require(loaded.is_selected() && test::save(loaded) == test::save(selected), "selector persistence is not stable");
	for (std::uint64_t target = 0; target < (std::uint64_t{1} << source.output_count()); ++target)
	{
		const auto value = test::to_bits(target, source.output_count());
		check_target(selected, value, test::preimages(expected, value));
		check_target(loaded, value, test::preimages(expected, value));
	}
	test::check_rejects_malformed_queries(loaded);
	const auto resynthesized = loaded.synthesized();
	require(resynthesized.is_synthesized() && !resynthesized.is_selected(), "resynthesis retained two active backends");
	const auto reselected = resynthesized.selected();
	require(reselected.is_selected() && !reselected.is_synthesized(), "reselection retained the old backend");
}

void small_relations()
{
	for (const auto& [program, expected] : test::small_relations())
		check_table(program, expected);
}

template<size_t Width>
void arithmetic()
{
	for (const auto& [program, expected] : test::arithmetic_relations<Width>())
		check_table(program, expected);
}

void budgets_and_artifact()
{
	const br::itu8 x = br::unknown;
	const auto original = inv::program::compile(inv::bits_of(x), inv::bits_of(x * br::itu8{3}));
	const auto untouched = test::save(original);
	for (const auto& options : {inv::selector_options{.max_assignments = 1}, inv::selector_options{.max_nodes = 1},
		inv::selector_options{.max_operations = 1}, inv::selector_options{.max_bytes = 1}})
	{
		inv::selector_statistics statistics;
		test::require_throws<inv::selector_limit>([&] { (void)original.selected(options, &statistics); },
			"selector exceeded an explicit construction budget");
		require(statistics.failed_phase != "none", "selector failure omitted its phase");
		require(test::save(original) == untouched, "failed selector build mutated the original program");
	}
	const auto selected = original.selected();
	require(test::save(selected).contains("SELECTOR\n"), "saved selector has no backend marker");
	test::check_artifact(selected);
}

void md5()
{
	const auto [program, expected] = test::md5_relation();
	const auto loaded = test::roundtrip(program.selected());
	for (const auto& [target, inputs] : test::md5_queries(expected))
		check_target(loaded, target, inputs);
}

} // namespace

int main()
{
	return test::run("selector", {
		{"small_relations", small_relations},
		{"arithmetic<4>", arithmetic<4>},
		{"arithmetic<8>", arithmetic<8>},
		{"budgets_and_artifact", budgets_and_artifact},
		{"md5", md5},
	});
}
