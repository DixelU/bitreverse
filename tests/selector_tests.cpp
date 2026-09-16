#include <iostream>
#include <map>
#include <set>
#include <sstream>
#include <stdexcept>
#include "inverse.h"
#include "md5.h"
#include "benchmark_native_md5.h"

namespace br = dixelu::bitreverse;
namespace inv = br::inversion;
namespace
{
using models = std::set<inv::values>;
using table = std::map<inv::values, models>;
void require(bool value, const char* message)
{
	if (!value) throw std::runtime_error(message);
}
template<class Exception = std::exception, class Function>
void reject(Function&& function, const char* message)
{
	try { function(); }
	catch (const Exception&) { return; }
	throw std::runtime_error(message);
}
inv::values bits(unsigned value, size_t width)
{
	inv::values result(width);
	for (size_t bit = 0; bit < width; ++bit) result[bit] = ((value >> bit) & 1U) != 0;
	return result;
}
std::string save(const inv::program& program)
{
	std::ostringstream output;
	program.save(output);
	return output.str();
}
inv::program load(const std::string& text)
{
	std::istringstream source(text);
	return inv::program::load(source);
}
void check_target(const inv::program& program, const inv::values& target, const models& expected)
{
	inv::selector_evaluation_statistics visits{999, 999};
	const auto result = program.evaluate_selected(target, &visits);
	require(result == program.evaluate(target, {}), "profiled selector disagrees with default evaluation");
	if (expected.empty()) require(!result, "selector accepted an unreachable target");
	else require(result && *result == *expected.begin(), "selector did not choose false-first canonical input");
	require(visits.decision_visits <= program.output_count(), "selector revisits target bits on a path");
	require(visits.forward_gate_evaluations <= program.selector_forward_node_count(),
		"selector evaluated more than one forward pass");
	models actual;
	br::solver_statistics statistics;
	statistics.decisions = 999;
	statistics.propagations = 999;
	const auto count = program.solve(target, [&](const inv::values& value)
	{
		require(actual.insert(value).second, "selector emitted a duplicate preimage");
		return true;
	}, 0, {}, &statistics);
	require(count == actual.size() && actual == expected, "selector lost or invented preimages");
	require(statistics.decisions == 0 && statistics.propagations == 0, "selector enumeration invoked search");
	if (!expected.empty())
	{
		require(program.solve(target, [](const auto&) { return false; }, 0) == 1,
			"selector ignored callback cancellation");
		require(program.solve(target, [](const auto&) { return true; }, 1) == 1,
			"selector ignored the result limit");
	}
}
void check_table(const inv::program& source, const table& expected)
{
	const auto original = save(source);
	inv::selector_statistics statistics;
	const auto selected = source.selected({}, &statistics);
	require(save(source) == original && !source.is_selected(), "selector compilation mutated the source");
	require(selected.is_selected() && !selected.is_synthesized() && !selected.is_affine(),
		"selector backend must be unambiguous");
	size_t allowed = 0;
	for (const auto& [target, inputs] : expected) { (void)target; allowed += inputs.size(); }
	require(statistics.domain_assignments == (size_t{1} << source.unknown_count()) &&
		statistics.allowed_assignments == allowed && statistics.distinct_outputs == expected.size(),
		"selector certification must cover the entire input domain");
	require(statistics.failed_phase == "none" && selected.selector_assignment_count() == allowed &&
		selected.selector_leaf_count() == expected.size(), "selector groups disagree with the exact forward image");
	const auto artifact = save(selected);
	const auto loaded = load(artifact);
	require(loaded.is_selected() && save(loaded) == artifact, "selector persistence is not stable");
	for (size_t target = 0; target < (size_t{1} << source.output_count()); ++target)
	{
		const auto value = bits(static_cast<unsigned>(target), source.output_count());
		const auto found = expected.find(value);
		const models empty;
		check_target(selected, value, found == expected.end() ? empty : found->second);
		check_target(loaded, value, found == expected.end() ? empty : found->second);
	}
	reject<std::invalid_argument>([&] { loaded.evaluate(inv::values(source.output_count() + 1), {}); },
		"selector accepted wrong target width");
	reject<std::invalid_argument>([&] { loaded.evaluate(inv::values(source.output_count()), {true}); },
		"selector accepted affine free bits");
	reject<std::invalid_argument>([&] { loaded.solve(inv::values(source.output_count()), {}); },
		"selector accepted an empty callback");
	reject<std::logic_error>([&] { (void)loaded.free_bit_count(); }, "selector claimed affine free parameters");
	const auto resynthesized = loaded.synthesized();
	require(resynthesized.is_synthesized() && !resynthesized.is_selected(), "resynthesis retained two active backends");
	const auto reselected = resynthesized.selected();
	require(reselected.is_selected() && !reselected.is_synthesized(), "reselection retained the old backend");
}
void small_cases()
{
	inv::bits x(3);
	for (auto& bit : x) bit = br::unknown;
	table shared, aliases, restricted, empty_outputs;
	for (unsigned input = 0; input < 8; ++input)
	{
		const auto value = bits(input, 3);
		shared[{value[0] && value[1], value[0] || value[1]}].insert(value);
		aliases[{value[0] != value[1]}].insert({value[0], value[0], value[1], true, value[2]});
		if ((value[0] || value[1]) && !value[2]) restricted[{value[0] != value[1]}].insert(value);
		empty_outputs[{}].insert(value);
	}
	check_table(inv::program::compile(x, {x[0] & x[1], x[0] | x[1]}), shared);
	check_table(inv::program::compile({x[0], x[0], x[1], br::bit_tracker(true), x[2]}, {x[0] ^ x[1]}), aliases);
	check_table(inv::program::compile(x, {x[0] ^ x[1]}, {x[0] | x[1], !x[2]}), restricted);
	check_table(inv::program::compile(x, {}), empty_outputs);
	check_table(inv::program::compile({}, {}), table{{{}, models{inv::values{}}}});
	check_table(inv::program::compile({}, {br::bit_tracker(true)}), table{{{true}, models{inv::values{}}}});
	check_table(inv::program::compile(x, {x[0]}, {br::bit_tracker(false)}), {});
}
template<size_t Width>
void arithmetic()
{
	br::int_tracker<Width> x{br::unknown};
	constexpr unsigned mask = (1U << Width) - 1;
	table square, odd;
	for (unsigned value = 0; value <= mask; ++value)
	{
		square[bits(value * value & mask, Width)].insert(bits(value, Width));
		odd[bits((value * 3U + 5U) & mask, Width)].insert(bits(value, Width));
	}
	check_table(inv::program::compile(inv::bits_of(x), inv::bits_of(x * x)), square);
	check_table(inv::program::compile(inv::bits_of(x), inv::bits_of(x * br::int_tracker<Width>{3} + br::int_tracker<Width>{5})), odd);
}
void budgets_and_damage()
{
	br::itu8 x{br::unknown};
	const auto original = inv::program::compile(inv::bits_of(x), inv::bits_of(x * br::itu8{3}));
	const auto untouched = save(original);
	for (unsigned budget = 0; budget < 4; ++budget)
	{
		inv::selector_options options;
		if (budget == 0) options.max_assignments = 1;
		if (budget == 1) options.max_nodes = 1;
		if (budget == 2) options.max_operations = 1;
		if (budget == 3) options.max_bytes = 1;
		inv::selector_statistics statistics;
		reject<inv::selector_limit>([&] { (void)original.selected(options, &statistics); },
			"selector exceeded an explicit construction budget");
		require(statistics.failed_phase != "none", "selector failure omitted its phase");
		require(save(original) == untouched, "failed selector build mutated the original program");
	}
	const auto selected = original.selected();
	const auto artifact = save(selected);
	const auto marker = artifact.find("SELECTOR\n");
	require(marker != std::string::npos, "saved selector has no backend marker");
	for (size_t end = marker; end < artifact.size() - 1; ++end)
		reject([&] { (void)load(artifact.substr(0, end)); }, "truncated selector artifact was accepted");
	for (size_t offset = marker; offset < artifact.find("CHECKSUM"); ++offset)
	{
		if (artifact[offset] < '0' || artifact[offset] > '9') continue;
		auto changed = artifact;
		changed[offset] = changed[offset] == '0' ? '1' : '0';
		reject([&] { (void)load(changed); }, "corrupted selector artifact was accepted");
	}
	reject([&] { (void)load(artifact + "extra"); }, "selector accepted trailing data");
	std::ostringstream formatted;
	formatted << std::hex << std::showbase;
	selected.save(formatted);
	require(formatted.str() == artifact, "selector serialization depends on stream numeric flags");
}
inv::values digest_bits(std::uint32_t word)
{
	const auto digest = benchmark_reference::md5_four_bytes(word);
	inv::values result(128);
	for (size_t byte = 0; byte < 16; ++byte)
		for (size_t bit = 0; bit < 8; ++bit)
			result[(15 - byte) * 8 + bit] = (digest[byte / 4] >> (8 * (byte % 4) + bit)) & 1U;
	return result;
}
void md5_case()
{
	require(benchmark_reference::md5_four_bytes(0x2135646dU) ==
		std::array<std::uint32_t, 4>{0x07c482b6U, 0xe8caa89aU, 0xbf864952U, 0x5c7fe48dU}, "native MD5 known vector failed");
	std::vector<br::itu8> message{'m', 'd', '5'};
	message.emplace_back(br::unknown);
	const auto selected = inv::program::compile(inv::bits_of(message), inv::bits_of(br::hash::md5(message))).selected();
	const auto loaded = load(save(selected));
	table expected;
	for (unsigned last = 0; last < 256; ++last)
	{
		const auto word = 0x0035646dU | (last << 24);
		expected[digest_bits(word)].insert(bits(word, 32));
	}
	for (const auto& [target, inputs] : expected) check_target(loaded, target, inputs);
	check_target(loaded, inv::values(128), {});
	for (size_t bit = 0; bit < 128; ++bit)
	{
		auto target = expected.begin()->first;
		target[bit] = !target[bit];
		if (!expected.contains(target)) check_target(loaded, target, {});
	}
}
}
int main()
{
	try
	{
		small_cases();
		arithmetic<4>();
		arithmetic<8>();
		budgets_and_damage();
		md5_case();
		std::cout << "All selector tests passed\n";
	}
	catch (const std::exception& error)
	{
		std::cerr << "Selector test failure: " << error.what() << '\n';
		return 1;
	}
}
