#include <iostream>
#include <map>
#include <set>
#include <sstream>
#include <stdexcept>
#include <string>

#include "inverse.h"

namespace br = dixelu::bitreverse;
namespace inv = br::inversion;

namespace
{
using models = std::set<inv::values>;
using table = std::map<inv::values, models>;

void require(bool condition, const char* message)
{
	if (!condition) throw std::runtime_error(message);
}

template<class Exception = std::exception, class Function>
void reject(Function&& function, const char* message)
{
	try { function(); }
	catch (const Exception&) { return; }
	throw std::runtime_error(message);
}

inv::values bits(size_t value, size_t width)
{
	inv::values result(width);
	for (size_t bit = 0; bit < width; ++bit) result[bit] = ((value >> bit) & 1U) != 0;
	return result;
}

std::string save(const inv::program& program)
{
	std::ostringstream stream;
	program.save(stream);
	return stream.str();
}

inv::program load(const std::string& artifact)
{
	std::istringstream stream(artifact);
	return inv::program::load(stream);
}

void check_target(const inv::program& learned, const inv::values& target, const models& expected)
{
	const auto witness = learned.evaluate(target, {});
	if (expected.empty()) require(!witness, "learned inverse accepted an unreachable target");
	else require(witness && expected.contains(*witness), "learned inverse returned a nonmember witness");
	// Many-to-one inverses promise a valid witness, without a canonical ordering.
	size_t callbacks = 0;
	br::solver_statistics statistics;
	statistics.decisions = 999;
	statistics.propagations = 999;
	const auto count = learned.solve(target, [&](const inv::values& value)
	{
		++callbacks;
		require(witness && value == *witness, "learned solve disagrees with evaluation");
		return true;
	}, 1, {}, &statistics);
	require(count == callbacks && count == static_cast<size_t>(witness.has_value()),
		"learned solve emitted the wrong number of witnesses");
	require(statistics.decisions == 0 && statistics.propagations == 0,
		"learned query invoked a search backend");
	if (witness)
	{
		require(learned.solve(target, [](const auto&) { return false; }) == 1,
			"learned solve mishandled callback cancellation");
		require(learned.solve(target, [](const auto&) { return true; }) == 1,
			"learned solve default is not one witness");
	}
}

void check_table(const inv::program& source, const table& expected, const inv::cegis_options& options = {})
{
	const auto before = save(source);
	inv::cegis_statistics statistics;
	const auto learned = source.learned(options, &statistics);
	require(save(source) == before && !source.is_learned(), "learning mutated the source program");
	require(learned.is_learned() && !learned.is_affine() && !learned.is_selected() && !learned.is_synthesized(),
		"learned inverse has more than one backend");
	require(statistics.certified && statistics.failed_phase == "none", "learned inverse has no complete proof");
	require(statistics.solver_calls > 0 && statistics.solver_steps <= options.max_solver_steps &&
		statistics.counterexamples <= options.max_counterexamples && statistics.features <= options.max_features,
		"learning statistics violate the construction bounds");
	require(statistics.candidate_terms == learned.learned_term_count() &&
		statistics.candidate_coefficients == learned.learned_coefficient_count(),
		"learned candidate size disagrees with its statistics");
	const auto artifact = save(learned);
	const auto loaded = load(artifact);
	require(loaded.is_learned() && save(loaded) == artifact, "learned artifact did not round trip");
	require(loaded.learned_term_count() == learned.learned_term_count() &&
		loaded.learned_coefficient_count() == learned.learned_coefficient_count(), "loaded candidate size changed");
	for (size_t target = 0; target < (size_t{1} << source.output_count()); ++target)
	{
		const auto value = bits(target, source.output_count());
		const auto found = expected.find(value);
		const models empty;
		check_target(learned, value, found == expected.end() ? empty : found->second);
		check_target(loaded, value, found == expected.end() ? empty : found->second);
	}
	reject<std::invalid_argument>([&] { loaded.evaluate(inv::values(source.output_count() + 1), {}); },
		"learned inverse accepted the wrong target width");
	reject<std::invalid_argument>([&] { loaded.evaluate(inv::values(source.output_count()), {true}); },
		"learned inverse accepted affine free parameters");
	reject<std::invalid_argument>([&] { loaded.solve(inv::values(source.output_count()), {}); },
		"learned solve accepted an empty callback");
	reject<std::logic_error>([&] { (void)loaded.free_bit_count(); }, "learned inverse claims affine free parameters");
	for (const size_t limit : {size_t{0}, size_t{2}, size_t{10}})
		reject<std::logic_error>([&] { loaded.solve(inv::values(source.output_count()), [](const auto&) { return true; }, limit); },
			"learned solve pretended to enumerate all preimages");
	std::ostringstream header;
	loaded.export_cpp(header);
	require(header.str().find("bitreverse_inverse") != std::string::npos, "learned export omitted its entry point");
}

inv::program toffoli(size_t width)
{
	inv::bits input(width);
	for (auto& bit : input) bit = br::unknown;
	auto output = input;
	for (size_t group = 0; group < width; group += 3)
		output[group + 2] = input[group + 2] ^ (input[group] & input[group + 1]);
	return inv::program::compile(input, output);
}

void reversible_cases()
{
	for (const size_t width : {size_t{3}, size_t{6}})
	{
		table expected;
		for (size_t word = 0; word < (size_t{1} << width); ++word)
		{
			const auto input = bits(word, width);
			auto output = input;
			for (size_t group = 0; group < width; group += 3)
				output[group + 2] = input[group + 2] != (input[group] && input[group + 1]);
			expected[output].insert(input);
		}
		check_table(toffoli(width), expected);
	}
	inv::bits input(3);
	for (auto& bit : input) bit = br::unknown;
	table expected;
	for (size_t word = 0; word < 8; ++word)
	{
		const auto value = bits(word, 3);
		expected[{!value[0], value[1] != value[2], value[2]}].insert(value);
	}
	inv::cegis_options linear;
	linear.max_degree = 1;
	check_table(inv::program::compile(input, {!input[0], input[1] ^ input[2], input[2]}), expected, linear);
}

void small_cases()
{
	inv::bits input(3);
	for (auto& bit : input) bit = br::unknown;
	table shared, aliases, restricted, empty_outputs;
	for (size_t word = 0; word < 8; ++word)
	{
		const auto value = bits(word, 3);
		shared[{value[0] && value[1], value[0] || value[1]}].insert(value);
		aliases[{value[0] != value[1], true, value[0] != value[1]}].insert({value[0], value[0], value[1], true, value[2]});
		if ((value[0] || value[1]) && !value[2]) restricted[{value[0] != value[1]}].insert(value);
		empty_outputs[{}].insert(value);
	}
	check_table(inv::program::compile(input, {input[0] & input[1], input[0] | input[1]}), shared);
	check_table(inv::program::compile({input[0], input[0], input[1], br::bit_tracker(true), input[2]},
		{input[0] ^ input[1], br::bit_tracker(true), input[0] ^ input[1]}), aliases);
	check_table(inv::program::compile(input, {input[0] ^ input[1]}, {input[0] | input[1], !input[2]}), restricted);
	check_table(inv::program::compile(input, {}), empty_outputs);
	check_table(inv::program::compile(input, {}, {input[0], !input[1], input[2]}), table{{{}, models{{true, false, true}}}});
	check_table(inv::program::compile({}, {}), table{{{}, models{inv::values{}}}});
	check_table(inv::program::compile({}, {br::bit_tracker(true)}), table{{{true}, models{inv::values{}}}});
	check_table(inv::program::compile(input, {input[0]}, {br::bit_tracker(false)}), {});
	check_table(inv::program::compile(input, {}, {input[0], !input[0]}), {});
}

void budgets_and_grammar()
{
	const auto source = toffoli(3);
	const auto before = save(source);
	for (unsigned kind = 0; kind < 4; ++kind)
	{
		inv::cegis_options options;
		if (kind == 0) options.max_features = 1;
		if (kind == 1) options.max_counterexamples = 1;
		if (kind == 2) options.max_solver_steps = 1;
		if (kind == 3) options.max_nodes = 1;
		inv::cegis_statistics statistics;
		reject<inv::cegis_limit>([&] { (void)source.learned(options, &statistics); }, "CEGIS exceeded an explicit positive budget");
		require(!statistics.certified && statistics.failed_phase != "none", "failed learning reported a complete proof");
		require(save(source) == before, "budget failure changed the source");
	}
	for (unsigned kind = 0; kind < 6; ++kind)
	{
		inv::cegis_options options;
		if (kind == 0) options.max_features = 0;
		if (kind == 1) options.max_counterexamples = 0;
		if (kind == 2) options.max_solver_steps = 0;
		if (kind == 3) options.max_nodes = 0;
		if (kind == 4) options.max_degree = 0;
		if (kind == 5) options.max_degree = 3;
		reject<std::invalid_argument>([&] { (void)source.learned(options); }, "CEGIS accepted invalid options");
		require(save(source) == before, "invalid options changed the source");
	}
	inv::cegis_options linear;
	linear.max_degree = 1;
	inv::cegis_statistics statistics;
	reject<inv::cegis_limit>([&] { (void)source.learned(linear, &statistics); }, "linear grammar represented a Toffoli inverse");
	require(!statistics.certified && statistics.failed_phase != "none", "inconsistent linear grammar reported certification");
	inv::bits input(4);
	for (auto& bit : input) bit = br::unknown;
	const auto cubic = inv::program::compile(input,
		{input[0], input[1], input[2], input[3] ^ (input[0] & input[1] & input[2])});
	const auto cubic_before = save(cubic);
	reject<inv::cegis_limit>([&] { (void)cubic.learned({}, &statistics); }, "quadratic grammar represented a cubic reversible inverse");
	require(!statistics.certified && save(cubic) == cubic_before, "grammar failure changed the source or certified a partial result");
	const auto learned = source.learned({}, &statistics);
	require(statistics.certified && statistics.failed_phase == "none" && learned.is_learned(),
		"statistics did not reset after a failed build");
}

void persistence_and_backends()
{
	const auto learned = toffoli(3).learned();
	const auto artifact = save(learned);
	const auto marker = artifact.find("LEARNED\n");
	require(marker != std::string::npos && artifact.find("CEGIS 1") != std::string::npos,
		"learned serialization has no versioned backend marker");
	for (size_t end = marker; end + 1 < artifact.size(); ++end)
		reject([&] { (void)load(artifact.substr(0, end)); }, "truncated learned artifact was accepted");
	const auto checksum = artifact.find("CHECKSUM");
	require(checksum != std::string::npos, "learned artifact has no checksum");
	for (size_t offset = marker; offset < checksum; ++offset)
	{
		if (artifact[offset] < '0' || artifact[offset] > '9') continue;
		auto changed = artifact;
		changed[offset] = changed[offset] == '0' ? '1' : '0';
		reject([&] { (void)load(changed); }, "corrupted learned artifact was accepted");
	}
	reject([&] { (void)load(artifact + "extra"); }, "learned artifact accepted trailing data");
	std::ostringstream formatted;
	formatted << std::hex << std::showbase;
	learned.save(formatted);
	require(formatted.str() == artifact, "learned serialization depends on stream formatting");
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
		for (size_t target = 0; target < 8; ++target)
			require(relearned.evaluate(bits(target, 3), {}) == learned.evaluate(bits(target, 3), {}),
				"backend recompilation changed the reversible inverse");
	}
}

void depth_guard()
{
	inv::bits input(8);
	for (auto& bit : input) bit = br::unknown;
	auto chain = input[0];
	for (size_t step = 0; step < 320; ++step)
		chain = (chain ^ input[(step + 1) % input.size()]) & input[(step + 4) % input.size()];
	const auto source = inv::program::compile(input, {chain});
	const auto before = save(source);
	inv::cegis_statistics statistics;
	bool depth_rejected = false;
	try { (void)source.learned({}, &statistics); }
	catch (const inv::cegis_limit& error) { depth_rejected = std::string(error.what()).find("depth") != std::string::npos; }
	require(depth_rejected, "deep nonlinear miter bypassed its construction depth cap");
	require(!statistics.certified && statistics.failed_phase == "verification" && save(source) == before,
		"miter depth failure certified a result or changed the source");
}
}

int main()
{
	try
	{
		reversible_cases();
		small_cases();
		budgets_and_grammar();
		persistence_and_backends();
		depth_guard();
		std::cout << "All CEGIS tests passed\n";
	}
	catch (const std::exception& error)
	{
		std::cerr << "CEGIS test failure: " << error.what() << '\n';
		return 1;
	}
}
