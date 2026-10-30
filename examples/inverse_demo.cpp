#include <algorithm>
#include <cstdint>
#include <filesystem>
#include <fstream>
#include <initializer_list>
#include <iostream>
#include <set>
#include <span>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include "crc32.h"
#include "inverse.h"
#include "md5.h"
#include "text.h"
#include "timing.h"

#if defined(__MINGW32__)
// Preserve ?? byte patterns when the Windows runtime parses command arguments.
extern "C" { int _dowildcard = 0; }
#endif

namespace br = dixelu::bitreverse;
namespace inv = br::inversion;

namespace
{

using arguments = std::span<char* const>;

// A --name flag or a --name=VALUE option of one command.
struct option
{
	std::string_view name;
	std::variant<bool*, size_t*, std::string*> target;
};

// Stores each argument in its option's target; a repeated option keeps the
// last value. Returns the option names given, with repeats.
std::multiset<std::string_view> parse_options(std::string_view command, arguments given,
	std::initializer_list<option> table)
{
	std::multiset<std::string_view> names;
	for (const std::string_view argument : given)
	{
		const auto separator = argument.find('=');
		const auto name = argument.substr(0, separator);
		const auto match = std::ranges::find(table, name, &option::name);
		const bool has_value = separator != std::string_view::npos;
		if (match == table.end() || has_value == std::holds_alternative<bool*>(match->target))
			throw std::invalid_argument("unknown " + std::string(command) + " option: " + std::string(argument));
		const auto value = has_value ? argument.substr(separator + 1) : std::string_view{};
		if (const auto flag = std::get_if<bool*>(&match->target))
			**flag = true;
		else if (const auto number = std::get_if<size_t*>(&match->target))
			**number = support::parse_size(value, name);
		else if (value.empty())
			throw std::invalid_argument(std::string(name) + " requires one nonempty file path");
		else
			*std::get<std::string*>(match->target) = value;
		names.insert(match->name);
	}
	return names;
}

// Each '?' is one unknown nibble; the message length and known nibbles are baked in.
std::vector<br::itu8> parse_pattern(std::string_view pattern)
{
	if (pattern.size() % 2 != 0)
		throw std::invalid_argument("HEX_PATTERN must contain complete byte pairs");
	std::vector<br::itu8> message(pattern.size() / 2);
	for (size_t digit = 0; digit < pattern.size(); ++digit)
	{
		const bool unknown = pattern[digit] == '?';
		const unsigned nibble = unknown ? 0 : support::hex_digit(pattern[digit]);
		// itu8 bits are most significant first, like the hex digits.
		auto* bits = &message[digit / 2].bits[(digit % 2) * 4];
		for (size_t bit = 0; bit < 4; ++bit)
			bits[bit] = unknown ? br::bit_tracker(br::unknown) : br::bit_tracker(((nibble >> (3 - bit)) & 1U) != 0);
	}
	return message;
}

// bits_of(integer) is least significant bit first; the displayed digest is
// most significant digit first.
inv::values parse_target(std::string_view hex, size_t bit_count)
{
	if (bit_count % 4 != 0 || hex.size() != bit_count / 4)
		throw std::invalid_argument(
			"TARGET_HEX must contain exactly " + std::to_string(bit_count / 4) +
			" hex digits for this inverse");
	return support::parse_hex_number(hex);
}

// Message bytes in order, each least significant bit first.
std::string message_hex(const inv::values& bits)
{
	std::vector<std::uint8_t> bytes(bits.size() / 8);
	for (size_t bit = 0; bit < bits.size(); ++bit)
		bytes[bit / 8] |= static_cast<std::uint8_t>(bits[bit] << (bit % 8));
	return support::to_hex(bytes);
}

void describe(const inv::program& program, std::ostream& out)
{
	out << "Input bits: " << program.input_count()
		<< ", unknown bits: " << program.unknown_count()
		<< ", output bits: " << program.output_count()
		<< ", circuit nodes: " << program.node_count() << '\n';
	if (program.is_learned())
		out << "Inverse: counterexample-learned polynomial with forward validation; no query-time search\n"
			<< "Shared polynomial terms: " << program.learned_term_count()
			<< "; coefficient references: " << program.learned_coefficient_count() << '\n';
	else if (program.is_selected())
		out << "Inverse: compiled selector with forward validation; no query-time search\n"
			<< "Selector nodes: " << program.selector_node_count()
			<< "; leaves: " << program.selector_leaf_count()
			<< "; retained input assignments: " << program.selector_assignment_count()
			<< "; forward verifier nodes: " << program.selector_forward_node_count() << '\n';
	else if (program.is_synthesized())
		out << "Inverse: synthesized Boolean functions; no query-time search\n"
			<< "Saved decision nodes: " << program.synthesized_node_count()
			<< "; relation nodes: " << program.synthesized_relation_node_count()
			<< "; canonical functions and validity nodes: "
			<< program.synthesized_function_node_count() << '\n'
			<< "Input selector nodes: " << program.synthesized_selector_node_count()
			<< "; reachable-target check nodes: " << program.synthesized_validity_node_count() << '\n';
	else if (program.is_affine())
		out << "Inverse: direct affine family; free bits: "
			<< program.free_bit_count() << '\n';
	else
		out << "Inverse: saved Boolean relation; each target requires search\n";
}

void describe_synthesis(std::ostream& stream, const inv::synthesis_statistics& statistics)
{
	stream << "Synthesis build: " << support::seconds(statistics.elapsed) << " seconds, "
		<< statistics.created_nodes << " created nodes, "
		<< statistics.operations << " operations\n";
	for (size_t index = 0; index < statistics.phases.size(); ++index)
	{
		const auto& phase = statistics.phases[index];
		if (!phase.started) continue;
		stream << "  " << inv::synthesis_phase_name(static_cast<inv::synthesis_phase>(index))
			<< (phase.completed ? ": completed, " : ": failed, ")
			<< support::milliseconds(phase.elapsed) << " ms, "
			<< phase.created_nodes << " created, " << phase.resident_nodes << " resident, ";
		if (phase.completed) stream << phase.live_nodes;
		else stream << "unavailable";
		stream << " reachable from phase outputs, " << phase.peak_tracked_bytes << " peak tracked bytes\n";
	}
	stream << "Peak tracked builder allocation: " << statistics.peak_tracked_bytes
		<< " bytes (excludes source/returned circuits and allocator overhead)\n";
}

void describe_selector(std::ostream& stream, const inv::selector_statistics& statistics)
{
	stream << "Selector build: " << support::seconds(statistics.elapsed)
		<< " seconds, " << statistics.domain_assignments << " domain assignments, "
		<< statistics.allowed_assignments << " allowed, " << statistics.distinct_outputs
		<< " distinct outputs, " << statistics.operations << " operations\n"
		<< "  Forward enumeration: " << support::milliseconds(statistics.enumeration_elapsed)
		<< " ms; tree construction: " << support::milliseconds(statistics.tree_elapsed)
		<< " ms; exhaustive certification: " << support::milliseconds(statistics.validation_elapsed)
		<< " ms\nPeak tracked selector allocation: " << statistics.peak_bytes << " bytes\n";
	if (statistics.failed_phase != "none") stream << "Failed phase: " << statistics.failed_phase << '\n';
}

void describe_cegis(std::ostream& stream, const inv::cegis_statistics& statistics)
{
	stream << "CEGIS build: " << support::seconds(statistics.elapsed)
		<< " seconds, " << statistics.features << " basis features, " << statistics.counterexamples
		<< " counterexamples, " << statistics.solver_calls << " solver calls, "
		<< statistics.solver_steps << " solver steps\n"
		<< "  Candidate: " << statistics.candidate_terms << " terms, "
		<< statistics.candidate_coefficients << " coefficient references; verification: "
		<< support::seconds(statistics.verification_elapsed) << " seconds\n"
		<< "  Certification: " << (statistics.certified ? "completed UNSAT miter" : "incomplete") << '\n';
	if (statistics.failed_phase != "none") stream << "Failed phase: " << statistics.failed_phase << '\n';
}

inv::program read_program(const std::string& path)
{
	std::ifstream input(path, std::ios::binary);
	if (!input)
		throw std::runtime_error("cannot open inverse file: " + path);
	return inv::program::load(input);
}

void require_different_files(const std::string& source, const std::string& destination)
{
	if (std::filesystem::exists(destination) &&
		std::filesystem::equivalent(source, destination))
		throw std::invalid_argument("output file must differ from the input inverse file");
}

// Opened only once the content is complete, so a failure before this point
// leaves an existing file untouched.
void write_file(const std::string& path, const std::string& kind, const std::string& content)
{
	std::ofstream output(path, std::ios::binary | std::ios::trunc);
	if (!output)
		throw std::runtime_error("cannot create " + kind + ": " + path);
	output << content;
	output.close();
	if (!output)
		throw std::runtime_error("failed to write " + kind + ": " + path);
}

void save_program(const inv::program& program, const std::string& path)
{
	std::ostringstream content;
	program.save(content);
	write_file(path, "inverse file", content.str());
}

// Build statistics go to stdout, or to stderr when a resource limit stops the build.
template<class Limit, class Options, class Statistics>
inv::program build_backend(const inv::program& source, const Options& options,
	inv::program (inv::program::*build)(const Options&, Statistics*) const,
	void (&describe_statistics)(std::ostream&, const Statistics&))
{
	Statistics statistics;
	try
	{
		auto program = (source.*build)(options, &statistics);
		describe_statistics(std::cout, statistics);
		return program;
	}
	catch (const Limit&)
	{
		describe_statistics(std::cerr, statistics);
		throw;
	}
}

// The synthesize* commands: compile INPUT.bri into a new backend in OUTPUT.bri.
template<class Limit, class Options, class Statistics>
int compile_backend(arguments files, const Options& options, std::string_view saved,
	inv::program (inv::program::*build)(const Options&, Statistics*) const,
	void (&describe_statistics)(std::ostream&, const Statistics&))
{
	const auto source = read_program(files[0]);
	require_different_files(files[0], files[1]);
	const auto program = build_backend<Limit>(source, options, build, describe_statistics);
	save_program(program, files[1]);
	std::cout << "Saved " << saved << ": " << files[1] << '\n';
	describe(program, std::cout);
	return 0;
}

int build(arguments files, arguments flags)
{
	const std::string_view algorithm = files[0], pattern = files[1];
	if (algorithm != "crc32" && algorithm != "md5")
		throw std::invalid_argument("unknown hash; choose crc32 or md5");
	bool printable = false, run_synthesis = false;
	inv::synthesis_options limits;
	const auto given = parse_options("build", flags, {
		{"--printable", &printable},
		{"--synthesize", &run_synthesis},
		{"--max-nodes", &limits.max_nodes},
		{"--max-operations", &limits.max_operations}});
	if (!run_synthesis && (given.contains("--max-nodes") || given.contains("--max-operations")))
		throw std::invalid_argument("build resource limits require --synthesize");
	const auto message = parse_pattern(pattern);
	inv::bits requirements;
	// Printable is 0x20..0x7e: below 0x80, some bit of 0x60 set, and not 0x7f.
	if (printable)
		for (const auto& byte : message)
			requirements.push_back(
				(!br::bit_tracker(byte & 0x80)) &
				br::bit_tracker(byte & 0x60) &
				br::bit_tracker(byte ^ 0x7f));
	const auto outputs = algorithm == "crc32"
		? inv::bits_of(br::hash::crc32(message))
		: inv::bits_of(br::hash::md5(message));
	auto program = inv::program::compile(inv::bits_of(message), outputs, requirements);
	if (run_synthesis)
		program = build_backend<inv::synthesis_limit>(
			program, limits, &inv::program::synthesized, describe_synthesis);
	save_program(program, files[2]);
	std::cout << "Saved " << algorithm << " inverse: " << files[2] << '\n'
		<< "Baked input pattern: " << pattern << '\n';
	if (printable)
		std::cout << "Baked input restriction: printable ASCII (20..7e)\n";
	describe(program, std::cout);
	return 0;
}

int synthesize(arguments files, arguments flags)
{
	inv::synthesis_options options;
	parse_options("synthesize", flags, {
		{"--max-nodes", &options.max_nodes},
		{"--max-operations", &options.max_operations}});
	return compile_backend<inv::synthesis_limit>(files, options, "synthesized inverse",
		&inv::program::synthesized, describe_synthesis);
}

int synthesize_selector(arguments files, arguments flags)
{
	inv::selector_options options;
	parse_options("synthesize-selector", flags, {
		{"--max-assignments", &options.max_assignments},
		{"--max-nodes", &options.max_nodes},
		{"--max-operations", &options.max_operations},
		{"--max-bytes", &options.max_bytes}});
	return compile_backend<inv::selector_limit>(files, options, "compiled selector",
		&inv::program::selected, describe_selector);
}

int synthesize_cegis(arguments files, arguments flags)
{
	inv::cegis_options options;
	parse_options("synthesize-cegis", flags, {
		{"--degree", &options.max_degree},
		{"--max-features", &options.max_features},
		{"--max-counterexamples", &options.max_counterexamples},
		{"--max-solver-steps", &options.max_solver_steps},
		{"--max-nodes", &options.max_nodes}});
	return compile_backend<inv::cegis_limit>(files, options, "certified learned inverse",
		&inv::program::learned, describe_cegis);
}

int export_cpp(arguments files, arguments flags)
{
	parse_options("export-cpp", flags, {});
	const auto program = read_program(files[0]);
	require_different_files(files[0], files[1]);
	std::ostringstream header;
	program.export_cpp(header);
	write_file(files[1], "C++ header", header.str());
	std::cout << "Exported standalone inverse: " << files[1] << '\n';
	return 0;
}

int inspect(arguments files, arguments flags)
{
	parse_options("inspect", flags, {});
	describe(read_program(files[0]), std::cout);
	return 0;
}

int solve(arguments files, arguments flags)
{
	bool all = false, no_affine = false, conflict_learning = false;
	size_t limit = 1;
	std::string output_path;
	const auto given = parse_options("solve", flags, {
		{"--all", &all},
		{"--limit", &limit},
		{"--output", &output_path},
		{"--no-affine", &no_affine},
		{"--conflict-learning", &conflict_learning}});
	if (given.count("--all") + given.count("--limit") > 1)
		throw std::invalid_argument("specify only one of --limit=N or --all");
	if (given.count("--output") > 1)
		throw std::invalid_argument("--output requires one nonempty file path");
	if (all) limit = 0;
	br::solver_options options;
	// The conflict-learning engine runs without affine reasoning.
	options.affine_reasoning = !no_affine && !conflict_learning;
	options.conflict_learning = conflict_learning;

	const auto program = read_program(files[0]);
	if (program.input_count() % 8 != 0)
		throw std::invalid_argument("this CLI requires whole-byte message inputs");
	const auto target = parse_target(files[1], program.output_count());
	std::ofstream output;
	if (!output_path.empty())
	{
		require_different_files(files[0], output_path);
		output.open(output_path, std::ios::trunc);
		if (!output)
			throw std::runtime_error("cannot create solution file: " + output_path);
	}
	describe(program, std::cerr);
	const size_t count = program.solve(target, [&](const inv::values& input)
	{
		const auto hex = message_hex(input);
		std::cout << hex << std::endl;
		if (!std::cout)
			throw std::runtime_error("failed to write solution to stdout");
		if (output.is_open())
		{
			output << hex << std::endl;
			if (!output)
				throw std::runtime_error("failed to write solution file: " + output_path);
		}
		return true;
	}, limit, options);
	if (count == 0)
	{
		std::cerr << "No matching input for this target and the baked constraints.\n";
		return 2;
	}
	std::cerr << "Emitted " << count << " matching input(s).\n";
	return 0;
}

struct subcommand
{
	std::string_view name;
	std::string_view positional; // one word per required argument
	std::string_view options;
	int (*run)(arguments positional, arguments options);
};

constexpr subcommand subcommands[] = {
	{"build", "crc32|md5 HEX_PATTERN FILE",
		"[--printable] [--synthesize [--max-nodes=N] [--max-operations=N]]", build},
	{"synthesize", "INPUT.bri OUTPUT.bri", "[--max-nodes=N] [--max-operations=N]", synthesize},
	{"synthesize-selector", "INPUT.bri OUTPUT.bri",
		"[--max-assignments=N] [--max-nodes=N] [--max-operations=N] [--max-bytes=N]", synthesize_selector},
	{"synthesize-cegis", "INPUT.bri OUTPUT.bri",
		"[--degree=1|2] [--max-features=N] [--max-counterexamples=N] [--max-solver-steps=N] [--max-nodes=N]",
		synthesize_cegis},
	{"export-cpp", "INPUT.bri OUTPUT.h", "", export_cpp},
	{"inspect", "FILE", "", inspect},
	{"solve", "FILE TARGET_HEX", "[--limit=N|--all] [--output=FILE] [--no-affine] [--conflict-learning]", solve},
};

void usage(std::ostream& out)
{
	out << "Usage:\n";
	for (const auto& command : subcommands)
	{
		out << "  inverse_demo " << command.name << ' ' << command.positional << '\n';
		if (!command.options.empty())
			out << "      " << command.options << '\n';
	}
	out << "\nHEX_PATTERN: hex digits, with ? for each unknown nibble (quote it in the shell).\n"
		<< "--printable restricts every input byte to ASCII 0x20..0x7e.\n"
		<< "TARGET_HEX: the CRC32 or MD5 digest as usually printed, without 0x.\n"
		<< "solve prints matching inputs as hex, one per line: 1 by default, all with --all or --limit=0.\n"
		<< "Exit codes: 0 success, 1 error, 2 no matching input. Details: docs/CLI.md.\n";
}

}

int main(int argc, char** argv)
{
	try
	{
		const auto given = arguments(argv, static_cast<size_t>(argc)).subspan(1);
		if (given.size() == 1 && std::string_view(given[0]) == "--help")
		{
			usage(std::cout);
			return 0;
		}
		if (given.empty())
			throw std::invalid_argument("expected a command");
		const auto command = std::ranges::find(subcommands, std::string_view(given[0]), &subcommand::name);
		if (command == std::ranges::end(subcommands))
			throw std::invalid_argument("unknown command: " + std::string(given[0]));
		const auto positional = static_cast<size_t>(std::ranges::count(command->positional, ' ')) + 1;
		if (given.size() - 1 < positional)
			throw std::invalid_argument(
				std::string(command->name) + " expects " + std::string(command->positional));
		return command->run(given.subspan(1, positional), given.subspan(1 + positional));
	}
	catch (const std::exception& error)
	{
		std::cerr << "Error: " << error.what() << '\n';
		usage(std::cerr);
		return 1;
	}
}
