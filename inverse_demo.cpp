#include <charconv>
#include <chrono>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

#include "crc32.h"
#include "inverse.h"
#include "md5.h"

#if defined(__MINGW32__)
// Preserve ?? byte patterns when the Windows runtime parses command arguments.
extern "C" { int _dowildcard = 0; }
#endif

namespace br = dixelu::bitreverse;
namespace inv = br::inversion;

namespace
{

void usage(std::ostream& out)
{
	out << "Usage:\n"
		<< "  inverse_demo build crc32|md5 HEX_PATTERN FILE [--printable] [--synthesize]\n"
		<< "                     [--max-nodes=N] [--max-operations=N]\n"
		<< "  inverse_demo synthesize INPUT.bri OUTPUT.bri [--max-nodes=N]\n"
		<< "                          [--max-operations=N]\n"
		<< "  inverse_demo synthesize-selector INPUT.bri OUTPUT.bri [--max-assignments=N]\n"
		<< "                                   [--max-nodes=N] [--max-operations=N] [--max-bytes=N]\n"
		<< "  inverse_demo export-cpp INPUT.bri OUTPUT.h\n"
		<< "  inverse_demo inspect FILE\n"
		<< "  inverse_demo solve FILE TARGET_HEX [--limit=N|--all] [--output=FILE]\n"
		<< "                     [--no-affine] [--conflict-learning]\n\n"
		<< "HEX_PATTERN contains hex digits and ? wildcards (one unknown nibble each).\n"
		<< "Quote patterns in your shell. The message length and known bytes are baked in.\n"
		<< "--printable requires every input byte to be ASCII 0x20..0x7e.\n"
		<< "Synthesis compiles an exact inverse function; resource limits must be positive.\n"
		<< "synthesize-selector enumerates a bounded domain and uses one forward check per query.\n"
		<< "export-cpp writes a standalone canonical inverse from a synthesized or selector file.\n"
		<< "TARGET_HEX is the ordinary CRC32 or MD5 hex digest, without a 0x prefix.\n"
		<< "Solutions are complete input messages as hex, one per line on stdout.\n"
		<< "The default limit is 1; --all or --limit=0 enumerates all matches.\n"
		<< "--output also writes and flushes each solution to a file.\n"
		<< "--conflict-learning disables affine propagation for the search solver.\n"
		<< "Exit codes: 0 success, 1 invalid arguments/file/error, 2 no matching input.\n";
}

unsigned hex_digit(char value)
{
	if (value >= '0' && value <= '9')
		return static_cast<unsigned>(value - '0');
	if (value >= 'a' && value <= 'f')
		return static_cast<unsigned>(value - 'a' + 10);
	if (value >= 'A' && value <= 'F')
		return static_cast<unsigned>(value - 'A' + 10);
	throw std::invalid_argument("invalid hex digit; expected 0..9, a..f, or A..F");
}

std::vector<br::itu8> parse_pattern(std::string_view pattern)
{
	if (pattern.size() % 2 != 0)
		throw std::invalid_argument("HEX_PATTERN must contain complete byte pairs");
	std::vector<br::itu8> message;
	message.reserve(pattern.size() / 2);
	for (size_t offset = 0; offset < pattern.size(); offset += 2)
	{
		if (pattern.substr(offset, 2) == "??")
			message.emplace_back(br::unknown);
		else if (pattern[offset] == '?' || pattern[offset + 1] == '?')
		{
			br::itu8 byte{0};
			for (size_t nibble = 0; nibble < 2; ++nibble)
			{
				const char digit = pattern[offset + nibble];
				if (digit == '?')
					for (size_t bit = 0; bit < 4; ++bit) byte.bits[nibble * 4 + bit] = br::unknown;
				else
				{
					const auto value = hex_digit(digit);
					for (size_t bit = 0; bit < 4; ++bit)
						byte.bits[nibble * 4 + bit] = br::bit_tracker(((value >> (3 - bit)) & 1U) != 0);
				}
			}
			message.push_back(std::move(byte));
		}
		else
			message.emplace_back(
				hex_digit(pattern[offset]) * 16 + hex_digit(pattern[offset + 1]));
	}
	return message;
}

inv::values parse_target(std::string_view hex, size_t bit_count)
{
	if (bit_count % 4 != 0 || hex.size() != bit_count / 4)
		throw std::invalid_argument(
			"TARGET_HEX must contain exactly " + std::to_string(bit_count / 4) +
			" hex digits for this inverse");
	inv::values result;
	result.reserve(bit_count);
	// bits_of(integer) is LSB first; the displayed digest is most significant first.
	for (auto digit = hex.rbegin(); digit != hex.rend(); ++digit)
	{
		const unsigned nibble = hex_digit(*digit);
		for (unsigned bit = 0; bit < 4; ++bit)
			result.push_back(((nibble >> bit) & 1) != 0);
	}
	return result;
}

std::string message_hex(const inv::values& value)
{
	if (value.size() % 8 != 0)
		throw std::invalid_argument("this CLI requires whole-byte message inputs");
	static constexpr char digits[] = "0123456789abcdef";
	std::string result;
	result.reserve(value.size() / 4);
	for (size_t offset = 0; offset < value.size(); offset += 8)
	{
		unsigned byte = 0;
		for (size_t bit = 0; bit < 8; ++bit)
			byte |= static_cast<unsigned>(value[offset + bit]) << bit;
		result.push_back(digits[byte >> 4]);
		result.push_back(digits[byte & 15]);
	}
	return result;
}

void describe(const inv::program& program, std::ostream& out)
{
	out << "Input bits: " << program.input_count()
		<< ", unknown bits: " << program.unknown_count()
		<< ", output bits: " << program.output_count()
		<< ", circuit nodes: " << program.node_count() << '\n';
	if (program.is_selected())
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

inv::program read_program(const std::string& path)
{
	std::ifstream input(path, std::ios::binary);
	if (!input)
		throw std::runtime_error("cannot open inverse file: " + path);
	return inv::program::load(input);
}

size_t parse_limit(std::string_view text, std::string_view name = "--limit")
{
	size_t result = 0;
	const auto parsed = std::from_chars(text.data(), text.data() + text.size(), result);
	if (text.empty() || parsed.ec != std::errc{} || parsed.ptr != text.data() + text.size())
		throw std::invalid_argument(std::string(name) + " requires a nonnegative integer");
	return result;
}

bool parse_synthesis_option(std::string_view option, inv::synthesis_options& options)
{
	std::string_view name;
	size_t* value = nullptr;
	if (option.starts_with("--max-nodes="))
	{
		name = "--max-nodes";
		value = &options.max_nodes;
	}
	else if (option.starts_with("--max-operations="))
	{
		name = "--max-operations";
		value = &options.max_operations;
	}
	else
		return false;
	*value = parse_limit(option.substr(name.size() + 1), name);
	if (*value == 0)
		throw std::invalid_argument(std::string(name) + " must be positive; synthesis is always bounded");
	return true;
}

void describe_synthesis(std::ostream& stream, const inv::synthesis_statistics& statistics)
{
	stream << "Synthesis build: "
		<< std::chrono::duration<double>(statistics.elapsed).count() << " seconds, "
		<< statistics.created_nodes << " created nodes, "
		<< statistics.operations << " operations\n";
	for (size_t index = 0; index < statistics.phases.size(); ++index)
	{
		const auto& phase = statistics.phases[index];
		if (!phase.started) continue;
		stream << "  " << inv::synthesis_phase_name(static_cast<inv::synthesis_phase>(index))
			<< (phase.completed ? ": completed, " : ": failed, ")
			<< std::chrono::duration<double, std::milli>(phase.elapsed).count() << " ms, "
			<< phase.created_nodes << " created, " << phase.resident_nodes << " resident, ";
		if (phase.completed) stream << phase.live_nodes;
		else stream << "unavailable";
		stream << " reachable from phase outputs, " << phase.peak_tracked_bytes << " peak tracked bytes\n";
	}
	stream << "Peak tracked builder allocation: " << statistics.peak_tracked_bytes
		<< " bytes (excludes source/returned circuits and allocator overhead)\n";
}

inv::program synthesize_program(const inv::program& program,
	const inv::synthesis_options& options)
{
	inv::synthesis_statistics statistics;
	const auto result = [&]
	{
		try { return program.synthesized(options, &statistics); }
		catch (const inv::synthesis_limit&)
		{
			describe_synthesis(std::cerr, statistics);
			throw;
		}
	}();
	describe_synthesis(std::cout, statistics);
	return result;
}

void require_different_files(const std::string& source, const std::string& destination)
{
	if (std::filesystem::exists(destination) &&
		std::filesystem::equivalent(source, destination))
		throw std::invalid_argument("output file must differ from the input inverse file");
}

void save_program(const inv::program& program, const std::string& path)
{
	std::ofstream output(path, std::ios::binary | std::ios::trunc);
	if (!output)
		throw std::runtime_error("cannot create inverse file: " + path);
	program.save(output);
	output.close();
	if (!output)
		throw std::runtime_error("failed to write inverse file: " + path);
}

int synthesize(int argc, char** argv)
{
	if (argc < 4)
		throw std::invalid_argument("synthesize expects INPUT.bri OUTPUT.bri and optional resource limits");
	inv::synthesis_options options;
	for (int index = 4; index < argc; ++index)
		if (!parse_synthesis_option(argv[index], options))
			throw std::invalid_argument("unknown synthesis option: " + std::string(argv[index]));
	const auto source = read_program(argv[2]);
	require_different_files(argv[2], argv[3]);
	const auto program = synthesize_program(source, options);
	save_program(program, argv[3]);
	std::cout << "Saved synthesized inverse: " << argv[3] << '\n';
	describe(program, std::cout);
	return 0;
}

void describe_selector(std::ostream& stream, const inv::selector_statistics& statistics)
{
	stream << "Selector build: " << std::chrono::duration<double>(statistics.elapsed).count()
		<< " seconds, " << statistics.domain_assignments << " domain assignments, "
		<< statistics.allowed_assignments << " allowed, " << statistics.distinct_outputs
		<< " distinct outputs, " << statistics.operations << " operations\n"
		<< "  Forward enumeration: " << std::chrono::duration<double, std::milli>(statistics.enumeration_elapsed).count()
		<< " ms; tree construction: " << std::chrono::duration<double, std::milli>(statistics.tree_elapsed).count()
		<< " ms; exhaustive certification: " << std::chrono::duration<double, std::milli>(statistics.validation_elapsed).count()
		<< " ms\nPeak tracked selector allocation: " << statistics.peak_bytes << " bytes\n";
	if (statistics.failed_phase != "none") stream << "Failed phase: " << statistics.failed_phase << '\n';
}

int synthesize_selector(int argc, char** argv)
{
	if (argc < 4)
		throw std::invalid_argument("synthesize-selector expects INPUT.bri OUTPUT.bri and optional resource limits");
	inv::selector_options options;
	for (int index = 4; index < argc; ++index)
	{
		const std::string_view argument(argv[index]);
		const auto separator = argument.find('=');
		const auto name = argument.substr(0, separator);
		size_t* value = nullptr;
		if (name == "--max-assignments") value = &options.max_assignments;
		else if (name == "--max-nodes") value = &options.max_nodes;
		else if (name == "--max-operations") value = &options.max_operations;
		else if (name == "--max-bytes") value = &options.max_bytes;
		if (!value || separator == std::string_view::npos)
			throw std::invalid_argument("unknown selector option: " + std::string(argument));
		*value = parse_limit(argument.substr(separator + 1), name);
		if (!*value) throw std::invalid_argument(std::string(name) + " must be positive");
	}
	const auto source = read_program(argv[2]);
	require_different_files(argv[2], argv[3]);
	inv::selector_statistics statistics;
	const auto program = [&]
	{
		try { return source.selected(options, &statistics); }
		catch (const inv::selector_limit&)
		{
			describe_selector(std::cerr, statistics);
			throw;
		}
	}();
	describe_selector(std::cout, statistics);
	save_program(program, argv[3]);
	std::cout << "Saved compiled selector: " << argv[3] << '\n';
	describe(program, std::cout);
	return 0;
}

int export_cpp(int argc, char** argv)
{
	if (argc != 4)
		throw std::invalid_argument("export-cpp expects INPUT.bri OUTPUT.h");
	const auto program = read_program(argv[2]);
	if (!program.is_synthesized() && !program.is_selected())
		throw std::invalid_argument("export-cpp requires a synthesized inverse or compiled selector");
	require_different_files(argv[2], argv[3]);
	std::ostringstream generated;
	program.export_cpp(generated);
	std::ofstream output(argv[3], std::ios::binary | std::ios::trunc);
	if (!output)
		throw std::runtime_error("cannot create C++ header: " + std::string(argv[3]));
	output << generated.str();
	output.close();
	if (!output)
		throw std::runtime_error("failed to write C++ header: " + std::string(argv[3]));
	std::cout << "Exported standalone inverse: " << argv[3] << '\n';
	return 0;
}

int build(int argc, char** argv)
{
	if (argc < 5)
		throw std::invalid_argument("build expects HASH HEX_PATTERN FILE and optional build flags");
	const std::string_view algorithm(argv[2]);
	if (algorithm != "crc32" && algorithm != "md5")
		throw std::invalid_argument("unknown hash; choose crc32 or md5");
	bool printable = false;
	bool do_synthesize = false;
	bool have_synthesis_limits = false;
	inv::synthesis_options synthesis_options;
	for (int index = 5; index < argc; ++index)
	{
		const std::string_view option(argv[index]);
		if (option == "--printable")
			printable = true;
		else if (option == "--synthesize")
			do_synthesize = true;
		else if (parse_synthesis_option(option, synthesis_options))
			have_synthesis_limits = true;
		else
			throw std::invalid_argument("unknown build option: " + std::string(option));
	}
	if (have_synthesis_limits && !do_synthesize)
		throw std::invalid_argument("build resource limits require --synthesize");
	const auto message = parse_pattern(argv[3]);
	inv::bits requirements;
	if (printable)
	{
		requirements.reserve(message.size());
		for (const auto& byte : message)
			requirements.push_back(
				(!br::bit_tracker(byte & 0x80)) &
				br::bit_tracker(byte & 0x60) &
				br::bit_tracker(byte ^ 0x7f));
	}
	const auto outputs = algorithm == "crc32"
		? inv::bits_of(br::hash::crc32(message))
		: inv::bits_of(br::hash::md5(message));
	auto program = inv::program::compile(inv::bits_of(message), outputs, requirements);
	if (do_synthesize)
		program = synthesize_program(program, synthesis_options);
	save_program(program, argv[4]);
	std::cout << "Saved " << algorithm << " inverse: " << argv[4] << '\n'
		<< "Baked input pattern: " << argv[3] << '\n';
	if (printable)
		std::cout << "Baked input restriction: printable ASCII (20..7e)\n";
	describe(program, std::cout);
	return 0;
}

int solve(int argc, char** argv)
{
	if (argc < 4)
		throw std::invalid_argument("solve expects FILE TARGET_HEX and optional solve flags");
	size_t limit = 1;
	bool have_limit = false;
	std::string output_path;
	br::solver_options options;
	for (int index = 4; index < argc; ++index)
	{
		const std::string_view option(argv[index]);
		if (option == "--all" || option.starts_with("--limit="))
		{
			if (have_limit)
				throw std::invalid_argument("specify only one of --limit=N or --all");
			have_limit = true;
			limit = option == "--all" ? 0 : parse_limit(option.substr(8));
		}
		else if (option.starts_with("--output="))
		{
			if (!output_path.empty() || option.size() == 9)
				throw std::invalid_argument("--output requires one nonempty file path");
			output_path = option.substr(9);
		}
		else if (option == "--no-affine")
			options.affine_reasoning = false;
		else if (option == "--conflict-learning")
		{
			options.affine_reasoning = false;
			options.conflict_learning = true;
		}
		else
			throw std::invalid_argument("unknown solve option: " + std::string(option));
	}
	const auto program = read_program(argv[2]);
	if (program.input_count() % 8 != 0)
		throw std::invalid_argument("this CLI requires whole-byte message inputs");
	const auto target = parse_target(argv[3], program.output_count());
	std::ofstream output;
	if (!output_path.empty())
	{
		if (std::filesystem::exists(output_path) &&
			std::filesystem::equivalent(argv[2], output_path))
			throw std::invalid_argument("solution output must differ from the inverse file");
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

}

int main(int argc, char** argv)
{
	try
	{
		if (argc == 2 && std::string_view(argv[1]) == "--help")
		{
			usage(std::cout);
			return 0;
		}
		if (argc < 2)
			throw std::invalid_argument("expected build, synthesize, synthesize-selector, export-cpp, inspect, or solve");
		const std::string_view command(argv[1]);
		if (command == "build")
			return build(argc, argv);
		if (command == "solve")
			return solve(argc, argv);
		if (command == "synthesize")
			return synthesize(argc, argv);
		if (command == "synthesize-selector")
			return synthesize_selector(argc, argv);
		if (command == "export-cpp")
			return export_cpp(argc, argv);
		if (command == "inspect")
		{
			if (argc != 3)
				throw std::invalid_argument("inspect expects FILE");
			describe(read_program(argv[2]), std::cout);
			return 0;
		}
		throw std::invalid_argument("unknown command: " + std::string(command));
	}
	catch (const std::exception& error)
	{
		std::cerr << "Error: " << error.what() << '\n';
		usage(std::cerr);
		return 1;
	}
}
