#include <algorithm>
#include <charconv>
#include <chrono>
#include <cstdint>
#include <filesystem>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <numeric>
#include <optional>
#include <random>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

#include "inverse.h"

namespace br = dixelu::bitreverse;
namespace inv = br::inversion;

namespace
{
using clock_type = std::chrono::steady_clock;
constexpr std::uint64_t seed = 0x8a06b7cd42e5913fULL;
std::uint64_t consumed = 0;

inv::values bits(std::uint64_t word, size_t width)
{
	inv::values result(width);
	for (size_t bit = 0; bit < width; ++bit) result[bit] = ((word >> bit) & 1U) != 0;
	return result;
}

std::uint64_t word(const inv::values& value, size_t expected_width)
{
	if (value.size() != expected_width) throw std::runtime_error("learned inverse returned the wrong input width");
	std::uint64_t result = 0;
	for (size_t bit = 0; bit < value.size(); ++bit) result |= static_cast<std::uint64_t>(value[bit]) << bit;
	return result;
}

struct experiment
{
	std::string name;
	size_t width;
	inv::program forward;
	std::vector<std::uint64_t> permutation;

	std::uint64_t native_forward(std::uint64_t input) const
	{
		if (!permutation.empty()) return permutation.at(static_cast<size_t>(input));
		auto result = input;
		for (size_t group = 0; group < width; group += 3)
			result ^= ((input >> group & 1U) & (input >> (group + 1) & 1U)) << (group + 2);
		return result;
	}
};

experiment toffoli(size_t width)
{
	inv::bits input(width);
	for (auto& bit : input) bit = br::unknown;
	auto output = input;
	for (size_t group = 0; group < width; group += 3)
		output[group + 2] = input[group + 2] ^ (input[group] & input[group + 1]);
	return {"toffoli_" + std::to_string(width), width, inv::program::compile(input, output), {}};
}

experiment permutation(size_t width)
{
	const size_t count = size_t{1} << width;
	std::vector<std::uint64_t> mapping(count);
	std::iota(mapping.begin(), mapping.end(), std::uint64_t{0});
	std::mt19937_64 random(seed + width);
	// Explicit Fisher-Yates fixes the permutation independently of std::shuffle.
	for (size_t end = count; end > 1; --end)
		std::swap(mapping[end - 1], mapping[static_cast<size_t>(random() % end)]);
	inv::bits input(width), output(width, br::bit_tracker(false));
	for (auto& bit : input) bit = br::unknown;
	for (size_t value = 0; value < count; ++value)
	{
		br::bit_tracker term(true);
		for (size_t bit = 0; bit < width; ++bit)
			term = term & ((value >> bit & 1U) ? input[bit] : !input[bit]);
		for (size_t bit = 0; bit < width; ++bit)
			if (mapping[value] >> bit & 1U) output[bit] = output[bit] | term;
	}
	return {"random_permutation_" + std::to_string(width), width,
		inv::program::compile(input, output), std::move(mapping)};
}

struct probes
{
	std::vector<std::uint64_t> native_targets;
	std::vector<inv::values> targets;
	std::string mode;
};

probes make_probes(size_t width)
{
	probes result;
	const bool exhaustive = width <= 12;
	const size_t count = exhaustive ? size_t{1} << width : size_t{4096};
	result.mode = exhaustive ? "native_exhaustive" : "native_random_probes";
	std::mt19937_64 random(seed ^ width);
	const auto mask = (std::uint64_t{1} << width) - 1;
	for (size_t sample = 0; sample < count; ++sample)
	{
		// Boundary values supplement the fixed-seed large-width sample.
		const auto target = exhaustive ? sample : sample == 0 ? 0 : sample == 1 ? mask : random() & mask;
		result.native_targets.push_back(target);
		result.targets.push_back(bits(target, width));
	}
	return result;
}

template<class Function>
double query_ns(size_t count, Function&& query)
{
	size_t completed = 0;
	std::uint64_t checksum = 0;
	const auto started = clock_type::now();
	do
	{
		for (size_t batch = 0; batch < 64; ++batch, ++completed)
			query(completed % count, checksum);
	} while (completed < 65536 && clock_type::now() - started < std::chrono::milliseconds(40));
	consumed ^= checksum + completed;
	return std::chrono::duration<double, std::nano>(clock_type::now() - started).count() / static_cast<double>(completed);
}

void write_artifacts(const experiment& item, const inv::program& learned, const std::filesystem::path& directory)
{
	std::filesystem::create_directories(directory);
	const auto write = [&](const std::string& extension)
	{
		std::ofstream output(directory / (item.name + extension), std::ios::binary);
		output.exceptions(std::ios::badbit | std::ios::failbit);
		return output;
	};
	auto artifact = write(".bri");
	learned.save(artifact);
	auto header = write(".h");
	learned.export_cpp(header);
	auto standalone = write("_standalone.cpp");
	standalone << "#include \"" << item.name << ".h\"\n#include \"" << item.name << ".h\"\n"
		<< "#include <array>\n#include <cstdint>\n#include <iostream>\n#include <random>\n"
		<< "constexpr size_t width = " << item.width << ";\n"
		<< "std::uint64_t native_forward(std::uint64_t input) {\n";
	if (item.permutation.empty())
		standalone << "  auto result = input;\n"
			<< "  for (size_t group = 0; group < width; group += 3)\n"
			<< "    result ^= ((input >> group & 1U) & (input >> (group + 1) & 1U)) << (group + 2);\n"
			<< "  return result;\n";
	else
	{
		standalone << "  constexpr std::array<std::uint64_t, " << item.permutation.size() << "> mapping{";
		for (size_t i = 0; i < item.permutation.size(); ++i)
			standalone << (i ? "," : "") << item.permutation[i];
		standalone << "};\n  return mapping[input];\n";
	}
	standalone << "}\nint main() {\n"
		<< "  constexpr bool exhaustive = width <= 12;\n"
		<< "  constexpr size_t count = exhaustive ? size_t{1} << width : size_t{4096};\n"
		<< "  constexpr std::uint64_t mask = (std::uint64_t{1} << width) - 1;\n"
		<< "  std::mt19937_64 random(" << seed << "ULL ^ width);\n"
		<< "  for (size_t sample = 0; sample < count; ++sample) {\n"
		<< "    const std::uint64_t value = exhaustive ? sample : sample == 0 ? 0 : sample == 1 ? mask : random() & mask;\n"
		<< "    std::array<bool, width> target{};\n"
		<< "    for (size_t bit = 0; bit < width; ++bit) target[bit] = ((value >> bit) & 1U) != 0;\n"
		<< "    const auto recovered = bitreverse_inverse(target);\n"
		<< "    if (!recovered) return 1;\n"
		<< "    std::uint64_t input = 0;\n"
		<< "    for (size_t bit = 0; bit < width; ++bit) input |= static_cast<std::uint64_t>((*recovered)[bit]) << bit;\n"
		<< "    if (native_forward(input) != value) return 2;\n"
		<< "  }\n  std::cout << (exhaustive ? \"native exhaustive: \" : \"native random probes: \") << count << '\\n';\n}\n";
}

void run(const experiment& item, const inv::cegis_options& options, const std::optional<std::filesystem::path>& directory)
{
	inv::cegis_statistics statistics;
	std::optional<inv::program> learned;
	const auto started = clock_type::now();
	std::string status = "certified";
	try { learned = item.forward.learned(options, &statistics); }
	catch (const inv::cegis_limit& error)
	{
		status = "limit";
		std::cerr << item.name << ": " << error.what() << '\n';
	}
	const auto build_ms = std::chrono::duration<double, std::milli>(clock_type::now() - started).count();
	size_t artifact_bytes = 0;
	size_t validation_cases = 0;
	double validation_ms = 0, inverse_ns = 0, native_ns = 0;
	std::string validation_mode = "not_run";
	if (learned)
	{
		if (!statistics.certified || !learned->is_learned()) throw std::runtime_error("learning returned an uncertified artifact");
		std::ostringstream saved;
		learned->save(saved);
		artifact_bytes = saved.str().size();
		std::istringstream source(saved.str());
		const auto loaded = inv::program::load(source);
		if (!loaded.is_learned()) throw std::runtime_error("round trip lost the learned backend");
		const auto queries = make_probes(item.width);
		const auto validation_started = clock_type::now();
		for (size_t sample = 0; sample < queries.targets.size(); ++sample)
		{
			const auto recovered = loaded.evaluate(queries.targets[sample], {});
			if (!recovered || item.native_forward(word(*recovered, item.width)) != queries.native_targets[sample])
				throw std::runtime_error("loaded learned inverse failed independent native validation");
		}
		validation_ms = std::chrono::duration<double, std::milli>(clock_type::now() - validation_started).count();
		validation_cases = queries.targets.size();
		validation_mode = queries.mode;
		inverse_ns = query_ns(queries.targets.size(), [&](size_t sample, std::uint64_t& checksum)
		{
			const auto recovered = loaded.evaluate(queries.targets[sample], {});
			checksum = checksum * 131U + static_cast<unsigned>(recovered.has_value());
			if (recovered)
				for (const bool bit : *recovered) checksum = checksum * 131U + static_cast<unsigned>(bit);
		});
		native_ns = query_ns(queries.targets.size(), [&](size_t sample, std::uint64_t& checksum)
		{
			checksum = checksum * 131U + item.native_forward(queries.native_targets[sample]);
		});
		if (directory) write_artifacts(item, loaded, *directory);
	}
	else if (statistics.certified) throw std::runtime_error("failed learning claimed formal certification");
	std::cout << item.name << ',' << item.width << ',' << item.forward.node_count() << ',' << status << ','
		<< statistics.failed_phase << ',' << options.max_degree << ',' << options.max_features << ','
		<< options.max_counterexamples << ',' << options.max_solver_steps << ',' << options.max_nodes << ','
		<< build_ms << ',' << std::chrono::duration<double, std::milli>(statistics.verification_elapsed).count() << ','
		<< statistics.features << ',' << statistics.counterexamples << ',' << statistics.solver_calls << ','
		<< statistics.solver_steps << ',' << statistics.candidate_terms << ',' << statistics.candidate_coefficients << ','
		<< artifact_bytes << ',' << inverse_ns << ',' << native_ns << ',' << validation_mode << ','
		<< validation_cases << ',' << validation_ms << ',' << static_cast<unsigned>(statistics.certified) << '\n';
	std::cout.flush();
}
}

int main(int argc, char** argv)
{
	try
	{
		std::optional<std::filesystem::path> directory;
		bool include_48 = false;
		inv::cegis_options options;
		for (int argument = 1; argument < argc; ++argument)
		{
			const std::string value = argv[argument];
			if (value == "--include-48") include_48 = true;
			else if (value.starts_with("--output=") && value.size() > 9) directory = value.substr(9);
			else if (value.starts_with("--max-solver-steps="))
			{
				constexpr size_t prefix_size = std::string_view("--max-solver-steps=").size();
				const auto parsed = std::from_chars(value.data() + prefix_size,
					value.data() + value.size(), options.max_solver_steps);
				if (parsed.ec != std::errc{} || parsed.ptr != value.data() + value.size() || !options.max_solver_steps)
					throw std::invalid_argument("--max-solver-steps requires a positive integer");
			}
			else throw std::invalid_argument("Usage: cegis_benchmark [--include-48] [--max-solver-steps=N] [--output=ARTIFACT_DIRECTORY]");
		}
		std::cout << std::fixed << std::setprecision(3)
			<< "# Bounded degree-two ANF CEGIS inverse; no input-domain enumeration in inverse construction.\n"
			<< "# Formal certification is the backend UNSAT proof; native checks are independent validation after save/load.\n"
			<< "# Native checks exhaust widths <=12; larger widths use 4096 fixed-seed probes, which are not a proof.\n"
			<< "# Random permutation truth tables define controls only; degree-two grammar failure is an expected bounded outcome.\n"
			<< "# Inverse timing includes vector<bool> allocation and forward validation; native timing uses uint64_t values.\n"
			<< "# Every query result is consumed; timing batches stop at 65536 queries or approximately 40 ms.\n"
			<< "# seed=" << seed << '\n'
			<< "case,width,forward_nodes,status,failed_phase,max_degree,max_features,max_counterexamples,max_solver_steps,max_nodes,build_ms,verification_ms,features,counterexamples,solver_calls,solver_steps,candidate_terms,candidate_coefficients,artifact_bytes,inverse_ns,native_forward_ns,validation_mode,validation_cases,validation_ms,formally_certified\n";
		for (const size_t width : {size_t{3}, size_t{6}, size_t{12}, size_t{24}})
			run(toffoli(width), options, directory);
		if (include_48) run(toffoli(48), options, directory);
		for (const size_t width : {size_t{3}, size_t{4}, size_t{5}, size_t{6}})
			run(permutation(width), options, directory);
		std::cout << "# consumed=" << consumed << '\n';
	}
	catch (const std::exception& error)
	{
		std::cerr << "CEGIS benchmark failure: " << error.what() << '\n';
		return 1;
	}
}
