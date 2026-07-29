#include <chrono>
#include <iostream>
#include <stdexcept>
#include <string>
#include <vector>

#include "md5.h"

namespace br = dixelu::bitreverse;

int main(int argc, char** argv)
{
	const std::string target_message = argc > 1 ? argv[1] : "md5!";
	const size_t known_prefix_size =
		argc > 2 ? std::stoull(argv[2]) : 3;
	if (known_prefix_size > target_message.size())
		throw std::invalid_argument(
			"known prefix cannot be longer than the target message");

	br::solver_options options;
	for (int argument = 3; argument < argc; ++argument)
	{
		const std::string flag = argv[argument];
		if (flag == "--no-affine")
			options.affine_reasoning = false;
		else if (flag == "--conflict-learning")
			options.conflict_learning = true;
		else if (flag.starts_with("--conflict-analysis-limit="))
			options.max_conflict_analysis_nodes =
				std::stoull(flag.substr(
					std::string("--conflict-analysis-limit=").size()));
		else
			throw std::invalid_argument("unknown option: " + flag);
	}

	std::vector<br::itu8> concrete_message;
	std::vector<br::itu8> candidate;
	concrete_message.reserve(target_message.size());
	candidate.reserve(target_message.size());
	for (size_t index = 0; index < target_message.size(); ++index)
	{
		const auto character =
			static_cast<unsigned char>(target_message[index]);
		concrete_message.emplace_back(character);
		if (index < known_prefix_size)
			candidate.emplace_back(character);
		else
			candidate.emplace_back(br::unknown);
	}

	const auto target = br::hash::md5(concrete_message);

	const auto construction_start = std::chrono::steady_clock::now();
	const auto symbolic = br::hash::md5(candidate);
	const auto construction_end = std::chrono::steady_clock::now();

	br::collision_resolution::crs_state first_solution;
	br::solver_statistics statistics;
	const auto solving_start = std::chrono::steady_clock::now();
	const size_t solution_count = br::assert_equality<128>(
		symbolic,
		target,
		options,
		[&](const br::collision_resolution::crs_state& solution)
		{
			first_solution = solution;
			return false;
		},
		&statistics);
	const auto solving_end = std::chrono::steady_clock::now();

	for (auto& character : candidate)
		br::assign_assert_result<8>(
			character,
			first_solution.assignments);

	std::string recovered;
	recovered.reserve(candidate.size());
	for (const auto& character : candidate)
		recovered.push_back(static_cast<char>(
			std::stoul(character.__to_string(), nullptr, 2)));

	const auto construction_ms =
		std::chrono::duration_cast<std::chrono::milliseconds>(
			construction_end - construction_start).count();
	const auto solving_ms =
		std::chrono::duration_cast<std::chrono::milliseconds>(
			solving_end - solving_start).count();

	std::cout
		<< "Unknown bytes: " << target_message.size() - known_prefix_size << '\n'
		<< "Recovered: " << recovered << '\n'
		<< "Solutions emitted: " << solution_count << '\n'
		<< "Expression construction: " << construction_ms << " ms\n"
		<< "Solving: " << solving_ms << " ms\n"
		<< "Nodes: " << statistics.nodes << '\n'
		<< "Variables: " << statistics.variables << '\n'
		<< "Decisions: " << statistics.decisions << '\n'
		<< "Propagations: " << statistics.propagations << '\n'
		<< "Affine enabled: "
		<< (statistics.affine_enabled ? "yes" : "no") << '\n'
		<< "Affine atoms: " << statistics.affine_atoms << '\n'
		<< "Affine passes: " << statistics.affine_passes << '\n'
		<< "Conflicts: " << statistics.conflicts << '\n'
		<< "Peak trail: " << statistics.peak_trail << '\n'
		<< "Learned clauses: " << statistics.learned_clauses << '\n'
		<< "Backjumps: " << statistics.backjumps << '\n'
		<< "Conflict-analysis cutoffs: "
		<< statistics.conflict_analysis_cutoffs << '\n';
}
