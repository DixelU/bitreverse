#ifndef DIXELU_BITREVERSE_PARTIAL_ASSERT_DEMO_SUPPORT_H
#define DIXELU_BITREVERSE_PARTIAL_ASSERT_DEMO_SUPPORT_H

#include "partial_assert.h"

#include <chrono>
#include <cstdint>
#include <iomanip>
#include <iostream>
#include <stdexcept>
#include <string_view>
#include <vector>

namespace partial_assert_demo
{
namespace br = dixelu::bitreverse;
using clock = std::chrono::steady_clock;

inline void require(bool condition, const char* message)
{
	if (!condition) throw std::runtime_error(message);
}

template<size_t N>
void append_bits(std::vector<br::bit_tracker>& roots, const br::int_tracker<N>& value)
{
	roots.insert(roots.end(), value.bits.begin(), value.bits.end());
}

inline size_t node_count(const std::vector<br::bit_tracker>& roots)
{
	std::vector<dixelu::counted_ptr<br::details::bitstate>> pointers;
	for (const auto& root : roots) pointers.push_back(root.bit_state);
	return br::collision_resolution::solver_core::compiled_circuit(std::move(pointers)).nodes.size();
}

template<size_t N>
size_t node_count(const br::int_tracker<N>& value)
{
	std::vector<br::bit_tracker> roots;
	append_bits(roots, value);
	return node_count(roots);
}

inline std::uint8_t decoded_byte(const br::itu8& input, const br::collision_resolution::crs_state& model)
{
	std::uint8_t result = 0;
	for (const auto& bit : input.bits)
	{
		const bool value = bit.bit_state->operation == '=' ? bit.bit_state->state != 0 :
			model.assignments.at(bit.bit_state);
		result = static_cast<std::uint8_t>((result << 1) | value);
	}
	return result;
}

inline double milliseconds(clock::duration duration)
{
	return std::chrono::duration<double, std::milli>(duration).count();
}

struct result
{
	size_t digest_nodes{}, retained_nodes{}, known_after_checkpoint{}, assertions{}, solutions{};
	size_t solver_decisions{}, solver_steps{};
	double build_and_assert_ms{}, final_solve_ms{};
	std::vector<std::uint8_t> recovered;
};

inline void print(std::string_view label, const result& value)
{
	std::cout << label << ": digest_nodes=" << value.digest_nodes
		<< ", retained_nodes=" << value.retained_nodes
		<< ", known_after_checkpoint=" << value.known_after_checkpoint
		<< ", assertions=" << value.assertions
		<< ", candidates=" << value.solutions
		<< ", final_solver_decisions=" << value.solver_decisions
		<< ", final_solver_steps=" << value.solver_steps
		<< std::fixed << std::setprecision(3)
		<< ", build_and_assert_ms=" << value.build_and_assert_ms
		<< ", final_solve_ms=" << value.final_solve_ms << '\n';
}
}

#endif
