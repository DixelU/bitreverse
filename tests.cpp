#include <cstddef>
#include <iostream>
#include <set>
#include <stdexcept>

#include "bitreverse.h"
#include "md5.h"

namespace br = dixelu::bitreverse;

namespace
{

bool is_constant(const br::bit_tracker& bit, bool expected)
{
	return bit.bit_state->operation == '=' &&
		static_cast<bool>(bit.bit_state->state) == expected;
}

void require(bool condition, const char* message)
{
	if (!condition)
		throw std::runtime_error(message);
}

void expression_simplification_tests()
{
	br::bit_tracker x;
	x = br::unknown;
	const auto same_as_x = [&](const br::bit_tracker& bit)
	{
		return bit.bit_state == x.bit_state;
	};

	require(same_as_x(x & x), "x & x must simplify to x");
	require(same_as_x(x | x), "x | x must simplify to x");
	require(is_constant(x ^ x, false), "x ^ x must simplify to false");

	require(is_constant(x & !x, false), "x & !x must simplify to false");
	require(is_constant(x | !x, true), "x | !x must simplify to true");
	require(is_constant(x ^ !x, true), "x ^ !x must simplify to true");
	require(same_as_x(!!x), "!!x must simplify to x");

	br::bit_tracker y;
	y = br::unknown;
	require(same_as_x(x & (x | y)), "x & (x | y) must simplify to x");
	require(same_as_x((y | x) & x), "(y | x) & x must simplify to x");
	require(same_as_x(x | (x & y)), "x | (x & y) must simplify to x");
	require(same_as_x((y & x) | x), "(y & x) | x must simplify to x");

	// CRC-style mask construction: negating an integer containing only one
	// symbolic low bit produces that bit replicated across the entire word.
	const br::int_tracker<8> one_bit(x);
	const auto mask = -one_bit;
	for (const auto& bit : mask.bits)
		require(same_as_x(bit), "-int_tracker(bit) must replicate bit");
}

void solver_regression_tests()
{
	br::bit_tracker x;
	br::bit_tracker y;
	br::bit_tracker z;
	x = br::unknown;
	y = br::unknown;
	z = br::unknown;

	const auto expression = (x & y) | z;
	const br::bit_tracker expected(true);

	const auto all = br::assert_equality(expression, expected);
	require(all.size() == 5, "(x & y) | z must have five complete true assignments");

	const auto first = br::assert_equality(expression, expected, true);
	require(first.size() == 1, "first-only solving must return one assignment");

	std::set<unsigned> streamed_assignments;
	const size_t streamed_count = br::assert_equality(
		expression,
		expected,
		[&](const br::collision_resolution::crs_state& solution)
		{
			const unsigned assignment =
				static_cast<unsigned>(solution.assignments.at(x.bit_state)) |
				(static_cast<unsigned>(
					solution.assignments.at(y.bit_state)) << 1) |
				(static_cast<unsigned>(
					solution.assignments.at(z.bit_state)) << 2);
			streamed_assignments.insert(assignment);
		});
	require(
		streamed_count == 5 && streamed_assignments.size() == 5,
		"streaming must emit every complete assignment exactly once");

	size_t early_stop_callbacks = 0;
	const size_t early_stop_count = br::assert_equality(
		expression,
		expected,
		[&](const br::collision_resolution::crs_state&)
		{
			return ++early_stop_callbacks < 2;
		});
	require(
		early_stop_count == 2 && early_stop_callbacks == 2,
		"a false callback result must stop streaming immediately");

	br::solver_options baseline_options;
	baseline_options.affine_reasoning = false;
	br::solver_statistics baseline_statistics;
	const size_t baseline_count = br::assert_equality(
		expression,
		expected,
		baseline_options,
		[](const br::collision_resolution::crs_state&) {},
		&baseline_statistics);
	require(
		baseline_count == 5 &&
			baseline_statistics.solutions == 5 &&
			baseline_statistics.variables == 3 &&
			baseline_statistics.nodes >= 3 &&
			baseline_statistics.decisions != 0 &&
			baseline_statistics.propagations != 0 &&
			baseline_statistics.affine_passes == 0,
		"baseline options and statistics mismatch");

	bool unavailable_learning_rejected = false;
	try
	{
		br::solver_options unavailable_options;
		unavailable_options.conflict_learning = true;
		(void)br::assert_equality(
			expression,
			expected,
			unavailable_options,
			true);
	}
	catch (const std::logic_error&)
	{
		unavailable_learning_rejected = true;
	}
	require(
		unavailable_learning_rejected,
		"unimplemented conflict learning must not silently run DPLL");

	bool unsatisfiable = false;
	try
	{
		const br::bit_tracker impossible = x & !x;
		(void)br::assert_equality(impossible, expected);
	}
	catch (const std::runtime_error&)
	{
		unsatisfiable = true;
	}
	require(unsatisfiable, "constant-false equality with true must be unsatisfiable");
}

using truth_table_t = std::set<unsigned>;

truth_table_t solve_binary_truth_table(
	char operation,
	bool expected)
{
	br::bit_tracker x;
	br::bit_tracker y;
	x = br::unknown;
	y = br::unknown;

	br::bit_tracker expression;
	switch (operation)
	{
		case '&': expression = x & y; break;
		case '|': expression = x | y; break;
		case '^': expression = x ^ y; break;
		default: throw std::logic_error("unknown test operation");
	}

	const br::bit_tracker expected_bit(expected);
	const auto solutions =
		br::assert_equality(expression, expected_bit);
	truth_table_t result;
	for (const auto& solution : solutions)
	{
		const bool x_value = solution.assignments.at(x.bit_state);
		const bool y_value = solution.assignments.at(y.bit_state);
		result.insert(
			static_cast<unsigned>(x_value) |
			(static_cast<unsigned>(y_value) << 1));
	}
	return result;
}

void exhaustive_gate_solver_tests()
{
	require(
		solve_binary_truth_table('&', false) == truth_table_t{0, 1, 2},
		"AND=false truth table mismatch");
	require(
		solve_binary_truth_table('&', true) == truth_table_t{3},
		"AND=true truth table mismatch");
	require(
		solve_binary_truth_table('|', false) == truth_table_t{0},
		"OR=false truth table mismatch");
	require(
		solve_binary_truth_table('|', true) == truth_table_t{1, 2, 3},
		"OR=true truth table mismatch");
	require(
		solve_binary_truth_table('^', false) == truth_table_t{0, 3},
		"XOR=false truth table mismatch");
	require(
		solve_binary_truth_table('^', true) == truth_table_t{1, 2},
		"XOR=true truth table mismatch");

	br::bit_tracker x;
	x = br::unknown;
	const auto not_x = !x;
	const br::bit_tracker false_bit(false);
	const br::bit_tracker true_bit(true);
	const auto not_false =
		br::assert_equality(not_x, false_bit);
	const auto not_true =
		br::assert_equality(not_x, true_bit);
	require(
		not_false.size() == 1 &&
			not_false.begin()->assignments.at(x.bit_state),
		"NOT=false truth table mismatch");
	require(
		not_true.size() == 1 &&
			!not_true.begin()->assignments.at(x.bit_state),
		"NOT=true truth table mismatch");
}

template<template<size_t> typename int_tracker>
int_tracker<32> tracked_crc32(const std::vector<int_tracker<8>>& message)
{
	int_tracker<32> byte;
	int_tracker<32> mask;
	int_tracker<32> crc = 0xFFFFFFFF;
	const int_tracker<32> polynomial = 0xEDB88320;

	for (const auto& character : message)
	{
		byte = int_tracker<32>(character);
		crc ^= byte;

		for (int bit = 0; bit < 8; ++bit)
		{
			mask = -(crc & 1);
			crc = (crc >> 1) ^ (polynomial & mask);
		}
	}

	return ~crc;
}

void crc_hybrid_solver_regression_test()
{
	const std::vector<br::itu8> known_message =
		{'b', 'i', 't', 'r', 'e', 'v', '!'};
	const auto expected_crc = tracked_crc32(known_message);

	std::vector<br::itu8> unknown_message =
		{br::unknown, br::unknown, br::unknown, br::unknown,
			br::unknown, br::unknown, br::unknown};
	const auto symbolic_crc = tracked_crc32(unknown_message);

	br::collision_resolution::crs_state first_solution;
	br::solver_options options;
	br::solver_statistics statistics;
	const size_t solution_count = br::assert_equality<32>(
		symbolic_crc,
		expected_crc,
		options,
		[&](const br::collision_resolution::crs_state& solution)
		{
			first_solution = solution;
			return false;
		},
		&statistics);
	require(
		solution_count == 1 &&
			statistics.solutions == 1 &&
			statistics.variables == 56 &&
			statistics.affine_enabled &&
			statistics.affine_atoms != 0 &&
			statistics.affine_passes != 0,
		"seven-byte CRC stream must find one solution before stopping");

	for (auto& byte : unknown_message)
		br::assign_assert_result<8>(
			byte,
			first_solution.assignments);

	const auto actual_crc = tracked_crc32(unknown_message);
	require(
		actual_crc.__to_string() == expected_crc.__to_string(),
		"reversed message must reproduce the target CRC");
}

template<size_t N>
std::string tracker_hex(const br::int_tracker<N>& value)
{
	static constexpr char digits[] = "0123456789abcdef";
	static_assert(N % 4 == 0);

	const std::string bits = value.__to_string();
	std::string result;
	result.reserve(N / 4);
	for (size_t offset = 0; offset < N; offset += 4)
	{
		unsigned nibble = 0;
		for (size_t bit = 0; bit < 4; ++bit)
		{
			require(
				bits[offset + bit] == '0' || bits[offset + bit] == '1',
				"hex conversion requires a concrete tracker");
			nibble =
				(nibble << 1) |
				static_cast<unsigned>(bits[offset + bit] - '0');
		}
		result.push_back(digits[nibble]);
	}
	return result;
}

std::vector<br::itu8> tracked_message(const std::string& message)
{
	std::vector<br::itu8> result;
	result.reserve(message.size());
	for (const unsigned char character : message)
		result.emplace_back(character);
	return result;
}

void md5_forward_regression_tests()
{
	require(
		tracker_hex(br::hash::md5(tracked_message(""))) ==
			"d41d8cd98f00b204e9800998ecf8427e",
		"MD5 empty-string vector mismatch");
	require(
		tracker_hex(br::hash::md5(tracked_message("a"))) ==
			"0cc175b9c0f1b6a831c399e269772661",
		"MD5 one-byte vector mismatch");
	require(
		tracker_hex(br::hash::md5(tracked_message("abc"))) ==
			"900150983cd24fb0d6963f7d28e17f72",
		"MD5 three-byte vector mismatch");
}

void md5_one_unknown_byte_reversal_test()
{
	const auto target = br::hash::md5(tracked_message("md5!"));

	std::vector<br::itu8> candidate = {'m', 'd', '5', br::unknown};
	const auto symbolic = br::hash::md5(candidate);

	br::collision_resolution::crs_state first_solution;
	const size_t solution_count = br::assert_equality<128>(
		symbolic,
		target,
		[&](const br::collision_resolution::crs_state& solution)
		{
			first_solution = solution;
			return false;
		});
	require(
		solution_count == 1,
		"one-byte MD5 reversal must find a solution");

	br::assign_assert_result<8>(
		candidate.back(),
		first_solution.assignments);
	require(
		tracker_hex(br::hash::md5(candidate)) == tracker_hex(target),
		"reversed MD5 candidate must reproduce the target");
}

}

int main()
{
	expression_simplification_tests();
	solver_regression_tests();
	exhaustive_gate_solver_tests();
	crc_hybrid_solver_regression_test();
	md5_forward_regression_tests();
	md5_one_unknown_byte_reversal_test();
	std::cout << "All bitreverse tests passed\n";
}
