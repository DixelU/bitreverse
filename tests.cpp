#include <array>
#include <cstddef>
#include <cstdint>
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

constexpr bool constexpr_boolean_operation_test()
{
	const br::bit_tracker false_value(false);
	const br::bit_tracker true_value(true);
	const auto result = (false_value | true_value) & !false_value;
	return result.bit_state->operation == '=' &&
		result.bit_state->state;
}

static_assert(constexpr_boolean_operation_test());

template<size_t N>
constexpr bool constexpr_matches(
	const br::int_tracker<N>& value,
	std::uintmax_t expected)
{
	for (size_t offset = 0; offset < N; ++offset)
	{
		const auto& bit = value.bits[N - offset - 1];
		if (bit.bit_state->operation != '=' ||
			bit.bit_state->state != (expected & 1))
			return false;
		expected >>= 1;
	}
	return expected == 0;
}

constexpr bool constexpr_division_test()
{
	const br::int_tracker<4> dividend{13};
	const br::int_tracker<4> divisor{3};
	return constexpr_matches(dividend / divisor, 4) &&
		constexpr_matches(dividend % divisor, 1);
}

static_assert(constexpr_division_test());

void require(bool condition, const char* message)
{
	if (!condition)
		throw std::runtime_error(message);
}

struct pool_probe
{
	static inline size_t live = 0;
	static inline size_t destroyed = 0;

	explicit pool_probe(size_t value) : value(value)
	{
		++live;
	}

	~pool_probe()
	{
		--live;
		++destroyed;
	}

	size_t value;
};

struct destruction_chain_node
{
	static inline size_t destroyed = 0;

	explicit destruction_chain_node(
		dixelu::counted_ptr<destruction_chain_node>&& next) :
		next(std::move(next))
	{}

	~destruction_chain_node()
	{
		++destroyed;
	}

	dixelu::counted_ptr<destruction_chain_node> next;
};

void counted_ptr_pool_tests()
{
	constexpr size_t object_count = 513;
	pool_probe::live = 0;
	pool_probe::destroyed = 0;
	auto& pool =
		dixelu::details::counted_control_block_pool<pool_probe>();
	const auto initial_statistics = pool.get_statistics();

	std::vector<dixelu::counted_ptr<pool_probe>> objects;
	objects.reserve(object_count);
	for (size_t i = 0; i < object_count; ++i)
		objects.push_back(dixelu::make_counted<pool_probe>(i));
	const auto populated_statistics = pool.get_statistics();
	require(
		populated_statistics.slabs == initial_statistics.slabs + 1 &&
			populated_statistics.live == object_count,
		"counted_ptr must allocate objects in a buffered slab");

	std::set<const pool_probe*> released_addresses;
	for (size_t i = 0; i < object_count; i += 3)
	{
		released_addresses.insert(objects[i].get());
		objects[i].reset();
	}

	std::vector<dixelu::counted_ptr<pool_probe>> replacements;
	replacements.reserve(released_addresses.size());
	for (size_t i = 0; i < released_addresses.size(); ++i)
	{
		auto replacement = dixelu::make_counted<pool_probe>(object_count + i);
		require(
			released_addresses.contains(replacement.get()),
			"counted_ptr pool must recycle arbitrary-order releases");
		replacements.push_back(std::move(replacement));
	}

	const auto reused_statistics = pool.get_statistics();
	require(
		reused_statistics.slabs == populated_statistics.slabs &&
			reused_statistics.live == object_count &&
			pool_probe::live == object_count,
		"counted_ptr pool lost track of a live object");
	objects.clear();
	replacements.clear();
	const auto cleared_statistics = pool.get_statistics();
	require(
		cleared_statistics.live == 0 &&
			pool_probe::live == 0 &&
			pool_probe::destroyed == object_count + released_addresses.size(),
		"counted_ptr pool must destroy each object exactly once");
}

void counted_ptr_iterative_destruction_test()
{
	constexpr size_t chain_length = 250000;
	destruction_chain_node::destroyed = 0;
	auto& pool =
		dixelu::details::counted_control_block_pool<
			destruction_chain_node>();
	const auto initial_statistics = pool.get_statistics();

	dixelu::counted_ptr<destruction_chain_node> chain;
	for (size_t i = 0; i < chain_length; ++i)
	{
		chain =
			dixelu::make_counted<destruction_chain_node>(
				std::move(chain));
	}

	require(
		pool.get_statistics().live ==
			initial_statistics.live + chain_length,
		"deep ownership chain construction lost nodes");
	chain.reset();
	require(
		destruction_chain_node::destroyed == chain_length &&
			pool.get_statistics().live == initial_statistics.live,
		"deep ownership chain must be destroyed iteratively");
}

void expression_simplification_tests()
{
	const br::bit_tracker false_a(false);
	const br::bit_tracker false_b(false);
	const br::bit_tracker true_a(true);
	const br::bit_tracker true_b(true);
	require(
		false_a.bit_state == false_b.bit_state &&
			true_a.bit_state == true_b.bit_state &&
			false_a.bit_state != true_a.bit_state,
		"boolean constants must use the shared flyweight nodes");

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

	br::solver_options learning_options;
	learning_options.affine_reasoning = false;
	learning_options.conflict_learning = true;
	br::solver_statistics learning_statistics;
	const auto learning_all = br::assert_equality(
		expression,
		expected,
		learning_options,
		false,
		&learning_statistics);
	require(
		learning_all.size() == 5 &&
			learning_statistics.solutions == 5 &&
			learning_statistics.decisions != 0 &&
			learning_statistics.propagations != 0,
		"CDCL must enumerate the same five complete assignments");
	require(
		br::assert_equality(
			expression,
			expected,
			learning_options,
			true).size() == 1,
		"CDCL first-only solving must stop after one model");

	std::set<unsigned> learned_streamed_assignments;
	size_t learned_callbacks = 0;
	const size_t learned_streamed_count = br::assert_equality(
		expression,
		expected,
		learning_options,
		[&](const br::collision_resolution::crs_state& solution)
		{
			const unsigned assignment =
				static_cast<unsigned>(solution.assignments.at(x.bit_state)) |
				(static_cast<unsigned>(
					solution.assignments.at(y.bit_state)) << 1) |
				(static_cast<unsigned>(
					solution.assignments.at(z.bit_state)) << 2);
			learned_streamed_assignments.insert(assignment);
			return ++learned_callbacks < 3;
		});
	require(
		learned_streamed_count == 3 &&
			learned_streamed_assignments.size() == 3,
		"CDCL streaming must emit unique models and stop immediately");

	br::bit_tracker conflict_x;
	br::bit_tracker conflict_y;
	conflict_x = br::unknown;
	conflict_y = br::unknown;
	const auto learned_unsatisfiable =
		(conflict_x | conflict_y) &
		((!conflict_x) | conflict_y) &
		(conflict_x | (!conflict_y)) &
		((!conflict_x) | (!conflict_y));
	br::solver_statistics conflict_statistics;
	bool learned_conflict_rejected = false;
	try
	{
		(void)br::assert_equality(
			learned_unsatisfiable,
			expected,
			learning_options,
			false,
			&conflict_statistics);
	}
	catch (const std::runtime_error&)
	{
		learned_conflict_rejected = true;
	}
	require(
		learned_conflict_rejected &&
			conflict_statistics.conflicts != 0 &&
			conflict_statistics.learned_clauses != 0,
		"CDCL must learn from a non-trivial unsatisfiable formula");

	br::solver_options cutoff_options = learning_options;
	cutoff_options.max_conflict_analysis_nodes = 1;
	br::solver_statistics cutoff_statistics;
	bool cutoff_unsatisfiable = false;
	try
	{
		(void)br::assert_equality(
			learned_unsatisfiable,
			expected,
			cutoff_options,
			false,
			&cutoff_statistics);
	}
	catch (const std::runtime_error&)
	{
		cutoff_unsatisfiable = true;
	}
	require(
		cutoff_unsatisfiable &&
			cutoff_statistics.conflict_analysis_cutoffs != 0,
		"bounded CDCL analysis must fall back without losing completeness");

	bool unexplained_affine_rejected = false;
	try
	{
		br::solver_options incompatible_options;
		incompatible_options.conflict_learning = true;
		(void)br::assert_equality(
			expression,
			expected,
			incompatible_options,
			true);
	}
	catch (const std::logic_error&)
	{
		unexplained_affine_rejected = true;
	}
	require(
		unexplained_affine_rejected,
		"CDCL must reject active affine propagation without explanations");

	br::bit_tracker jump_a, jump_c, jump_d, jump_x;
	br::bit_tracker jump_p, jump_q, jump_r, jump_s;
	br::bit_tracker jump_t, jump_u, jump_v;
	jump_a = br::unknown;
	jump_c = br::unknown;
	jump_d = br::unknown;
	jump_x = br::unknown;
	jump_p = br::unknown;
	jump_q = br::unknown;
	jump_r = br::unknown;
	jump_s = br::unknown;
	jump_t = br::unknown;
	jump_u = br::unknown;
	jump_v = br::unknown;

	// Fanout orders the decisions as a, c, d. Once a=d=true, the two
	// trap clauses imply both x and !x. The learned (!a | !d) clause can
	// therefore skip the irrelevant c decision level.
	const auto jump_padding_a =
		(jump_a | jump_p) & (jump_a | jump_q);
	const auto jump_padding_c =
		(jump_c | jump_u) &
		(jump_c | jump_v) &
		(jump_c | jump_r) &
		(jump_c | jump_t);
	const auto jump_padding_d = jump_d | jump_s;
	const auto jump_trap =
		((!jump_a) | (!jump_d) | jump_x) &
		((!jump_a) | (!jump_d) | (!jump_x));
	const auto jump_expression =
		jump_padding_a &
		jump_padding_c &
		jump_padding_d &
		jump_trap &
		(jump_t | jump_a);

	br::solver_options jump_options = learning_options;
	jump_options.max_conflict_analysis_nodes = 0;
	br::solver_statistics jump_statistics;
	require(
		br::assert_equality(
			jump_expression,
			expected,
			jump_options,
			true,
			&jump_statistics).size() == 1 &&
			jump_statistics.learned_clauses != 0 &&
			jump_statistics.backjumps != 0,
		"first-UIP learning must skip an irrelevant decision level");

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
	bool expected,
	const br::solver_options* options = nullptr)
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
	const auto solutions = options
		? br::assert_equality(
			expression,
			expected_bit,
			*options)
		: br::assert_equality(expression, expected_bit);
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

	br::solver_options learning_options;
	learning_options.affine_reasoning = false;
	learning_options.conflict_learning = true;
	for (const char operation : {'&', '|', '^'})
		for (const bool expected : {false, true})
			require(
				solve_binary_truth_table(
					operation,
					expected,
					&learning_options) ==
				solve_binary_truth_table(operation, expected),
				"CDCL gate truth table differs from DPLL");

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

void multiplication_and_division_tests()
{
	using u8 = br::int_tracker<8>;
	using u32 = br::int_tracker<32>;

	const auto is_same = []<size_t N>(
		const br::int_tracker<N>& actual,
		std::uintmax_t expected)
	{
		const br::int_tracker<N> expected_value{expected};
		for (size_t i = 0; i < N; ++i)
			if (actual.bits[i].bit_state->state !=
				expected_value.bits[i].bit_state->state)
				return false;
		return true;
	};

	constexpr std::array<std::uint8_t, 16> divisors{
		1, 2, 3, 7, 8, 15, 16, 31,
		32, 63, 64, 127, 128, 129, 254, 255};

	require(is_same(u8{255} * u8{0}, 0), "broken multiplication");
	for (std::uint16_t i = 0; i <= UINT8_MAX; ++i)
	{
		for (const std::uint8_t j : divisors)
		{
			require(
				is_same(u8{i} * u8{j}, static_cast<std::uint8_t>(i * j)),
				"broken multiplication");
			require(is_same(u8{i} / u8{j}, i / j), "broken division");
			require(is_same(u8{i} % u8{j}, i % j), "broken remainder");

			u8 difference{i};
			br::bit_tracker no_underflow = false;
			difference.self_sub_ret_carry(u8{j}, no_underflow);
			require(
				is_same(
					difference,
					static_cast<std::uint8_t>(i - j)),
				"broken direct subtraction");
			require(
				is_constant(no_underflow, i >= j),
				"subtraction carry must indicate no underflow");
		}
	}

	for (std::uint16_t dividend = 0;
		dividend <= UINT8_MAX;
		++dividend)
	{
		for (std::uint16_t divisor = 1;
			divisor <= UINT8_MAX;
			++divisor)
		{
			require(
				is_same(
					u8{dividend} / u8{divisor},
					dividend / divisor),
				"broken exhaustive division");
			require(
				is_same(
					u8{dividend} % u8{divisor},
					dividend % divisor),
				"broken exhaustive remainder");
		}
	}

	std::uint32_t random_state = 0x6d2b79f5;
	for (size_t sample = 0; sample < 512; ++sample)
	{
		random_state = random_state * 1664525u + 1013904223u;
		const std::uint32_t lhs = random_state;
		random_state = random_state * 1664525u + 1013904223u;
		const std::uint32_t rhs_candidate =
			random_state ^ (random_state >> 16);
		const std::uint32_t rhs = rhs_candidate ? rhs_candidate : 1;

		require(
			is_same(
				u32{lhs} * u32{rhs},
				static_cast<std::uint32_t>(
					static_cast<std::uint64_t>(lhs) * rhs)),
			"broken multiplication");
		require(is_same(u32{lhs} / u32{rhs}, lhs / rhs), "broken division");
		require(is_same(u32{lhs} % u32{rhs}, lhs % rhs), "broken remainder");
	}
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
	counted_ptr_pool_tests();
	counted_ptr_iterative_destruction_test();
	multiplication_and_division_tests();
	expression_simplification_tests();
	solver_regression_tests();
	exhaustive_gate_solver_tests();
	crc_hybrid_solver_regression_test();
	md5_forward_regression_tests();
	md5_one_unknown_byte_reversal_test();
	std::cout << "All bitreverse tests passed\n";
}
