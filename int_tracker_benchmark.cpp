#include <chrono>
#include <cstddef>
#include <cstdint>
#include <iomanip>
#include <iostream>
#include <stdexcept>
#include <string>
#include <string_view>

#include "bitreverse.h"

namespace br = dixelu::bitreverse;
using clock_type = std::chrono::steady_clock;
volatile std::uint64_t sink_value = 0;

// Timings are informational because they vary by machine. The symbolic
// benchmarks also enforce deterministic upper bounds on retained DAG nodes
// and expression depth, which makes the executable suitable for CTest.
// Functions named "prototype" are comparison implementations for the
// optimization opportunities identified by this benchmark.

template<typename Function>
void benchmark(
	std::string_view name,
	std::size_t iterations,
	Function&& function)
{
	function();
	const auto start = clock_type::now();
	for (std::size_t i = 0; i < iterations; ++i)
	{
		auto value = function();
		sink_value = sink_value + value.bits.back().bit_state->state;
	}
	const auto elapsed =
		std::chrono::duration<double, std::micro>(
			clock_type::now() - start).count();
	std::cout
		<< std::left << std::setw(24) << name << std::right
		<< std::setw(11) << std::fixed << std::setprecision(3)
		<< elapsed / iterations << " us/op\n";
}

br::bit_tracker compact_select(
	const br::bit_tracker& condition,
	const br::bit_tracker& lhs,
	const br::bit_tracker& rhs)
{
	return rhs ^ (condition & (lhs ^ rhs));
}

template<std::size_t N>
br::int_tracker<N> branch_multiply(
	const br::int_tracker<N>& lhs,
	const br::int_tracker<N>& rhs)
{
	br::int_tracker<N> result{};
	br::int_tracker<N> shifted = lhs;

	for (std::size_t offset = 0; offset < N; ++offset)
	{
		const auto& selector = rhs.bits[N - offset - 1];
		if (selector.bit_state->operation == '=')
		{
			if (selector.bit_state->state)
				result += shifted;
		}
		else
		{
			const auto sum = result + shifted;
			for (std::size_t i = 0; i < N; ++i)
				result.bits[i] =
					compact_select(
						selector,
						sum.bits[i],
						result.bits[i]);
		}
		shifted <<= 1;
	}
	return result;
}

template<std::size_t N>
bool same_concrete_value(
	const br::int_tracker<N>& lhs,
	const br::int_tracker<N>& rhs)
{
	for (std::size_t i = 0; i < N; ++i)
	{
		if (lhs.bits[i].bit_state->operation != '=' ||
			rhs.bits[i].bit_state->operation != '=' ||
			lhs.bits[i].bit_state->state !=
				rhs.bits[i].bit_state->state)
			return false;
	}
	return true;
}

template<typename Function>
void symbolic_benchmark(
	std::string_view name,
	std::size_t iterations,
	std::size_t maximum_nodes,
	std::size_t maximum_depth,
	Function&& function)
{
	auto& pool =
		dixelu::details::counted_control_block_pool<
			br::details::bitstate>();
	br::int_tracker<32> lhs = br::unknown;
	br::int_tracker<32> rhs = br::unknown;
	const auto before = pool.get_statistics();
	auto sample = function(lhs, rhs);
	const auto after = pool.get_statistics();
	const std::size_t retained_nodes = after.live - before.live;
	const std::size_t depth = sample.__max_depth();

	if (retained_nodes > maximum_nodes || depth > maximum_depth)
		throw std::runtime_error(
			std::string(name) +
			" exceeded its expression-graph budget");

	const auto start = clock_type::now();
	for (std::size_t i = 0; i < iterations; ++i)
	{
		auto result = function(lhs, rhs);
		sink_value =
			sink_value + result.bits.back().bit_state->operation;
	}
	const auto elapsed =
		std::chrono::duration<double, std::micro>(
			clock_type::now() - start).count();
	std::cout
		<< std::left << std::setw(24) << name << std::right
		<< std::setw(11) << std::fixed << std::setprecision(3)
		<< elapsed / iterations << " us/op, "
		<< std::setw(7) << retained_nodes
		<< " retained nodes, depth "
		<< depth << "\n";
}

int main()
{
	using u32 = br::int_tracker<32>;
	const u32 a{0x9e3779b9u};
	const u32 b{0x85ebca6bu};
	const auto expected_product = a * b;

	if (!same_concrete_value(
			branch_multiply(a, b),
			expected_product))
		throw std::runtime_error(
			"comparison prototype produced an invalid result");

	benchmark(
		"concrete construct",
		200000,
		[] { return u32{0x12345678u}; });
	benchmark("concrete add", 100000, [&] { return a + b; });
	benchmark("concrete subtract", 100000, [&] { return a - b; });
	benchmark("concrete multiply", 5000, [&] { return a * b; });
	benchmark(
		"branch multiply prototype",
		5000,
		[&] { return branch_multiply(a, b); });
	benchmark("concrete divide", 5000, [&] { return a / b; });
	benchmark("concrete shift", 100000, [&] { return a << 13; });

	std::cout << "\n";
	symbolic_benchmark(
		"symbolic add",
		50000,
		154,
		63,
		[](const auto& x, const auto& y) { return x + y; });
	symbolic_benchmark(
		"symbolic subtract",
		25000,
		215,
		94,
		[](const auto& x, const auto& y) { return x - y; });
	symbolic_benchmark(
		"symbolic multiply",
		2000,
		6296,
		183,
		[](const auto& x, const auto& y) { return x * y; });
	symbolic_benchmark(
		"symbolic variable shift",
		20000,
		578,
		12,
		[](const auto& x, const auto& y)
		{
			return x << br::ref_handler<u32>(y);
		});

	std::cout << "sink " << sink_value << "\n";
}
