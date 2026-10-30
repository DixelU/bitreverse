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

// Timings vary by machine and are informational. The symbolic benchmarks also
// enforce upper bounds on retained DAG nodes and expression depth, which is
// what the int_tracker_performance_regression CTest checks.

void print_timing(std::string_view name, clock_type::time_point start, std::size_t iterations)
{
	const auto elapsed = std::chrono::duration<double, std::micro>(clock_type::now() - start).count();
	std::cout << std::left << std::setw(24) << name << std::right
		<< std::setw(11) << std::fixed << std::setprecision(3) << elapsed / iterations << " us/op";
}

template<typename Function>
void benchmark(std::string_view name, std::size_t iterations, Function&& function)
{
	function();
	const auto start = clock_type::now();
	for (std::size_t i = 0; i < iterations; ++i)
	{
		auto value = function();
		sink_value = sink_value + value.bits.back().bit_state->state;
	}
	print_timing(name, start, iterations);
	std::cout << '\n';
}

template<typename Function>
void symbolic_benchmark(std::string_view name, std::size_t iterations,
	std::size_t maximum_nodes, std::size_t maximum_depth, Function&& function)
{
	auto& pool = dixelu::details::counted_control_block_pool<br::details::bitstate>();
	br::int_tracker<32> lhs = br::unknown;
	br::int_tracker<32> rhs = br::unknown;
	const auto before = pool.get_statistics();
	auto sample = function(lhs, rhs);
	const auto after = pool.get_statistics();
	const std::size_t retained_nodes = after.live - before.live;
	const std::size_t depth = sample.max_depth();

	if (retained_nodes > maximum_nodes || depth > maximum_depth)
		throw std::runtime_error(std::string(name) + " exceeded its expression-graph budget");

	const auto start = clock_type::now();
	for (std::size_t i = 0; i < iterations; ++i)
	{
		auto result = function(lhs, rhs);
		sink_value = sink_value + result.bits.back().bit_state->operation;
	}
	print_timing(name, start, iterations);
	std::cout << ", " << std::setw(7) << retained_nodes << " retained nodes, depth " << depth << "\n";
}

int main()
{
	using u32 = br::int_tracker<32>;
	const u32 a{0x9e3779b9u};
	const u32 b{0x85ebca6bu};

	// Concrete operands must fold to constants equal to native arithmetic.
	if (!u32::are_equal(a * b, u32{0x9e3779b9u * 0x85ebca6bu}).is_constant(true) ||
		!u32::are_equal(a / b, u32{0x9e3779b9u / 0x85ebca6bu}).is_constant(true))
		throw std::runtime_error("concrete arithmetic disagrees with native arithmetic");

	benchmark("concrete construct", 200000, [] { return u32{0x12345678u}; });
	benchmark("concrete add", 100000, [&] { return a + b; });
	benchmark("concrete subtract", 100000, [&] { return a - b; });
	benchmark("concrete multiply", 5000, [&] { return a * b; });
	benchmark("concrete divide", 5000, [&] { return a / b; });
	benchmark("concrete remainder", 5000, [&] { return a % b; });
	benchmark("concrete shift", 100000, [&] { return a << 13; });

	std::cout << "\n";
	symbolic_benchmark("symbolic add", 50000, 220, 19, [](const auto& x, const auto& y) { return x + y; });
	symbolic_benchmark("symbolic subtract", 25000, 254, 21, [](const auto& x, const auto& y) { return x - y; });
	symbolic_benchmark("symbolic multiply", 2000, 2997, 37, [](const auto& x, const auto& y) { return x * y; });
	symbolic_benchmark("symbolic divide", 500, 8533, 505, [](const auto& x, const auto& y) { return x / y; });
	symbolic_benchmark("symbolic remainder", 500, 8902, 524, [](const auto& x, const auto& y) { return x % y; });
	symbolic_benchmark("symbolic variable shift", 20000, 578, 12, [](const auto& x, const auto& y) { return x << y; });

	std::cout << "sink " << sink_value << "\n";
}
