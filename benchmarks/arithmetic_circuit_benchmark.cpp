#include <chrono>
#include <iomanip>
#include <iostream>
#include <string_view>

#include "circuit_metrics.h"

namespace br = dixelu::bitreverse;

template<size_t N, typename Function>
void measure(std::string_view operation, Function function)
{
	const br::int_tracker<N> x = br::unknown;
	const br::int_tracker<N> y = br::unknown;
	const auto started = std::chrono::steady_clock::now();
	const auto result = function(x, y);
	const double milliseconds = std::chrono::duration<double, std::milli>(
		std::chrono::steady_clock::now() - started).count();
	const auto shape = br::measure_circuit(result);
	std::cout << N << ',' << operation << ',' << shape.nodes << ','
		<< shape.gates << ',' << shape.max_depth << ',' << shape.max_width
		<< ',' << milliseconds << '\n';
}

template<size_t N>
void run()
{
	using word = br::int_tracker<N>;
	measure<N>("add", [](const auto& x, const auto& y) { return x + y; });
	measure<N>("subtract", [](const auto& x, const auto& y) { return x - y; });
	measure<N>("increment", [](const auto& x, const auto&) { return x + word{1}; });
	measure<N>("multiply", [](const auto& x, const auto& y) { return x * y; });
	measure<N>("multiply_13", [](const auto& x, const auto&) { return x * word{13}; });
	measure<N>("13_multiply", [](const auto& x, const auto&) { return word{13} * x; });
	measure<N>("divide", [](const auto& x, const auto& y) { return x / y; });
	measure<N>("remainder", [](const auto& x, const auto& y) { return x % y; });
	measure<N>("divide_8", [](const auto& x, const auto&) { return x / word{8}; });
	measure<N>("remainder_8", [](const auto& x, const auto&) { return x % word{8}; });
	measure<N>("divide_10", [](const auto& x, const auto&) { return x / word{10}; });
	measure<N>("remainder_10", [](const auto& x, const auto&) { return x % word{10}; });
	measure<N>("37_divide", [](const auto& x, const auto&) { return word{37} / x; });
	measure<N>("37_remainder", [](const auto& x, const auto&) { return word{37} % x; });
}

int main()
{
	std::cout << "bits,operation,nodes,gates,depth,width,build_ms\n"
		<< std::fixed << std::setprecision(6);
	run<8>();
	run<32>();
	run<64>();
	run<256>();
}
