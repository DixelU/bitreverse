#include "inverse/affine_plan.h"

#include <bit>
#include <cstdint>
#include <iomanip>
#include <iostream>
#include <random>
#include <set>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

namespace br = dixelu::bitreverse;

namespace
{

using plan_type = br::inversion::detail::affine_plan;
using circuit_type = br::collision_resolution::solver_core::compiled_circuit;

void require(bool condition, const char* message)
{
	if (!condition)
		throw std::runtime_error(message);
}

template<typename Exception, typename Function>
void require_throws(Function&& function, const char* message)
{
	try
	{
		function();
	}
	catch (const Exception&)
	{
		return;
	}
	throw std::runtime_error(message);
}

std::vector<br::bit_tracker> unknown_bits(size_t count)
{
	std::vector<br::bit_tracker> result(count);
	for (auto& bit : result)
		bit = br::unknown;
	return result;
}

std::optional<plan_type> compile_plan(
	const std::vector<br::bit_tracker>& outputs,
	const std::vector<br::bit_tracker>& inputs)
{
	// Output-first roots deliberately do not impose a topological node order.
	std::vector<dixelu::counted_ptr<br::details::bitstate>> roots;
	for (const auto& output : outputs)
		roots.push_back(output.bit_state);
	for (const auto& input : inputs)
		roots.push_back(input.bit_state);
	circuit_type circuit(std::move(roots));
	std::vector<size_t> output_ids, input_ids;
	for (const auto& output : outputs)
		output_ids.push_back(circuit.node_ids.at(output.bit_state.get()));
	for (const auto& input : inputs)
		input_ids.push_back(circuit.node_ids.at(input.bit_state.get()));
	return plan_type::compile(circuit, output_ids, input_ids);
}

std::vector<bool> unpack(unsigned value, size_t count)
{
	std::vector<bool> result(count);
	for (size_t bit = 0; bit < count; ++bit)
		result[bit] = (value >> bit) & 1U;
	return result;
}

unsigned pack(const std::vector<bool>& value)
{
	unsigned result = 0;
	for (size_t bit = 0; bit < value.size(); ++bit)
		result |= static_cast<unsigned>(value[bit]) << bit;
	return result;
}

void exhaustive_affine_maps()
{
	std::mt19937 random(19091);
	for (size_t input_count = 0; input_count <= 6; ++input_count)
	{
		for (size_t output_count = 0; output_count <= 5; ++output_count)
		{
			for (size_t trial = 0; trial < 40; ++trial)
			{
				const auto inputs = unknown_bits(input_count);
				std::vector<unsigned> masks(output_count);
				std::vector<bool> constants(output_count);
				std::vector<br::bit_tracker> outputs;
				for (size_t row = 0; row < output_count; ++row)
				{
					masks[row] = random() % (1U << input_count);
					constants[row] = random() % 2;
					br::bit_tracker expression(constants[row]);
					for (size_t column = 0; column < input_count; ++column)
						if (masks[row] & (1U << column))
							expression ^= inputs[column];
					outputs.push_back(expression);
				}

				const auto plan = compile_plan(outputs, inputs);
				require(plan.has_value(), "An affine map must compile");
				std::stringstream archive;
				plan->save(archive);
				const auto loaded = plan_type::load(archive);
				require(loaded.input_count() == input_count &&
					loaded.output_count() == output_count &&
					loaded.free_bit_count() == plan->free_bit_count(),
					"Persistence must retain affine dimensions and freedom");
				std::stringstream saved_again;
				loaded.save(saved_again);
				require(saved_again.str() == archive.str(),
					"Persistence must retain the already factored plan exactly");

				// This oracle evaluates the generated truth table directly;
				// it does not perform elimination or inspect plan internals.
				std::vector<std::set<unsigned>> expected(1U << output_count);
				for (unsigned input = 0; input < (1U << input_count); ++input)
				{
					unsigned output = 0;
					for (size_t row = 0; row < output_count; ++row)
					{
						const bool bit = (std::popcount(input & masks[row]) & 1) ^ constants[row];
						output |= static_cast<unsigned>(bit) << row;
					}
					expected[output].insert(input);
				}
				for (unsigned output = 0; output < (1U << output_count); ++output)
				{
					const auto target = unpack(output, output_count);
					std::set<unsigned> actual;
					for (unsigned free = 0; free < (1U << plan->free_bit_count()); ++free)
					{
						const auto parameters = unpack(free, plan->free_bit_count());
						const auto result = loaded.evaluate(target, parameters);
						require(result == plan->evaluate(target, parameters),
							"Loaded and original plans must return identical inputs");
						if (result)
							require(actual.insert(pack(*result)).second,
								"Independent free bits must select distinct solutions");
					}
					require(actual == expected[output],
						"Affine inverse must return exactly the brute-force solution set");
				}
			}
		}
	}
}

void word_boundaries_and_consistency()
{
	const auto inputs = unknown_bits(75);
	std::vector<br::bit_tracker> outputs;
	for (size_t row = 0; row < 70; ++row)
		outputs.push_back(row ?
			inputs[row] ^ inputs[row - 1] ^ br::bit_tracker(row % 3 == 0) : !inputs[row]);
	outputs.push_back(outputs[17] ^ outputs[66]);
	outputs.emplace_back(true);
	const auto plan = compile_plan(outputs, inputs);
	require(plan && plan->free_bit_count() == 5,
		"Word-spanning triangular map must retain exactly five unused inputs");
	std::vector<bool> expected(75), target(72), free(5);
	for (size_t bit = 0; bit < expected.size(); ++bit)
		expected[bit] = bit % 5 < 2;
	for (size_t row = 0; row < 70; ++row)
		target[row] = row ? expected[row] ^ expected[row - 1] ^ (row % 3 == 0) : !expected[0];
	target[70] = target[17] ^ target[66];
	target[71] = true;
	for (size_t bit = 0; bit < free.size(); ++bit)
		free[bit] = expected[bit + 70];
	require(plan->evaluate(target, free) == expected,
		"Inverse must handle multiple coefficient and transform words");

	std::stringstream archive;
	archive << std::hex << std::showbase;
	plan->save(archive);
	archive << "FOLLOWING_SECTION";
	const auto loaded = plan_type::load(archive);
	std::string following;
	archive >> following;
	require(following == "FOLLOWING_SECTION",
		"Loading an affine block must leave subsequent archive sections intact");
	require(loaded.evaluate(target, free) == expected,
		"Serialization must not depend on stream integer formatting");

	target[70] = !target[70];
	require(!loaded.evaluate(target, free),
		"A target violating a dependent output equation must be rejected");
	target[70] = !target[70];
	target[71] = false;
	require(!loaded.evaluate(target, free),
		"A target violating a constant output must be rejected");
	require_throws<std::invalid_argument>([&] { loaded.evaluate({}, free); },
		"Wrong target dimensions must throw invalid_argument");
	require_throws<std::invalid_argument>([&] { loaded.evaluate(target, {}); },
		"Wrong free-bit dimensions must throw invalid_argument");
}

void constants_and_parameter_order()
{
	const auto inputs = unknown_bits(3);
	const auto constant = compile_plan({br::bit_tracker(false), br::bit_tracker(true)}, inputs);
	require(constant && constant->free_bit_count() == 3,
		"All declared inputs of a constant map must remain free");
	require(constant->evaluate({false, true}, {true, false, true}) ==
		std::vector<bool>({true, false, true}),
		"Unused inputs must be supplied through independent parameters");
	require(!constant->evaluate({false, false}, {true, false, true}),
		"An impossible constant target must have no preimage");
	const auto no_inputs = compile_plan({br::bit_tracker(true)}, {});
	require(no_inputs && no_inputs->evaluate({true}, {}) == std::vector<bool>{} &&
		!no_inputs->evaluate({false}, {}),
		"A constant circuit with no variables must handle both possible and impossible targets");
	const auto reordered = compile_plan({inputs[0] ^ inputs[1]}, {inputs[2], inputs[1], inputs[0]});
	require(reordered && reordered->evaluate({false}, {true, false}) ==
		std::vector<bool>({true, false, false}),
		"Results and free parameters must respect the declared variable order");
}

br::bit_tracker raw_gate(char operation, const br::bit_tracker& lhs, const br::bit_tracker& rhs = {})
{
	// Bypass construction-time simplification to exercise affine detection.
	auto node = dixelu::make_counted<br::details::bitstate>();
	node->operation = operation;
	node->_1 = lhs.bit_state;
	if (operation != '!')
		node->_2 = rhs.bit_state;
	return br::bit_tracker(std::move(node));
}

void affine_detection_and_deep_graph()
{
	const auto inputs = unknown_bits(2);
	const auto a = raw_gate('^', inputs[0], inputs[1]);
	const auto b = raw_gate('^', inputs[1], inputs[0]);
	const auto outputs = std::vector<br::bit_tracker>{
		raw_gate('&', a, b),
		raw_gate('|', a, raw_gate('!', b)),
		raw_gate('&', a, br::bit_tracker(false)),
		raw_gate('|', a, br::bit_tracker(false))};
	const auto plan = compile_plan(outputs, inputs);
	require(plan && plan->evaluate({true, true, false, true}, {false}) ==
		std::vector<bool>({true, false}),
		"Affine identities must be recognized from coefficients, not node identity");
	require(!compile_plan({inputs[0] & inputs[1]}, inputs),
		"A nonlinear conjunction must not be presented as a direct affine inverse");
	require(!compile_plan({inputs[0] | inputs[1]}, inputs),
		"A nonlinear disjunction must not be presented as a direct affine inverse");

	auto deep = inputs[0];
	for (size_t depth = 0; depth < 20000; ++depth)
		deep = raw_gate('!', deep);
	const auto deep_plan = compile_plan({deep}, {inputs[0]});
	require(deep_plan && deep_plan->evaluate({true}, {}) == std::vector<bool>({true}),
		"Compiling deeply nested affine graphs must use iterative traversal");
}

void malformed_archives()
{
	const auto inputs = unknown_bits(2);
	const auto plan = compile_plan({inputs[0] ^ inputs[1], !inputs[0]}, inputs);
	require(plan.has_value(), "Archive test map must compile");
	std::stringstream archive;
	plan->save(archive);
	const std::string good = archive.str();
	const size_t marker_end = good.find("END_AFFINE") + std::string("END_AFFINE").size();
	for (size_t length = 0; length < marker_end; ++length)
	{
		std::stringstream truncated(good.substr(0, length));
		require_throws<std::runtime_error>([&] { plan_type::load(truncated); },
			"Every truncated affine archive must be rejected");
	}
	const std::vector<std::string> malformed{
		"NOT_AN_AFFINE_PLAN 1",
		"BITREVERSE_AFFINE 2",
		"BITREVERSE_AFFINE 1\n-1 0 0",
		"BITREVERSE_AFFINE 1\n0x1 0 0",
		"BITREVERSE_AFFINE 1\n99999999999999999999999999",
		"BITREVERSE_AFFINE 1\n1000000 1000000 0",
		"BITREVERSE_AFFINE 1\n1 1 2",
		"BITREVERSE_AFFINE 1\n1 1 1\n1\n0\n1\n1\nEND_AFFINE",
		"BITREVERSE_AFFINE 1\n2 2 2\n0 0\n0 0\n1 2\n1 2\nEND_AFFINE",
		"BITREVERSE_AFFINE 1\n1 1 1\n0\n2\n1\n1\nEND_AFFINE",
		"BITREVERSE_AFFINE 1\n1 1 1\n0\n0\n2\n1\nEND_AFFINE",
		"BITREVERSE_AFFINE 1\n1 1 1\n0\n0\n1\n0\nEND_AFFINE",
		"BITREVERSE_AFFINE 1\n1 1 1\n0\n0\n1\n2\nEND_AFFINE",
		"BITREVERSE_AFFINE 1\n1 1 1\n0\n0\n1\n1\nEND_AFFINEX"};
	for (const auto& text : malformed)
	{
		std::stringstream stream(text);
		require_throws<std::runtime_error>([&] { plan_type::load(stream); },
			"Malformed or oversized affine archives must be rejected");
	}
}

} // namespace

int main()
{
	exhaustive_affine_maps();
	word_boundaries_and_consistency();
	constants_and_parameter_order();
	affine_detection_and_deep_graph();
	malformed_archives();
	std::cout << "All affine inverse tests passed\n";
}
