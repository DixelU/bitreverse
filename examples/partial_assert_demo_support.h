#ifndef DIXELU_BITREVERSE_PARTIAL_ASSERT_DEMO_SUPPORT_H
#define DIXELU_BITREVERSE_PARTIAL_ASSERT_DEMO_SUPPORT_H

// Shared driver of the partial_assert examples. Each example hashes a message
// whose leading bytes are unknown and also observes one masked internal value,
// the checkpoint. Eager mode asserts the checkpoint inside the algorithm and
// simplifies its live state; deferred mode asserts it after the algorithm ends.
//
// An Example type provides:
//   name, unknown_bytes
//   native_checkpoint(bytes) -> std::uint32_t   masked internal value
//   native_digest(bytes)                         native reference digest
//   tracked_digest(native digest) -> int_tracker
//   hash(std::vector<itu8>, checkpoint&) -> int_tracker, calling checkpoint.reach()

#include "circuit_metrics.h"
#include "partial_assert.h"
#include "timing.h"

#include <cstdint>
#include <iomanip>
#include <iostream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

namespace partial_assert_demo
{
namespace br = dixelu::bitreverse;
using bytes = std::vector<std::uint8_t>;

inline void require(bool condition, const std::string& message)
{
	if (!condition) throw std::runtime_error(message);
}

struct checkpoint
{
	br::partial_assert_context& context;
	bool eager;
	std::uint32_t observed;
	br::bit_tracker equality{};
	size_t known_after{};

	// `value` is the masked internal value, compared with `observed`. Eager
	// mode asserts that at once and replaces each listed state variable by its
	// simplified form.
	template<class... State>
	void reach(const br::int_tracker<32>& value, State&... state)
	{
		equality = br::int_tracker<32>::are_equal(value, br::int_tracker<32>(observed));
		if (!eager) return;
		assert_observation();
		((state = context.simplify(state)), ...);
	}

	void assert_observation()
	{
		context.partial_assert(equality);
		known_after = context.known_variable_count();
	}
};

struct result
{
	size_t digest_nodes{}, retained_nodes{}, known_after_checkpoint{}, assertions{}, solutions{};
	size_t solver_decisions{}, solver_steps{};
	double build_and_assert_ms{}, final_solve_ms{};
	bytes recovered;
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

inline std::uint8_t decoded_byte(const br::itu8& input, const br::collision_resolution::solution& model)
{
	std::uint8_t byte = 0;
	for (const auto& bit : input.bits)
	{
		const bool value = bit.is_constant() ? bit.bit_state->state != 0 : model.assignments.at(bit.bit_state);
		byte = static_cast<std::uint8_t>((byte << 1) | value);
	}
	return byte;
}

// Checked natively, without the context: the checkpoint must single out the
// known message among all values of its unknown bytes.
template<class Example>
void require_unique_checkpoint(const bytes& known, std::uint32_t observed)
{
	size_t matches = 0;
	for (size_t value = 0; value < (size_t{1} << (8 * Example::unknown_bytes)); ++value)
	{
		auto candidate = known;
		for (size_t index = 0; index < Example::unknown_bytes; ++index)
			candidate[index] = static_cast<std::uint8_t>(value >> (8 * index));
		matches += Example::native_checkpoint(candidate) == observed;
	}
	require(matches == 1, std::string(Example::name) + " masked checkpoint is not unique in this example");
}

template<class Example>
result run(bool eager, const bytes& known, std::uint32_t observed)
{
	const std::string name(Example::name);
	const auto expected = Example::native_digest(known);
	result result;
	const auto started = support::clock::now();
	br::partial_assert_context context;
	std::vector<br::itu8> message;
	for (size_t index = 0; index < known.size(); ++index)
		message.push_back(index < Example::unknown_bytes ? br::itu8(br::unknown) : br::itu8(known[index]));
	std::vector<br::bit_tracker> inputs;
	for (size_t index = 0; index < Example::unknown_bytes; ++index)
		inputs.insert(inputs.end(), message[index].bits.begin(), message[index].bits.end());
	checkpoint point{context, eager, observed};
	const auto digest = Example::hash(message, point);
	if (!eager) point.assert_observation();
	result.known_after_checkpoint = point.known_after;
	context.partial_assert(digest, Example::tracked_digest(expected));
	result.build_and_assert_ms = support::milliseconds_since(started);
	result.digest_nodes = br::measure_circuit(digest).nodes;
	auto retained = context.requirements();
	retained.insert(retained.end(), inputs.begin(), inputs.end());
	retained.insert(retained.end(), digest.bits.begin(), digest.bits.end());
	result.retained_nodes = br::measure_circuit(retained).nodes;
	result.assertions = context.assertion_count();

	br::solver_options options;
	options.max_search_steps = 2000000;
	br::solver_statistics statistics;
	const auto solve_started = support::clock::now();
	result.solutions = context.solve(inputs, [&](const br::collision_resolution::solution& model)
	{
		auto recovered = known;
		for (size_t index = 0; index < Example::unknown_bytes; ++index)
			recovered[index] = decoded_byte(message[index], model);
		require(Example::native_checkpoint(recovered) == observed,
			name + " recovered input violates the native checkpoint");
		require(Example::native_digest(recovered) == expected,
			name + " recovered input fails independent native verification");
		require(recovered == known, name + " recovered a different input despite unique checkpoint");
		result.recovered = std::move(recovered);
		return true;
	}, 2, options, &statistics);
	result.final_solve_ms = support::milliseconds_since(solve_started);
	result.solver_decisions = statistics.decisions;
	result.solver_steps = statistics.search_steps;
	require(result.solutions == 1, name + " constraints did not have exactly one solution");
	return result;
}

// Both modes receive the same two constraints; eager simplification must
// determine every unknown bit at the checkpoint, shrink the digest
// expressions and recover the same message.
template<class Example>
void compare_modes(const bytes& known, std::uint32_t observed)
{
	std::cout << "Both modes receive exactly these two constraints. The checkpoint is extra information, not derived from the digest.\n";
	const auto eager = run<Example>(true, known, observed);
	const auto deferred = run<Example>(false, known, observed);
	print("eager inside algorithm", eager);
	print("deferred until algorithm ends", deferred);
	const std::string name(Example::name);
	const size_t unknown_bits = Example::unknown_bytes * 8;
	require(eager.known_after_checkpoint == unknown_bits,
		name + " checkpoint did not determine all " + std::to_string(unknown_bits) + " unknown bits");
	require(eager.digest_nodes < deferred.digest_nodes, name + " eager simplification did not shrink final expressions");
	require(eager.recovered == deferred.recovered, name + " comparison recovered different inputs");
}

} // namespace partial_assert_demo

#endif // DIXELU_BITREVERSE_PARTIAL_ASSERT_DEMO_SUPPORT_H
