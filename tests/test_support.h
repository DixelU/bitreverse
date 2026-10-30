#ifndef BITREVERSE_TESTS_TEST_SUPPORT_H
#define BITREVERSE_TESTS_TEST_SUPPORT_H

// The harness shared by the C++ tests: checks that report file:line, a case
// runner, bit conversions, and a DAG evaluator that serves as an oracle
// independent of the solver and of tracked arithmetic.

#include <array>
#include <cstddef>
#include <cstdint>
#include <exception>
#include <initializer_list>
#include <iostream>
#include <random>
#include <source_location>
#include <stdexcept>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <vector>

#include "tracker.h"
#include "solver/options.h"

namespace test
{
namespace br = dixelu::bitreverse;

// A failed check. require_throws never mistakes it for the expected exception.
class failure : public std::runtime_error
{
public:
	using std::runtime_error::runtime_error;
};

[[noreturn]] inline void fail(std::string_view message,
	std::source_location where = std::source_location::current())
{
	std::string_view file = where.file_name();
	file.remove_prefix(file.find_last_of("/\\") + 1);
	throw failure(std::string(file) + ":" + std::to_string(where.line()) + ": " + std::string(message));
}

inline void require(bool condition, std::string_view message,
	std::source_location where = std::source_location::current())
{
	if (!condition)
		fail(message, where);
}

// `function` must throw Exception with `what` somewhere in its what().
template<class Exception = std::exception, class Function>
void require_throws_with(Function&& function, std::string_view what, std::string_view message,
	std::source_location where = std::source_location::current())
{
	try
	{
		function();
	}
	catch (const failure&)
	{
		throw;
	}
	catch (const Exception& error)
	{
		if (std::string_view(error.what()).contains(what))
			return;
	}
	fail(message, where);
}

template<class Exception = std::exception, class Function>
void require_throws(Function&& function, std::string_view message,
	std::source_location where = std::source_location::current())
{
	require_throws_with<Exception>(std::forward<Function>(function), "", message, where);
}

struct test_case
{
	std::string_view name;
	void (*function)();
};

// Runs every case, even after a failure, and returns how many failed.
inline int run(std::string_view suite, std::initializer_list<test_case> cases)
{
	int failures = 0;
	for (const auto& [name, function] : cases)
	{
		try
		{
			function();
		}
		catch (const std::exception& error)
		{
			++failures;
			std::cerr << suite << '/' << name << ": " << error.what() << '\n';
		}
	}
	std::cout << suite << ": " << cases.size() - failures << " of " << cases.size() << " cases passed\n";
	return failures;
}

// Randomized tests seed this engine with fixed values, so every run checks
// the same cases.
using random_engine = std::mt19937;

// The search configurations. DPLL with affine propagation is the default;
// CDCL requires affine propagation to be off.
inline const br::solver_options dpll{.affine_reasoning = false};
inline const br::solver_options dpll_affine{};
inline const br::solver_options cdcl{.affine_reasoning = false, .conflict_learning = true};
inline const std::array<br::solver_options, 3> search_engines{dpll, dpll_affine, cdcl};

// Every three of these clauses are satisfiable, all four are not, and unit
// propagation alone cannot tell.
inline std::vector<br::bit_tracker> contradiction(const br::bit_tracker& x, const br::bit_tracker& y)
{
	return {x | y, ~x | y, x | ~y, ~x | ~y};
}

// Bit vectors here are least significant bit first, the order of
// inversion::bits_of, whereas int_tracker::bits is most significant first.
inline std::vector<bool> to_bits(std::uint64_t value, size_t width)
{
	std::vector<bool> bits(width);
	for (size_t bit = 0; bit < width; ++bit)
		bits[bit] = ((value >> bit) & 1U) != 0;
	return bits;
}

inline std::uint64_t from_bits(const std::vector<bool>& bits)
{
	require(bits.size() <= 64, "bit vector is wider than 64 bits");
	std::uint64_t value = 0;
	for (size_t bit = 0; bit < bits.size(); ++bit)
		value |= std::uint64_t{bits[bit]} << bit;
	return value;
}

// Whole bytes, each least significant bit first.
inline std::string text_of(const std::vector<bool>& bits)
{
	require(bits.size() % 8 == 0, "bit vector is not whole bytes");
	std::string text;
	for (size_t offset = 0; offset < bits.size(); offset += 8)
		text.push_back(static_cast<char>(from_bits({bits.begin() + offset, bits.begin() + offset + 8})));
	return text;
}

inline std::vector<br::itu8> tracked(std::string_view text)
{
	std::vector<br::itu8> bytes;
	for (const unsigned char byte : text)
		bytes.emplace_back(byte);
	return bytes;
}

// Distinct unknowns; copies of one br::unknown bit would all be the same leaf.
inline std::vector<br::bit_tracker> unknown_bits(size_t count)
{
	std::vector<br::bit_tracker> bits;
	for (size_t bit = 0; bit < count; ++bit)
		bits.emplace_back(br::unknown);
	return bits;
}

inline std::vector<br::itu8> unknown_bytes(size_t count)
{
	std::vector<br::itu8> bytes;
	for (size_t byte = 0; byte < count; ++byte)
		bytes.emplace_back(br::unknown);
	return bytes;
}

template<size_t N>
std::vector<br::bit_tracker> lsb_first(const br::int_tracker<N>& value)
{
	return {value.bits.rbegin(), value.bits.rend()};
}

inline std::vector<bool> constant_bits(const std::vector<br::bit_tracker>& bits)
{
	std::vector<bool> values;
	for (const auto& bit : bits)
	{
		require(bit.is_constant(), "expected a constant bit");
		values.push_back(bit.bit_state->state != 0);
	}
	return values;
}

template<size_t N>
std::uint64_t to_integer(const br::int_tracker<N>& value)
{
	static_assert(N <= 64);
	std::uint64_t result = 0;
	for (const auto& bit : value.bits)
	{
		require(bit.is_constant(), "expected a constant integer");
		result = (result << 1) | bit.bit_state->state;
	}
	return result;
}

// Truth tables over six inputs are 64-bit masks: bit a is the value when
// input i holds bit i of a. This is the truth table of input `bit`.
inline std::uint64_t input_mask(size_t bit)
{
	std::uint64_t mask = 0;
	for (size_t assignment = 0; assignment < 64; ++assignment)
		if ((assignment >> bit) & 1U)
			mask |= std::uint64_t{1} << assignment;
	return mask;
}

// Random gates over six unknowns (and, optionally, both constants), each
// expression with its truth table.
struct random_circuit
{
	std::vector<br::bit_tracker> inputs, expressions;
	std::vector<std::uint64_t> masks;

	random_circuit(random_engine& random, size_t gates, bool constants) :
		inputs(unknown_bits(6)), expressions(inputs)
	{
		for (size_t bit = 0; bit < inputs.size(); ++bit)
			masks.push_back(input_mask(bit));
		if (constants)
		{
			add(false, 0);
			add(true, ~std::uint64_t{0});
		}
		for (size_t gate = 0; gate < gates; ++gate)
		{
			const size_t a = random() % expressions.size(), b = random() % expressions.size();
			switch (random() % 4)
			{
			case 0: add(expressions[a] & expressions[b], masks[a] & masks[b]); break;
			case 1: add(expressions[a] | expressions[b], masks[a] | masks[b]); break;
			case 2: add(expressions[a] ^ expressions[b], masks[a] ^ masks[b]); break;
			default: add(~expressions[a], ~masks[a]); break;
			}
		}
	}

	void add(const br::bit_tracker& expression, std::uint64_t mask)
	{
		expressions.push_back(expression);
		masks.push_back(mask);
	}
};

// Evaluates a tracked DAG for chosen values of its unknown leaves. Nodes are
// numbered children first, so one pass in that order evaluates them all,
// however deep the DAG.
class evaluator
{
	using node = br::details::bitstate;

	struct gate
	{
		char operation;
		bool state;
		size_t lhs, rhs;
	};

	std::unordered_map<const node*, size_t> ids_;
	std::vector<gate> gates_;
	std::vector<signed char> values_;

	size_t id(const br::bit_tracker& bit) const
	{
		return ids_.at(bit.bit_state.get());
	}

public:
	// Inputs must be among the roots, or reachable from them.
	explicit evaluator(const std::vector<br::bit_tracker>& roots)
	{
		std::vector<std::pair<const node*, bool>> pending;
		for (const auto& root : roots)
			pending.emplace_back(root.bit_state.get(), false);
		while (!pending.empty())
		{
			const auto [current, children_numbered] = pending.back();
			if (ids_.contains(current))
			{
				pending.pop_back();
				continue;
			}
			const auto arity = br::details::operation_args_count[current->operation];
			if (arity && !children_numbered)
			{
				pending.back().second = true;
				pending.emplace_back(current->_1.get(), false);
				if (arity == 2)
					pending.emplace_back(current->_2.get(), false);
				continue;
			}
			pending.pop_back();
			ids_.emplace(current, gates_.size());
			gates_.push_back({static_cast<char>(current->operation), current->state != 0,
				arity ? ids_.at(current->_1.get()) : 0, arity == 2 ? ids_.at(current->_2.get()) : 0});
		}
		values_.assign(gates_.size(), -1);
	}

	// Forgets every input, so that evaluate() rejects an unassigned one.
	void reset()
	{
		values_.assign(gates_.size(), -1);
	}

	void assign(const br::bit_tracker& input, bool value)
	{
		const auto input_id = id(input);
		require(gates_[input_id].operation == '*', "only an unknown leaf can be assigned");
		values_[input_id] = value;
	}

	template<size_t N>
	void assign(const br::int_tracker<N>& input, std::uint64_t value)
	{
		static_assert(N <= 64);
		for (size_t bit = 0; bit < N; ++bit)
			assign(input.bits[N - 1 - bit], ((value >> bit) & 1U) != 0);
	}

	void evaluate()
	{
		for (size_t id = 0; id < gates_.size(); ++id)
		{
			const auto& [operation, state, lhs, rhs] = gates_[id];
			switch (operation)
			{
			case '*': require(values_[id] >= 0, "an unknown leaf has no assigned value"); break;
			case '=': values_[id] = state; break;
			case '!': values_[id] = !values_[lhs]; break;
			case '&': values_[id] = values_[lhs] & values_[rhs]; break;
			case '|': values_[id] = values_[lhs] | values_[rhs]; break;
			case '^': values_[id] = values_[lhs] ^ values_[rhs]; break;
			default: fail("unexpected operation in an evaluated DAG");
			}
		}
	}

	bool bit(const br::bit_tracker& output) const
	{
		return values_[id(output)] != 0;
	}

	template<size_t N>
	std::uint64_t word(const br::int_tracker<N>& output) const
	{
		static_assert(N <= 64);
		std::uint64_t value = 0;
		for (size_t bit = 0; bit < N; ++bit)
			value |= std::uint64_t{this->bit(output.bits[N - 1 - bit])} << bit;
		return value;
	}
};

} // namespace test

#endif // BITREVERSE_TESTS_TEST_SUPPORT_H
