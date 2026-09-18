#include <array>
#include <cstdint>
#include <iostream>
#include <stdexcept>
#include <vector>

#include "circuit_metrics.h"

namespace br = dixelu::bitreverse;
namespace core = br::collision_resolution::solver_core;

void require(bool condition, const char* message)
{
	if (!condition) throw std::runtime_error(message);
}

template<size_t N>
using value = std::array<bool, N>; // Independent reference values, LSB first.

template<size_t N>
value<N> native_value(std::uint64_t input)
{
	value<N> result{};
	for (size_t i = 0; i < N; ++i, input >>= 1) result[i] = input & 1;
	return result;
}

template<size_t N>
constexpr br::int_tracker<N> tracked(const value<N>& input)
{
	br::int_tracker<N> result;
	for (size_t i = 0; i < N; ++i) result.bits[N - 1 - i] = input[i];
	return result;
}

template<size_t N>
void append(std::vector<br::bit_tracker>& outputs, const br::int_tracker<N>& word)
{
	outputs.insert(outputs.end(), word.bits.begin(), word.bits.end());
}

// Evaluate only Boolean gates, in dependency order. No tracked arithmetic,
// solver, or recursive traversal is used to check the generated circuits.
class evaluator
{
	core::compiled_circuit graph;
	std::vector<size_t> order;
	std::vector<signed char> values;

	static std::vector<dixelu::counted_ptr<br::details::bitstate>> roots(
		const std::vector<br::bit_tracker>& outputs)
	{
		std::vector<dixelu::counted_ptr<br::details::bitstate>> result;
		for (const auto& bit : outputs) result.push_back(bit.bit_state);
		return result;
	}

public:
	explicit evaluator(const std::vector<br::bit_tracker>& outputs) :
		graph(roots(outputs)), values(graph.nodes.size())
	{
		std::vector<size_t> remaining(graph.nodes.size());
		for (size_t i = 0; i < graph.nodes.size(); ++i)
		{
			remaining[i] = br::details::operation_args_count[graph.nodes[i]->operation];
			if (!remaining[i]) order.push_back(i);
		}
		for (size_t i = 0; i < order.size(); ++i)
			for (size_t parent : graph.parents[order[i]])
				if (!--remaining[parent]) order.push_back(parent);
		require(order.size() == graph.nodes.size(), "arithmetic created a cycle");
	}

	void reset() { std::fill(values.begin(), values.end(), -1); }

	void assign(const br::bit_tracker& bit, bool input)
	{
		values[graph.node_ids.at(bit.bit_state.get())] = input;
	}

	template<size_t N>
	void assign(const br::int_tracker<N>& word, const value<N>& input)
	{
		for (size_t i = 0; i < N; ++i) assign(word.bits[N - 1 - i], input[i]);
	}

	void run()
	{
		for (size_t id : order)
		{
			const auto [a, b] = graph.inputs[id];
			switch (graph.nodes[id]->operation)
			{
			case '*': require(values[id] >= 0, "unbound arithmetic input"); break;
			case '=': values[id] = graph.nodes[id]->state; break;
			case '!': values[id] = !values[a]; break;
			case '^': values[id] = values[a] != values[b]; break;
			case '&': values[id] = values[a] & values[b]; break;
			case '|': values[id] = values[a] | values[b]; break;
			default: throw std::runtime_error("invalid arithmetic gate");
			}
		}
	}

	bool bit(const br::bit_tracker& output) const
	{
		return values[graph.node_ids.at(output.bit_state.get())] != 0;
	}

	template<size_t N>
	value<N> word(const br::int_tracker<N>& output) const
	{
		value<N> result{};
		for (size_t i = 0; i < N; ++i) result[i] = bit(output.bits[N - 1 - i]);
		return result;
	}
};

template<size_t N>
void exhaustive_symbolic()
{
	using word = br::int_tracker<N>;
	const word x = br::unknown, y = br::unknown;
	br::bit_tracker input_carry;
	input_carry = br::unknown;
	auto carry = input_carry;
	auto sum = x;
	sum.self_add_ret_carry(y, carry);
	auto difference = x;
	auto no_underflow = input_carry; // Incoming value must be ignored.
	difference.self_sub_ret_carry(y, no_underflow);
	const auto product = x * y;
	word remainder;
	const auto quotient = x.divmod(y, remainder);
	word self_remainder;
	const auto self_quotient = x.divmod(x, self_remainder);
	auto doubled = x, squared = x, cancelled = x;
	doubled += doubled;
	squared *= squared;
	cancelled -= cancelled;
	std::vector<br::bit_tracker> outputs{input_carry, carry, no_underflow};
	for (const auto& output : {x, y, sum, difference, product, quotient, remainder,
		doubled, squared, cancelled, self_quotient, self_remainder}) append(outputs, output);
	evaluator eval(outputs);
	constexpr unsigned count = 1u << N, mask = count - 1;
	for (unsigned a = 0; a < count; ++a)
		for (unsigned b = 0; b < count; ++b)
			for (unsigned c = 0; c < 2; ++c)
			{
				eval.reset();
				eval.assign(x, native_value<N>(a));
				eval.assign(y, native_value<N>(b));
				eval.assign(input_carry, c);
				eval.run();
				require(eval.word(sum) == native_value<N>((a + b + c) & mask), "symbolic sum failed");
				require(eval.bit(carry) == (a + b + c >= count), "symbolic carry failed");
				require(eval.word(difference) == native_value<N>((a - b) & mask), "symbolic subtraction failed");
				require(eval.bit(no_underflow) == (a >= b), "subtraction carry contract changed");
				require(eval.word(product) == native_value<N>((a * b) & mask), "symbolic product failed");
				require(eval.word(quotient) == native_value<N>(b ? a / b : mask), "symbolic quotient failed");
				require(eval.word(remainder) == native_value<N>(b ? a % b : a), "symbolic remainder failed");
				require(eval.word(doubled) == native_value<N>((2 * a) & mask), "aliased addition failed");
				require(eval.word(squared) == native_value<N>((a * a) & mask), "aliased multiplication failed");
				require(eval.word(cancelled) == value<N>{}, "aliased subtraction failed");
				require(eval.word(self_quotient) == native_value<N>(a ? 1 : mask) &&
					eval.word(self_remainder) == value<N>{}, "division with repeated operands failed");
			}
}

void exhaustive_constant_division()
{
	using word = br::int_tracker<8>;
	const word x = br::unknown;
	for (unsigned constant = 0; constant < 256; ++constant)
	{
		const word fixed{constant};
		word right_remainder, left_remainder;
		const auto right_quotient = x.divmod(fixed, right_remainder);
		const auto left_quotient = fixed.divmod(x, left_remainder);
		std::vector<br::bit_tracker> outputs;
		for (const auto& output : {x, right_quotient, right_remainder, left_quotient, left_remainder})
			append(outputs, output);
		evaluator eval(outputs);
		for (unsigned input = 0; input < 256; ++input)
		{
			eval.reset(); eval.assign(x, native_value<8>(input)); eval.run();
			require(eval.word(right_quotient) == native_value<8>(constant ? input / constant : 255),
				"constant divisor quotient failed");
			require(eval.word(right_remainder) == native_value<8>(constant ? input % constant : input),
				"constant divisor remainder failed");
			require(eval.word(left_quotient) == native_value<8>(input ? constant / input : 255),
				"constant dividend quotient failed");
			require(eval.word(left_remainder) == native_value<8>(input ? constant % input : constant),
				"constant dividend remainder failed");
		}
	}
}

template<size_t N>
value<N> reference_add(value<N> a, const value<N>& b, bool& carry)
{
	for (size_t i = 0; i < N; ++i)
	{
		const unsigned total = unsigned(a[i]) + b[i] + carry;
		a[i] = total & 1;
		carry = total > 1;
	}
	return a;
}

template<size_t N>
value<N> reference_product(const value<N>& a, const value<N>& b)
{
	value<N> result{};
	for (size_t shift = 0; shift < N; ++shift)
		if (b[shift])
		{
			unsigned carry = 0;
			for (size_t i = shift; i < N; ++i)
			{
				const unsigned sum = unsigned(result[i]) + a[i - shift] + carry;
				result[i] = sum & 1; carry = sum >> 1;
			}
		}
	return result;
}

template<size_t N>
std::pair<value<N>, value<N>> reference_divmod(const value<N>& a, const value<N>& b)
{
	value<N> quotient{}, remainder{};
	value<N + 1> partial{};
	for (size_t bit = N; bit-- > 0;)
	{
		for (size_t i = N; i; --i) partial[i] = partial[i - 1];
		partial[0] = a[bit];
		bool greater_equal = true;
		if (!partial[N])
			for (size_t i = N; i-- > 0;)
				if (partial[i] != b[i]) { greater_equal = partial[i]; break; }
		if (greater_equal)
		{
			bool borrow = false;
			for (size_t i = 0; i <= N; ++i)
			{
				const int difference = int(partial[i]) - (i < N ? b[i] : 0) - borrow;
				partial[i] = difference & 1; borrow = difference < 0;
			}
			quotient[bit] = true;
		}
	}
	std::copy_n(partial.begin(), N, remainder.begin());
	return {quotient, remainder};
}

template<size_t N>
void wide_arithmetic()
{
	using word = br::int_tracker<N>;
	const word x = br::unknown, y = br::unknown;
	auto sum = x, difference = x;
	br::bit_tracker carry = true, no_underflow = false;
	sum.self_add_ret_carry(y, carry);
	difference.self_sub_ret_carry(y, no_underflow);
	const auto product = x * y;
	word remainder;
	const auto quotient = x.divmod(y, remainder);
	value<N> large{}; large[N - 1] = true; large[0] = true; large[2] = true;
	const auto large_divisor = tracked(large);
	word large_remainder;
	const auto large_quotient = x.divmod(large_divisor, large_remainder);
	const auto small_remainder = x % word{10};
	value<N> power{}; power[N - 1] = true;
	word power_remainder;
	const auto power_quotient = x.divmod(tracked(power), power_remainder);
	std::vector<br::bit_tracker> outputs{carry, no_underflow};
	for (const auto& output : {x, y, sum, difference, product, quotient, remainder,
		large_quotient, large_remainder, small_remainder, power_quotient, power_remainder}) append(outputs, output);
	evaluator eval(outputs);
	std::uint64_t random = 0x6a09e667f3bcc909ULL;
	for (size_t sample = 0; sample < 64; ++sample)
	{
		value<N> a{}, b{};
		for (size_t i = 0; i < N; ++i)
		{
			random ^= random << 13; random ^= random >> 7; random ^= random << 17;
			a[i] = random & 1; b[i] = random & 2;
		}
		if (sample < 4) b = native_value<N>(sample);
		if (sample == 4) { a.fill(true); b.fill(true); }
		if (sample == 5) { a = {}; b = power; }
		eval.reset(); eval.assign(x, a); eval.assign(y, b); eval.run();
		bool expected_carry = true;
		require(eval.word(sum) == reference_add(a, b, expected_carry), "wide sum failed");
		require(eval.bit(carry) == expected_carry, "wide addition carry failed");
		auto complement = b;
		for (auto& bit : complement) bit = !bit;
		expected_carry = true;
		require(eval.word(difference) == reference_add(a, complement, expected_carry), "wide subtraction failed");
		require(eval.bit(no_underflow) == expected_carry, "wide subtraction carry failed");
		require(eval.word(product) == reference_product(a, b), "wide multiplication failed");
		const auto [q, r] = reference_divmod(a, b);
		require(eval.word(quotient) == q && eval.word(remainder) == r, "wide division failed");
		const auto [cq, cr] = reference_divmod(a, large);
		require(eval.word(large_quotient) == cq && eval.word(large_remainder) == cr,
			"wide constant divisor was truncated");
		require(eval.word(small_remainder) == reference_divmod(a, native_value<N>(10)).second,
			"wide remainder by ten failed");
		const auto [pq, pr] = reference_divmod(a, power);
		require(eval.word(power_quotient) == pq && eval.word(power_remainder) == pr,
			"wide power of two divisor failed");
	}
}

constexpr bool constexpr_arithmetic()
{
	using word = br::int_tracker<5>;
	const word a{29}, b{11};
	auto sum = a;
	br::bit_tracker carry = true;
	sum.self_add_ret_carry(b, carry);
	const auto product = a * b;
	word remainder;
	const auto quotient = a.divmod(b, remainder);
	const auto difference = a - b;
	const std::array<word, 5> actual{sum, difference, product, quotient, remainder};
	const std::array<unsigned, 5> expected{9, 18, 31, 2, 7};
	for (size_t i = 0; i < actual.size(); ++i)
		for (size_t bit = 0; bit < 5; ++bit)
			if (actual[i].bits[4 - bit].bit_state->operation != '=' ||
				actual[i].bits[4 - bit].bit_state->state != ((expected[i] >> bit) & 1)) return false;
	return carry.bit_state->operation == '=' && carry.bit_state->state;
}

static_assert(constexpr_arithmetic());

constexpr bool constexpr_symbolic_arithmetic()
{
	using word = br::int_tracker<5>;
	const word x = br::unknown;
	auto doubled = x;
	br::bit_tracker carry = false;
	doubled.self_add_ret_carry(x, carry);
	const auto shifted = x * word{8};
	const auto expected_shift = x << 3;
	const auto remainder = x % word{10}; // Fixed-capacity scratch is constexpr too.
	for (size_t i = 0; i < 5; ++i)
		if (shifted.bits[i].bit_state != expected_shift.bits[i].bit_state &&
			(shifted.bits[i].bit_state->operation != '=' ||
			 expected_shift.bits[i].bit_state->operation != '=' ||
			 shifted.bits[i].bit_state->state != expected_shift.bits[i].bit_state->state)) return false;
	for (size_t i = 0; i < 4; ++i)
		if (doubled.bits[i].bit_state != x.bits[i + 1].bit_state) return false;
	return carry.bit_state == x.bits[0].bit_state &&
		doubled.bits.back().bit_state->operation == '=' &&
		!doubled.bits.back().bit_state->state && bool(remainder.bits.back().bit_state);
}

static_assert(constexpr_symbolic_arithmetic());

constexpr bool constexpr_prepared_division()
{
	using word = br::int_tracker<5>;
	const auto by_three = br::prepare_divisor<5>(3);
	word remainder;
	const auto quotient = by_three.divmod(word{13}, remainder);
	const auto operator_quotient = word{13} / by_three;
	const auto operator_remainder = word{13} % by_three;
	const auto matches = [](const word& actual, unsigned expected)
	{
		for (size_t bit = 0; bit < 5; ++bit)
			if (actual.bits[4 - bit].bit_state->operation != '=' ||
				actual.bits[4 - bit].bit_state->state != ((expected >> bit) & 1))
				return false;
		return true;
	};
	return matches(quotient, 4) && matches(remainder, 1) &&
		matches(operator_quotient, 4) && matches(operator_remainder, 1);
}

static_assert(constexpr_prepared_division());

void prepared_constant_division()
{
	using word = br::int_tracker<8>;
	const word x = br::unknown;
	for (const unsigned constant : {0u, 1u, 8u, 10u, 255u})
	{
		const auto divisor = br::prepare_divisor(word{constant});
		word combined_remainder;
		const auto combined_quotient = divisor.divmod(x, combined_remainder);
		const auto method_quotient = divisor.divide(x);
		const auto method_remainder = divisor.modulo(x);
		const auto operator_quotient = x / divisor;
		const auto operator_remainder = x % divisor;

		std::vector<br::bit_tracker> outputs;
		for (const auto& output : {x, combined_quotient, combined_remainder,
			method_quotient, method_remainder, operator_quotient, operator_remainder})
			append(outputs, output);
		evaluator eval(outputs);
		for (unsigned input = 0; input < 256; ++input)
		{
			eval.reset();
			eval.assign(x, native_value<8>(input));
			eval.run();
			const auto expected_quotient = native_value<8>(constant ? input / constant : 255);
			const auto expected_remainder = native_value<8>(constant ? input % constant : input);
			for (const auto& quotient : {combined_quotient, method_quotient, operator_quotient})
				require(eval.word(quotient) == expected_quotient,
					"prepared quotient failed");
			for (const auto& remainder : {combined_remainder, method_remainder, operator_remainder})
				require(eval.word(remainder) == expected_remainder,
					"prepared remainder failed");
		}
	}

	bool rejected = false;
	try
	{
		const auto invalid = br::prepare_divisor(x);
		(void)invalid;
	}
	catch (const std::invalid_argument&)
	{
		rejected = true;
	}
	require(rejected, "prepared divisor accepted a symbolic value");
}

void division_aliases()
{
	using word = br::int_tracker<8>;
	for (unsigned divisor : {0u, 1u, 8u, 10u, 255u})
	{
		word dividend{237}, divisor_word{divisor};
		const auto q1 = dividend.divmod(divisor_word, dividend);
		const auto q2 = word{237}.divmod(divisor_word, divisor_word);
		const auto expected_q = word{divisor ? 237 / divisor : 255};
		const auto expected_r = word{divisor ? 237 % divisor : 237};
		for (size_t i = 0; i < 8; ++i)
		{
			require(q1.bits[i].bit_state == expected_q.bits[i].bit_state &&
				q2.bits[i].bit_state == expected_q.bits[i].bit_state, "aliased quotient failed");
			require(dividend.bits[i].bit_state == expected_r.bits[i].bit_state &&
				divisor_word.bits[i].bit_state == expected_r.bits[i].bit_state, "aliased remainder failed");
		}
	}
}

void circuit_shape()
{
	const br::itu64 x = br::unknown, y = br::unknown;
	const auto add = br::measure_circuit(x + y);
	const auto multiply = br::measure_circuit(x * y);
	const auto divide = br::measure_circuit(x / y);
	require(add.max_depth <= 24 && add.gates <= 500, "prefix addition shape regressed");
	require(multiply.max_depth <= 50 && multiply.gates <= 13000, "carry-save multiplication shape regressed");
	require(divide.max_depth <= 1200 && divide.gates <= 36000, "prefix division shape regressed");
	require(br::measure_circuit(x / br::itu64{8}).gates == 0 &&
		br::measure_circuit(x % br::itu64{8}).gates == 0, "power-of-two division must be wiring only");
	const auto modulo = br::measure_circuit(x % br::itu64{10});
	require(modulo.gates <= 1600 && modulo.max_depth <= 560, "constant remainder was not narrowed");
	const auto by_ten = br::prepare_divisor<64>(10);
	const auto prepared_modulo = br::measure_circuit(x % by_ten);
	require(prepared_modulo.gates == modulo.gates &&
		prepared_modulo.max_depth == modulo.max_depth,
		"prepared divisor changed the circuit shape");
	const auto right = br::measure_circuit(x * br::itu64{13});
	const auto left = br::measure_circuit(br::itu64{13} * x);
	require(right.gates == left.gates && right.max_depth == left.max_depth,
		"constant multiplication depends on operand order");
}

int main()
{
	exhaustive_symbolic<1>();
	exhaustive_symbolic<3>();
	exhaustive_symbolic<5>();
	exhaustive_symbolic<8>();
	exhaustive_constant_division();
	wide_arithmetic<64>();
	wide_arithmetic<65>();
	wide_arithmetic<129>();
	prepared_constant_division();
	division_aliases();
	circuit_shape();
	std::cout << "All arithmetic tests passed\n";
}
