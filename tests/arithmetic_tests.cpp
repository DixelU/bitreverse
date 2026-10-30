// Tracked Boolean simplification and integer arithmetic. Symbolic results are
// checked by evaluating the generated gates against native arithmetic, so no
// tracked arithmetic or solver code takes part in checking them.

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>
#include <initializer_list>
#include <stdexcept>
#include <utility>
#include <vector>

#include "circuit_metrics.h"
#include "test_support.h"
#include "tracker.h"

namespace br = dixelu::bitreverse;
using test::require;

namespace
{

template<size_t N>
void append(std::vector<br::bit_tracker>& roots, const br::int_tracker<N>& word)
{
	roots.insert(roots.end(), word.bits.begin(), word.bits.end());
}

constexpr bool constexpr_boolean_operations()
{
	const br::bit_tracker no(false), yes(true);
	return ((no | yes) & !no).is_constant(true);
}

static_assert(constexpr_boolean_operations());

void expression_simplification()
{
	require(br::bit_tracker(false).bit_state == br::bit_tracker(false).bit_state &&
		br::bit_tracker(true).bit_state == br::bit_tracker(true).bit_state &&
		br::bit_tracker(false).bit_state != br::bit_tracker(true).bit_state,
		"boolean constants must use the shared flyweight nodes");

	const br::bit_tracker x = br::unknown, y = br::unknown;
	const auto same_as_x = [&](const br::bit_tracker& bit) { return bit.bit_state == x.bit_state; };
	require(same_as_x(x & x), "x & x must simplify to x");
	require(same_as_x(x | x), "x | x must simplify to x");
	require((x ^ x).is_constant(false), "x ^ x must simplify to false");
	require((x & !x).is_constant(false), "x & !x must simplify to false");
	require((x | !x).is_constant(true), "x | !x must simplify to true");
	require((x ^ !x).is_constant(true), "x ^ !x must simplify to true");
	require(same_as_x(!!x), "!!x must simplify to x");
	require(same_as_x(x & (x | y)), "x & (x | y) must simplify to x");
	require(same_as_x((y | x) & x), "(y | x) & x must simplify to x");
	require(same_as_x(x | (x & y)), "x | (x & y) must simplify to x");
	require(same_as_x((y & x) | x), "(y & x) | x must simplify to x");

	// CRC-style mask construction: negating an integer whose only symbolic bit
	// is its lowest replicates that bit across the whole word.
	for (const auto& bit : (-br::int_tracker<8>(x)).bits)
		require(same_as_x(bit), "-int_tracker(bit) must replicate bit");
}

// Operations on constants take a separate code path from symbolic ones.
void concrete_arithmetic()
{
	using u8 = br::int_tracker<8>;
	for (std::uint64_t a = 0; a < 256; ++a)
	{
		for (std::uint64_t b = 1; b < 256; ++b)
			require(test::to_integer(u8{a} / u8{b}) == a / b && test::to_integer(u8{a} % u8{b}) == a % b,
				"broken division");
		for (const std::uint64_t b : {0, 1, 2, 3, 7, 8, 15, 16, 31, 32, 63, 64, 127, 128, 129, 254, 255})
		{
			require(test::to_integer(u8{a} * u8{b}) == ((a * b) & 255), "broken multiplication");
			u8 difference{a};
			br::bit_tracker no_underflow = false;
			difference.self_sub_ret_carry(u8{b}, no_underflow);
			require(test::to_integer(difference) == ((a - b) & 255), "broken direct subtraction");
			require(no_underflow.is_constant(a >= b), "subtraction carry must indicate no underflow");
		}
	}

	using u32 = br::int_tracker<32>;
	test::random_engine random(0x6d2b79f5);
	for (size_t sample = 0; sample < 512; ++sample)
	{
		const std::uint64_t lhs = random();
		const std::uint64_t rhs = std::max<std::uint64_t>(random(), 1);
		require(test::to_integer(u32{lhs} * u32{rhs}) == ((lhs * rhs) & 0xffffffffU), "broken multiplication");
		require(test::to_integer(u32{lhs} / u32{rhs}) == lhs / rhs, "broken division");
		require(test::to_integer(u32{lhs} % u32{rhs}) == lhs % rhs, "broken remainder");
	}
}

template<size_t N>
void exhaustive_symbolic()
{
	using word = br::int_tracker<N>;
	const word x = br::unknown, y = br::unknown;
	const br::bit_tracker input_carry = br::unknown;
	auto carry = input_carry;
	auto sum = x;
	sum.self_add_ret_carry(y, carry);
	auto difference = x;
	auto no_underflow = input_carry; // The incoming value must be ignored.
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

	std::vector<br::bit_tracker> roots{input_carry, carry, no_underflow};
	for (const auto& output : {x, y, sum, difference, product, quotient, remainder,
		doubled, squared, cancelled, self_quotient, self_remainder})
		append(roots, output);
	test::evaluator eval(roots);
	constexpr std::uint64_t count = std::uint64_t{1} << N, mask = count - 1;
	for (std::uint64_t a = 0; a < count; ++a)
		for (std::uint64_t b = 0; b < count; ++b)
			for (const unsigned c : {0U, 1U})
			{
				eval.reset();
				eval.assign(x, a);
				eval.assign(y, b);
				eval.assign(input_carry, c != 0);
				eval.evaluate();
				require(eval.word(sum) == ((a + b + c) & mask), "symbolic sum failed");
				require(eval.bit(carry) == (a + b + c >= count), "symbolic carry failed");
				require(eval.word(difference) == ((a - b) & mask), "symbolic subtraction failed");
				require(eval.bit(no_underflow) == (a >= b), "subtraction carry contract changed");
				require(eval.word(product) == ((a * b) & mask), "symbolic product failed");
				require(eval.word(quotient) == (b ? a / b : mask), "symbolic quotient failed");
				require(eval.word(remainder) == (b ? a % b : a), "symbolic remainder failed");
				require(eval.word(doubled) == ((2 * a) & mask), "aliased addition failed");
				require(eval.word(squared) == ((a * a) & mask), "aliased multiplication failed");
				require(eval.word(cancelled) == 0, "aliased subtraction failed");
				require(eval.word(self_quotient) == (a ? 1 : mask) && eval.word(self_remainder) == 0,
					"division with repeated operands failed");
			}
}

void exhaustive_constant_division()
{
	using word = br::int_tracker<8>;
	const word x = br::unknown;
	for (std::uint64_t constant = 0; constant < 256; ++constant)
	{
		const word fixed{constant};
		word right_remainder, left_remainder;
		const auto right_quotient = x.divmod(fixed, right_remainder);
		const auto left_quotient = fixed.divmod(x, left_remainder);
		std::vector<br::bit_tracker> roots;
		for (const auto& output : {x, right_quotient, right_remainder, left_quotient, left_remainder})
			append(roots, output);
		test::evaluator eval(roots);
		for (std::uint64_t input = 0; input < 256; ++input)
		{
			eval.reset();
			eval.assign(x, input);
			eval.evaluate();
			require(eval.word(right_quotient) == (constant ? input / constant : 255), "constant divisor quotient failed");
			require(eval.word(right_remainder) == (constant ? input % constant : input), "constant divisor remainder failed");
			require(eval.word(left_quotient) == (input ? constant / input : 255), "constant dividend quotient failed");
			require(eval.word(left_remainder) == (input ? constant % input : constant), "constant dividend remainder failed");
		}
	}
}

// Words wider than 64 bits, least significant bit first, with reference
// arithmetic on single bits.
template<size_t N>
using wide = std::array<bool, N>;

template<size_t N>
wide<N> wide_value(std::uint64_t value)
{
	wide<N> result{};
	for (size_t i = 0; i < N && i < 64; ++i)
		result[i] = ((value >> i) & 1U) != 0;
	return result;
}

template<size_t N>
br::int_tracker<N> tracked(const wide<N>& value)
{
	br::int_tracker<N> result;
	for (size_t i = 0; i < N; ++i)
		result.bits[N - 1 - i] = value[i];
	return result;
}

template<size_t N>
wide<N> read(const test::evaluator& eval, const br::int_tracker<N>& word)
{
	wide<N> result{};
	for (size_t i = 0; i < N; ++i)
		result[i] = eval.bit(word.bits[N - 1 - i]);
	return result;
}

template<size_t N>
wide<N> reference_add(wide<N> a, const wide<N>& b, bool& carry)
{
	for (size_t i = 0; i < N; ++i)
	{
		const unsigned total = unsigned{a[i]} + b[i] + carry;
		a[i] = total & 1;
		carry = total > 1;
	}
	return a;
}

template<size_t N>
wide<N> reference_product(const wide<N>& a, const wide<N>& b)
{
	wide<N> result{};
	for (size_t shift = 0; shift < N; ++shift)
		if (b[shift])
		{
			unsigned carry = 0;
			for (size_t i = shift; i < N; ++i)
			{
				const unsigned sum = unsigned{result[i]} + a[i - shift] + carry;
				result[i] = sum & 1;
				carry = sum >> 1;
			}
		}
	return result;
}

template<size_t N>
std::pair<wide<N>, wide<N>> reference_divmod(const wide<N>& a, const wide<N>& b)
{
	wide<N> quotient{}, remainder{};
	wide<N + 1> partial{};
	for (size_t bit = N; bit-- > 0;)
	{
		for (size_t i = N; i; --i)
			partial[i] = partial[i - 1];
		partial[0] = a[bit];
		bool greater_equal = true;
		if (!partial[N])
			for (size_t i = N; i-- > 0;)
				if (partial[i] != b[i])
				{
					greater_equal = partial[i];
					break;
				}
		if (greater_equal)
		{
			bool borrow = false;
			for (size_t i = 0; i <= N; ++i)
			{
				const int difference = int{partial[i]} - (i < N ? b[i] : 0) - borrow;
				partial[i] = difference & 1;
				borrow = difference < 0;
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
	wide<N> large{};
	large[N - 1] = large[0] = large[2] = true;
	word large_remainder;
	const auto large_quotient = x.divmod(tracked(large), large_remainder);
	const auto small_remainder = x % word{10};
	wide<N> power{};
	power[N - 1] = true;
	word power_remainder;
	const auto power_quotient = x.divmod(tracked(power), power_remainder);

	std::vector<br::bit_tracker> roots{carry, no_underflow};
	for (const auto& output : {x, y, sum, difference, product, quotient, remainder,
		large_quotient, large_remainder, small_remainder, power_quotient, power_remainder})
		append(roots, output);
	test::evaluator eval(roots);
	test::random_engine random(0x6a09e667);
	for (size_t sample = 0; sample < 64; ++sample)
	{
		wide<N> a{}, b{};
		for (size_t i = 0; i < N; ++i)
		{
			const auto bits = random();
			a[i] = bits & 1;
			b[i] = bits & 2;
		}
		if (sample < 4)
			b = wide_value<N>(sample);
		if (sample == 4)
		{
			a.fill(true);
			b.fill(true);
		}
		if (sample == 5)
		{
			a = {};
			b = power;
		}
		eval.reset();
		for (size_t i = 0; i < N; ++i)
		{
			eval.assign(x.bits[N - 1 - i], a[i]);
			eval.assign(y.bits[N - 1 - i], b[i]);
		}
		eval.evaluate();

		bool expected_carry = true;
		require(read(eval, sum) == reference_add(a, b, expected_carry), "wide sum failed");
		require(eval.bit(carry) == expected_carry, "wide addition carry failed");
		auto complement = b;
		for (auto& bit : complement)
			bit = !bit;
		expected_carry = true;
		require(read(eval, difference) == reference_add(a, complement, expected_carry), "wide subtraction failed");
		require(eval.bit(no_underflow) == expected_carry, "wide subtraction carry failed");
		require(read(eval, product) == reference_product(a, b), "wide multiplication failed");
		const auto [q, r] = reference_divmod(a, b);
		require(read(eval, quotient) == q && read(eval, remainder) == r, "wide division failed");
		const auto [large_q, large_r] = reference_divmod(a, large);
		require(read(eval, large_quotient) == large_q && read(eval, large_remainder) == large_r,
			"wide constant divisor was truncated");
		require(read(eval, small_remainder) == reference_divmod(a, wide_value<N>(10)).second,
			"wide remainder by ten failed");
		const auto [power_q, power_r] = reference_divmod(a, power);
		require(read(eval, power_quotient) == power_q && read(eval, power_remainder) == power_r,
			"wide power of two divisor failed");
	}
}

template<size_t N>
constexpr bool constant_equals(const br::int_tracker<N>& actual, unsigned expected)
{
	for (size_t bit = 0; bit < N; ++bit)
		if (!actual.bits[N - 1 - bit].is_constant(((expected >> bit) & 1U) != 0))
			return false;
	return true;
}

constexpr bool constexpr_arithmetic()
{
	using word = br::int_tracker<5>;
	const word a{29}, b{11};
	auto sum = a;
	br::bit_tracker carry = true;
	sum.self_add_ret_carry(b, carry);
	word remainder;
	const auto quotient = a.divmod(b, remainder);
	return constant_equals(sum, 9) && carry.is_constant(true) && constant_equals(a - b, 18) &&
		constant_equals(a * b, 31) && constant_equals(quotient, 2) && constant_equals(remainder, 7) &&
		constant_equals(a / b, 2) && constant_equals(a % b, 7);
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
			(!shifted.bits[i].is_constant() || !expected_shift.bits[i].is_constant() ||
			 shifted.bits[i].bit_state->state != expected_shift.bits[i].bit_state->state))
			return false;
	for (size_t i = 0; i < 4; ++i)
		if (doubled.bits[i].bit_state != x.bits[i + 1].bit_state)
			return false;
	return carry.bit_state == x.bits[0].bit_state && doubled.bits.back().is_constant(false) &&
		bool(remainder.bits.back().bit_state);
}

static_assert(constexpr_symbolic_arithmetic());

constexpr bool constexpr_prepared_division()
{
	using word = br::int_tracker<5>;
	const auto by_three = br::prepare_divisor<5>(3);
	word remainder;
	const auto quotient = by_three.divmod(word{13}, remainder);
	return constant_equals(quotient, 4) && constant_equals(remainder, 1) &&
		constant_equals(word{13} / by_three, 4) && constant_equals(word{13} % by_three, 1);
}

static_assert(constexpr_prepared_division());

void prepared_constant_division()
{
	using word = br::int_tracker<8>;
	const word x = br::unknown;
	for (const std::uint64_t constant : {0U, 1U, 8U, 10U, 255U})
	{
		const auto divisor = br::prepare_divisor(word{constant});
		word combined_remainder;
		const auto combined_quotient = divisor.divmod(x, combined_remainder);
		const auto quotients = {combined_quotient, divisor.divide(x), x / divisor};
		const auto remainders = {combined_remainder, divisor.modulo(x), x % divisor};
		std::vector<br::bit_tracker> roots;
		append(roots, x);
		for (const auto& output : quotients)
			append(roots, output);
		for (const auto& output : remainders)
			append(roots, output);
		test::evaluator eval(roots);
		for (std::uint64_t input = 0; input < 256; ++input)
		{
			eval.reset();
			eval.assign(x, input);
			eval.evaluate();
			for (const auto& quotient : quotients)
				require(eval.word(quotient) == (constant ? input / constant : 255), "prepared quotient failed");
			for (const auto& remainder : remainders)
				require(eval.word(remainder) == (constant ? input % constant : input), "prepared remainder failed");
		}
	}
	test::require_throws<std::invalid_argument>([&] { (void)br::prepare_divisor(x); },
		"prepared divisor accepted a symbolic value");
}

void division_aliases()
{
	using word = br::int_tracker<8>;
	for (const std::uint64_t divisor : {0U, 1U, 8U, 10U, 255U})
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
	require(br::measure_circuit(x / br::itu64{8}).gates == 0 && br::measure_circuit(x % br::itu64{8}).gates == 0,
		"power-of-two division must be wiring only");
	const auto modulo = br::measure_circuit(x % br::itu64{10});
	require(modulo.gates <= 1600 && modulo.max_depth <= 560, "constant remainder was not narrowed");
	const auto prepared_modulo = br::measure_circuit(x % br::prepare_divisor<64>(10));
	require(prepared_modulo.gates == modulo.gates && prepared_modulo.max_depth == modulo.max_depth,
		"prepared divisor changed the circuit shape");
	const auto right = br::measure_circuit(x * br::itu64{13});
	const auto left = br::measure_circuit(br::itu64{13} * x);
	require(right.gates == left.gates && right.max_depth == left.max_depth,
		"constant multiplication depends on operand order");
}

} // namespace

int main()
{
	return test::run("arithmetic", {
		{"expression_simplification", expression_simplification},
		{"concrete_arithmetic", concrete_arithmetic},
		{"exhaustive_symbolic<1>", exhaustive_symbolic<1>},
		{"exhaustive_symbolic<3>", exhaustive_symbolic<3>},
		{"exhaustive_symbolic<5>", exhaustive_symbolic<5>},
		{"exhaustive_symbolic<8>", exhaustive_symbolic<8>},
		{"exhaustive_constant_division", exhaustive_constant_division},
		{"wide_arithmetic<64>", wide_arithmetic<64>},
		{"wide_arithmetic<65>", wide_arithmetic<65>},
		{"wide_arithmetic<129>", wide_arithmetic<129>},
		{"prepared_constant_division", prepared_constant_division},
		{"division_aliases", division_aliases},
		{"circuit_shape", circuit_shape},
	});
}
