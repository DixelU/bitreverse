#ifndef DIXELU_BITREVERSE_TRACKER_H
#define DIXELU_BITREVERSE_TRACKER_H

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>
#include <span>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

#include "counted_ptr.h"

namespace dixelu::bitreverse
{
namespace details
{

// One node of a tracked Boolean DAG. `operation` is one of:
//   '=' constant (value in `state`), '*' unknown input leaf,
//   '!' negation of _1, and the binary gates '&', '|', '^' over _1 and _2.
struct bitstate
{
	counted_ptr<bitstate> _1{};
	counted_ptr<bitstate> _2{};

	size_t max_depth : 56 {0};
	size_t state : 1 {0};
	size_t operation : 7 {'='};
};

constexpr std::array<std::uint8_t, 256> get_operation_args_count()
{
	std::array<std::uint8_t, 256> a{};
	a['^'] = a['|'] = a['&'] = 2;
	a['!'] = 1;
	return a;
}

constexpr auto operation_args_count = get_operation_args_count();

constexpr counted_ptr<bitstate> make_fresh_boolean_constant(bool value)
{
	auto state = make_counted<bitstate>();
	state->state = value;
	state->operation = '=';
	return state;
}

inline const counted_ptr<bitstate>& runtime_boolean_constant(bool value)
{
	static const counted_ptr<bitstate> false_state =
		make_fresh_boolean_constant(false);
	static const counted_ptr<bitstate> true_state =
		make_fresh_boolean_constant(true);
	return value ? true_state : false_state;
}

// Runtime constants are shared flyweights; constant evaluation cannot use
// statics, so it allocates a fresh node instead.
constexpr counted_ptr<bitstate> make_boolean_constant(const bool value)
{
	if consteval
	{
		return make_fresh_boolean_constant(value);
	}

	return runtime_boolean_constant(value);
}

constexpr counted_ptr<bitstate> make_bitstate_operation(
	std::uint8_t operation,
	const counted_ptr<bitstate>& lhs = {},
	const counted_ptr<bitstate>& rhs = {});

constexpr bool is_negation_of(
	const counted_ptr<bitstate>& expression,
	const counted_ptr<bitstate>& candidate)
{
	return expression &&
		expression->operation == '!' &&
		expression->_1 == candidate;
}

constexpr bool are_complements(
	const counted_ptr<bitstate>& lhs,
	const counted_ptr<bitstate>& rhs)
{
	return is_negation_of(lhs, rhs) || is_negation_of(rhs, lhs);
}

constexpr bool contains_operand(
	const counted_ptr<bitstate>& expression,
	std::uint8_t operation,
	const counted_ptr<bitstate>& operand)
{
	return expression &&
		expression->operation == operation &&
		(expression->_1 == operand || expression->_2 == operand);
}

// Local Boolean identities applied while a gate is constructed. Returns the
// simplified node, or nothing when no identity applies.
constexpr counted_ptr<bitstate> simplify_operation(
	std::uint8_t operation,
	const counted_ptr<bitstate>& lhs,
	const counted_ptr<bitstate>& rhs)
{
	const auto is_constant = [](const counted_ptr<bitstate>& node, bool value)
	{
		return node->operation == '=' && node->state == value;
	};

	switch (operation)
	{
		case '|':
			// Constant, idempotence, complement, and absorption identities.
			if (is_constant(lhs, true) || is_constant(rhs, false))
				return lhs;
			if (is_constant(lhs, false) || is_constant(rhs, true))
				return rhs;
			if (lhs == rhs)
				return lhs;
			if (are_complements(lhs, rhs))
				return make_boolean_constant(true);
			if (contains_operand(rhs, '&', lhs))
				return lhs;
			if (contains_operand(lhs, '&', rhs))
				return rhs;
			break;
		case '&':
			// Constant, idempotence, complement, and absorption identities.
			if (is_constant(lhs, false) || is_constant(rhs, true))
				return lhs;
			if (is_constant(lhs, true) || is_constant(rhs, false))
				return rhs;
			if (lhs == rhs)
				return lhs;
			if (are_complements(lhs, rhs))
				return make_boolean_constant(false);
			if (contains_operand(rhs, '|', lhs))
				return lhs;
			if (contains_operand(lhs, '|', rhs))
				return rhs;
			break;
		case '^':
			// Constant, cancellation, and complement identities.
			if (is_constant(lhs, false))
				return rhs;
			if (is_constant(lhs, true))
				return make_bitstate_operation('!', rhs);
			if (is_constant(rhs, false))
				return lhs;
			if (is_constant(rhs, true))
				return make_bitstate_operation('!', lhs);
			if (lhs == rhs)
				return make_boolean_constant(false);
			if (are_complements(lhs, rhs))
				return make_boolean_constant(true);
			break;
		case '!':
			// Involution: NOT(NOT(x)) == x.
			if (lhs->operation == '!')
				return lhs->_1;
			break;
		default:
			break;
	}
	return {};
}

// Builds an unknown leaf ('*') or a gate ('!', '&', '|', '^'). Gates over
// constants are folded and local identities are applied, so the result may
// be an existing node. Constants come from make_boolean_constant.
constexpr counted_ptr<bitstate> make_bitstate_operation(
	const std::uint8_t operation,
	const counted_ptr<bitstate>& lhs,
	const counted_ptr<bitstate>& rhs)
{
	if (operation != '*' && operation != '!' && operation != '&' &&
		operation != '|' && operation != '^')
		throw std::logic_error("Unknown operand");

	if (operation != '*' && lhs->operation == '=' &&
		(operation == '!' || rhs->operation == '='))
	{
		switch (operation)
		{
			case '|': return make_boolean_constant(lhs->state | rhs->state);
			case '&': return make_boolean_constant(lhs->state & rhs->state);
			case '^': return make_boolean_constant(lhs->state ^ rhs->state);
			default: return make_boolean_constant(!lhs->state);
		}
	}

	if (operation != '*')
		if (auto simplified = simplify_operation(operation, lhs, rhs))
			return simplified;

	counted_ptr<bitstate> node = make_counted<bitstate>();
	node->operation = operation;
	node->_1 = lhs;
	node->_2 = rhs;
	node->max_depth = 1 + std::max(
		lhs ? lhs->max_depth : 0,
		rhs ? rhs->max_depth : 0);
	return node;
}

} // namespace details

struct unknown_t {};
inline constexpr unknown_t unknown;

// A single tracked bit: a handle to a node of the shared Boolean DAG.
struct bit_tracker
{
	counted_ptr<details::bitstate> bit_state;

	constexpr bit_tracker() : bit_tracker(false) {}

	constexpr bit_tracker(bool value) :
		bit_state(details::make_boolean_constant(value))
	{
	}

	constexpr bit_tracker(unknown_t) :
		bit_state(details::make_bitstate_operation('*'))
	{
	}

	explicit constexpr bit_tracker(counted_ptr<details::bitstate>&& state) :
		bit_state(std::move(state))
	{
	}

	[[nodiscard]] constexpr bool is_constant() const
	{
		return bit_state->operation == '=';
	}

	[[nodiscard]] constexpr bool is_constant(bool value) const
	{
		return is_constant() && bit_state->state == value;
	}

	constexpr bit_tracker& operator|=(const bit_tracker& rhs)
	{
		bit_state = details::make_bitstate_operation('|', bit_state, rhs.bit_state);
		return *this;
	}

	constexpr bit_tracker& operator&=(const bit_tracker& rhs)
	{
		bit_state = details::make_bitstate_operation('&', bit_state, rhs.bit_state);
		return *this;
	}

	constexpr bit_tracker& operator^=(const bit_tracker& rhs)
	{
		bit_state = details::make_bitstate_operation('^', bit_state, rhs.bit_state);
		return *this;
	}

	constexpr bit_tracker operator|(const bit_tracker& rhs) const
	{
		bit_tracker tracker = *this;
		tracker |= rhs;
		return tracker;
	}

	constexpr bit_tracker operator&(const bit_tracker& rhs) const
	{
		bit_tracker tracker = *this;
		tracker &= rhs;
		return tracker;
	}

	constexpr bit_tracker operator^(const bit_tracker& rhs) const
	{
		bit_tracker tracker = *this;
		tracker ^= rhs;
		return tracker;
	}

	constexpr bit_tracker operator~() const
	{
		return bit_tracker(details::make_bitstate_operation('!', bit_state));
	}

	constexpr bit_tracker operator!() const
	{
		return ~*this;
	}

	// '0'/'1' for constants, otherwise the operation character.
	[[nodiscard]] constexpr char to_char() const
	{
		if (is_constant())
			return static_cast<char>(bit_state->state + '0');

		return static_cast<char>(bit_state->operation);
	}
};

constexpr bit_tracker if_then_else(
	const bit_tracker& condition,
	const bit_tracker& then_value,
	const bit_tracker& else_value)
{
	return ((!condition) & else_value) | (condition & then_value);
}

template<size_t N>
struct int_tracker;

template<size_t N>
class prepared_divisor;

namespace details
{

// Brent-Kung scan of carry transforms, ordered least significant bit first.
// (G2, P2) composed with (G1, P1) is (G2 | P2&G1, P2&P1).
// The up/down sweeps use linear work and logarithmic dependency depth, also
// for widths that are not powers of two.
template<typename Bits>
constexpr void prefix_carries(Bits& generate, Bits& propagate)
{
	const auto combine = [&](size_t upper, size_t lower)
	{
		generate[upper] |= propagate[upper] & generate[lower];
		propagate[upper] &= propagate[lower];
	};

	size_t stride = 1;
	for (; stride < generate.size(); stride *= 2)
		for (size_t i = 2 * stride - 1; i < generate.size(); i += 2 * stride)
			combine(i, i - stride);

	for (stride /= 4; stride; stride /= 2)
		for (size_t i = 3 * stride - 1; i < generate.size(); i += 2 * stride)
			combine(i, i - stride);
}

// Integer bit storage is MSB first; the carry scratch is LSB first.
template<typename LhsBits, typename RhsBits>
constexpr bool try_add_concrete_bits_with_carry(
	LhsBits& lhs,
	const RhsBits& rhs,
	bit_tracker& carry)
{
	const size_t count = lhs.size();
	if (!count)
		return true;

	bool concrete = carry.is_constant();
	for (size_t i = 0; concrete && i < count; ++i)
		concrete = lhs[i].is_constant() && rhs[i].is_constant();

	if (!concrete)
		return false;

	bool next = carry.bit_state->state;
	for (size_t i = count; i-- > 0;)
	{
		const unsigned sum = lhs[i].bit_state->state + rhs[i].bit_state->state + next;
		lhs[i] = static_cast<bool>(sum & 1);
		next = sum > 1;
	}
	carry = next;
	return true;
}

template<typename LhsBits, typename RhsBits, typename ScratchBits>
constexpr void add_symbolic_bits_with_carry(
	LhsBits& lhs,
	const RhsBits& rhs,
	bit_tracker& carry,
	ScratchBits& propagate,
	ScratchBits& generate,
	ScratchBits& sum_propagate)
{
	const size_t count = lhs.size();
	for (size_t i = 0; i < count; ++i)
	{
		propagate[i] = lhs[count - 1 - i] ^ rhs[count - 1 - i];
		generate[i] = lhs[count - 1 - i] & rhs[count - 1 - i];
		sum_propagate[i] = propagate[i];
	}

	const auto carry_in = carry;
	generate[0] |= propagate[0] & carry_in;
	propagate[0] = false; // The first group already includes the input carry.

	prefix_carries(generate, propagate);
	for (size_t i = 0; i < count; ++i)
		lhs[count - 1 - i] = sum_propagate[i] ^ (i ? generate[i - 1] : carry_in);

	carry = generate[count - 1];
}

// lhs += rhs + carry; carry receives the carry out. The scratch buffers let
// narrow division views reuse fixed-capacity stack storage.
template<typename LhsBits, typename RhsBits, typename ScratchBits>
constexpr void add_bits_with_carry(
	LhsBits& lhs,
	const RhsBits& rhs,
	bit_tracker& carry,
	ScratchBits& propagate,
	ScratchBits& generate,
	ScratchBits& sum_propagate)
{
	if (!try_add_concrete_bits_with_carry(lhs, rhs, carry))
		add_symbolic_bits_with_carry(lhs, rhs, carry, propagate, generate, sum_propagate);
}

template<size_t N>
constexpr void add_bits_with_carry(
	std::array<bit_tracker, N>& lhs,
	const std::array<bit_tracker, N>& rhs,
	bit_tracker& carry)
{
	if (try_add_concrete_bits_with_carry(lhs, rhs, carry))
		return;

	std::array<bit_tracker, N> propagate, generate, sum_propagate;
	add_symbolic_bits_with_carry(lhs, rhs, carry, propagate, generate, sum_propagate);
}

enum class divisor_kind
{
	symbolic,
	zero,
	power_of_two,
	constant
};

template<size_t N>
struct divisor_profile
{
	divisor_kind classification = divisor_kind::zero;
	size_t width = 0;
	std::array<bit_tracker, N + 1> extended;

	constexpr explicit divisor_profile(const std::array<bit_tracker, N>& bits)
	{
		size_t ones = 0;
		for (size_t i = 0; i < N; ++i)
		{
			if (!bits[i].is_constant())
			{
				classification = divisor_kind::symbolic;
				width = N;
				for (size_t bit = 0; bit < N; ++bit)
					extended[bit + 1] = bits[bit];
				return;
			}
			if (bits[i].bit_state->state)
			{
				if (!width)
					width = N - i;
				++ones;
			}
		}

		classification = !width
			? divisor_kind::zero
			: ones == 1
				? divisor_kind::power_of_two
				: divisor_kind::constant;
		for (size_t i = 0; i < width; ++i)
			extended[i + 1] = bits[N - width + i];
	}

	[[nodiscard]] constexpr bool is_constant() const
	{
		return classification != divisor_kind::symbolic;
	}
};

} // namespace details

template<size_t N>
class prepared_divisor
{
	details::divisor_profile<N> profile_;

	friend struct int_tracker<N>;

public:
	explicit constexpr prepared_divisor(const int_tracker<N>& divisor);

	[[nodiscard]] constexpr int_tracker<N> divide(const int_tracker<N>& dividend) const;
	[[nodiscard]] constexpr int_tracker<N> modulo(const int_tracker<N>& dividend) const;
	[[nodiscard]] constexpr int_tracker<N> divmod(
		const int_tracker<N>& dividend,
		int_tracker<N>& remainder) const;
};

// An N-bit unsigned integer of tracked bits. `bits` is most significant bit
// first. Arithmetic wraps modulo 2^N.
template <size_t N>
struct int_tracker
{
	using self_type = int_tracker<N>;
	std::array<bit_tracker, N> bits;

	constexpr int_tracker() = default;

	constexpr int_tracker(unknown_t)
	{
		for (auto& el : bits)
			el = unknown;
	}

	constexpr int_tracker(std::uintmax_t value)
	{
		for (size_t i = 0; i < N && value; ++i, value >>= 1)
			bits[N - i - 1] = static_cast<bool>(value & 1);
	}

	// Zero-extends or truncates, keeping the low bits.
	template <size_t Q>
	constexpr int_tracker(const int_tracker<Q>& rhs)
	{
		auto rhs_rit = rhs.bits.crbegin();
		auto this_rit = bits.rbegin();

		for (; rhs_rit != rhs.bits.crend() && this_rit != bits.rend(); ++rhs_rit, ++this_rit)
			*this_rit = *rhs_rit;
	}

	constexpr int_tracker(bit_tracker bit)
	{
		bits.back() = std::move(bit);
	}

	explicit constexpr int_tracker(std::array<bit_tracker, N>&& bits) :
		bits(std::move(bits))
	{
	}

	constexpr int_tracker(self_type&&) = default;
	constexpr int_tracker(const self_type&) = default;
	constexpr self_type& operator=(const self_type&) = default;
	constexpr self_type& operator=(self_type&&) = default;

	constexpr self_type& operator|=(const self_type& rhs)
	{
		for (size_t i = 0; i < N; ++i)
			bits[i] |= rhs.bits[i];
		return *this;
	}

	constexpr self_type& operator&=(const self_type& rhs)
	{
		for (size_t i = 0; i < N; ++i)
			bits[i] &= rhs.bits[i];
		return *this;
	}

	constexpr self_type& operator^=(const self_type& rhs)
	{
		for (size_t i = 0; i < N; ++i)
			bits[i] ^= rhs.bits[i];
		return *this;
	}

	constexpr self_type operator|(const self_type& rhs) const
	{
		self_type copy = *this;
		copy |= rhs;
		return copy;
	}

	constexpr self_type operator&(const self_type& rhs) const
	{
		self_type copy = *this;
		copy &= rhs;
		return copy;
	}

	constexpr self_type operator^(const self_type& rhs) const
	{
		self_type copy = *this;
		copy ^= rhs;
		return copy;
	}

	constexpr self_type operator~() const
	{
		self_type value = *this;
		for (size_t i = 0; i < N; ++i)
			value.bits[i] = !value.bits[i];
		return value;
	}

	// True when any bit is set.
	explicit constexpr operator bit_tracker() const
	{
		bit_tracker result;
		for (size_t i = 0; i < N; ++i)
			result |= bits[i];
		return result;
	}

	constexpr bit_tracker operator!() const
	{
		return !static_cast<bit_tracker>(*this);
	}

	constexpr self_type operator<<(size_t shift) const
	{
		self_type value = *this;
		value <<= shift;
		return value;
	}

	constexpr self_type& operator<<=(size_t shift)
	{
		if (shift >= N)
		{
			*this = 0;
			return *this;
		}

		for (size_t i = 0; i < N - shift; ++i)
			bits[i] = std::move(bits[i + shift]);
		for (size_t i = N - shift; i < N; ++i)
			bits[i] = false;

		return *this;
	}

	constexpr self_type operator>>(size_t shift) const
	{
		self_type value = *this;
		value >>= shift;
		return value;
	}

	constexpr self_type& operator>>=(size_t shift)
	{
		if (shift >= N)
		{
			*this = 0;
			return *this;
		}

		for (size_t i = N; i-- > shift;)
			bits[i] = std::move(bits[i - shift]);
		for (size_t i = 0; i < shift; ++i)
			bits[i] = false;

		return *this;
	}

	constexpr self_type& operator>>=(const self_type& shift)
	{
		return barrel_shift(shift, false);
	}

	constexpr self_type& operator<<=(const self_type& shift)
	{
		return barrel_shift(shift, true);
	}

	constexpr self_type operator>>(const self_type& shift) const
	{
		self_type copy = *this;
		copy >>= shift;
		return copy;
	}

	constexpr self_type operator<<(const self_type& shift) const
	{
		self_type copy = *this;
		copy <<= shift;
		return copy;
	}

	// carry is both the carry in and, on return, the carry out.
	constexpr self_type& self_add_ret_carry(const self_type& rhs, bit_tracker& carry)
	{
		details::add_bits_with_carry(bits, rhs.bits, carry);
		return *this;
	}

	constexpr self_type& operator+=(const self_type& rhs)
	{
		bit_tracker carry = false;
		return self_add_ret_carry(rhs, carry);
	}

	constexpr self_type& self_sub_ret_carry(const self_type& rhs, bit_tracker& carry)
	{
		// carry is an output: true means the unsigned subtraction did not
		// underflow. Subtraction is lhs + ~rhs + 1 with the same prefix scan.
		const auto complement = ~rhs;
		carry = true;
		return self_add_ret_carry(complement, carry);
	}

	constexpr self_type& operator-=(const self_type& rhs)
	{
		bit_tracker carry = false;
		return self_sub_ret_carry(rhs, carry);
	}

	constexpr self_type operator+(const self_type& rhs) const
	{
		self_type lhs = *this;
		lhs += rhs;
		return lhs;
	}

	constexpr self_type operator-(const self_type& rhs) const
	{
		self_type lhs = *this;
		lhs -= rhs;
		return lhs;
	}

	constexpr self_type operator-() const
	{
		return ~*this + 1;
	}

	constexpr self_type& operator*=(const self_type& rhs)
	{
		const auto nonzero_bits = [](const self_type& value)
		{
			size_t count = 0;
			for (const auto& bit : value.bits)
				count += !bit.is_constant(false);
			return count;
		};

		const self_type* multiplicand = this;
		const self_type* multiplier = &rhs;
		if (nonzero_bits(*this) < nonzero_bits(rhs))
			std::swap(multiplicand, multiplier);

		const size_t row_count = nonzero_bits(*multiplier);
		std::vector<self_type> rows;
		rows.reserve(row_count);
		for (size_t shift = 0; shift < N; ++shift)
		{
			const auto& selector = multiplier->bits[N - 1 - shift];
			if (selector.is_constant(false))
				continue;
			self_type row;
			for (size_t i = 0; i < N - shift; ++i)
				row.bits[i] = multiplicand->bits[i + shift] & selector;
			rows.push_back(std::move(row));
		}

		// Three rows become two without propagating carries horizontally.
		// Overflow above bit N-1 is deliberately discarded (modulo 2^N).
		// Each group is fully consumed before its outputs overwrite earlier rows.
		while (rows.size() > 2)
		{
			size_t row = 0, write = 0;
			for (; row + 2 < rows.size(); row += 3)
			{
				self_type sum, carry;
				for (size_t i = 0; i < N; ++i)
				{
					const auto ab = rows[row].bits[i] ^ rows[row + 1].bits[i];
					sum.bits[i] = ab ^ rows[row + 2].bits[i];
					if (i)
						carry.bits[i - 1] = (rows[row].bits[i] & rows[row + 1].bits[i]) |
							(rows[row + 2].bits[i] & ab);
				}
				rows[write++] = std::move(sum);
				rows[write++] = std::move(carry);
			}
			for (; row < rows.size(); ++row)
				rows[write++] = std::move(rows[row]);
			rows.resize(write);
		}
		if (rows.empty()) return *this = self_type{};
		if (rows.size() == 2) rows[0] += rows[1];
		return *this = std::move(rows[0]);
	}

	// Division by zero yields an all-ones quotient and the dividend as the
	// remainder (total bit-vector semantics).
	constexpr void divide_profile(
		const details::divisor_profile<N>& divisor,
		self_type* quotient,
		self_type* remainder) const
	{
		if (divisor.classification == details::divisor_kind::zero)
		{
			if (quotient)
				for (auto& bit : quotient->bits)
					bit = true;
			if (remainder)
				*remainder = *this;
			return;
		}

		if (divisor.classification == details::divisor_kind::power_of_two)
		{
			const size_t shift = divisor.width - 1;
			if (quotient)
				*quotient = *this >> shift;
			if (remainder)
			{
				self_type low_bits;
				for (size_t i = N - shift; i < N; ++i)
					low_bits.bits[i] = bits[i];
				*remainder = std::move(low_bits);
			}
			return;
		}

		// Known leading zeroes need no arithmetic iterations. For an unknown
		// divisor the skipped quotient bits must still be one when D == 0.
		size_t first_bit = 0;
		while (first_bit < N && bits[first_bit].is_constant(false))
			++first_bit;

		if (first_bit && quotient && !divisor.is_constant())
		{
			auto nonzero = divisor.extended;
			for (size_t stride = 1; stride < N; stride *= 2)
			{
				for (size_t i = 1; i + stride <= N; i += 2 * stride)
					nonzero[i] |= nonzero[i + stride];
			}

			const auto is_zero = !nonzero[1];
			for (size_t i = 0; i < first_bit; ++i)
				quotient->bits[i] = is_zero;
		}

		// Non-restoring remainders lie in [-D, D). A fixed divisor therefore
		// needs only its significant bits plus a sign bit, at any tracker width.
		const size_t width = divisor.width;
		const size_t scratch_size = width + 1;
		std::array<bit_tracker, N + 1> partial_storage;
		std::array<bit_tracker, N + 1> operand_storage;
		std::array<bit_tracker, N + 1> propagate_storage;
		std::array<bit_tracker, N + 1> generate_storage;
		std::array<bit_tracker, N + 1> sum_propagate_storage;

		std::span<bit_tracker> partial_remainder(partial_storage.data(), scratch_size);
		std::span<const bit_tracker> extended_divisor(divisor.extended.data(), scratch_size);
		std::span<bit_tracker> operand(operand_storage.data(), scratch_size);
		std::span<bit_tracker> propagate(propagate_storage.data(), scratch_size);
		std::span<bit_tracker> generate(generate_storage.data(), scratch_size);
		std::span<bit_tracker> sum_propagate(sum_propagate_storage.data(), scratch_size);

		for (size_t dividend_bit = first_bit; dividend_bit < N; ++dividend_bit)
		{
			const auto was_negative = partial_remainder[0];
			for (size_t i = 0; i < width; ++i)
				partial_remainder[i] = std::move(partial_remainder[i + 1]);

			partial_remainder[width] = bits[dividend_bit];

			// A non-negative partial remainder subtracts the divisor; a
			// negative one adds it. XOR supplies either D or ~D, and the
			// initial carry supplies the +1 required for subtraction.
			const auto invert_divisor = !was_negative;
			auto carry = invert_divisor;
			if (divisor.is_constant())
			{
				// D xor !sign is either sign or !sign. Select it directly instead
				// of redispatching constant folding for every divisor bit.
				for (size_t i = 0; i < scratch_size; ++i)
					operand[i] = extended_divisor[i].bit_state->state
						? was_negative
						: invert_divisor;
			}
			else
			{
				for (size_t i = 0; i < scratch_size; ++i)
					operand[i] = extended_divisor[i] ^ invert_divisor;
			}

			details::add_bits_with_carry(
				partial_remainder, operand, carry, propagate, generate, sum_propagate);

			if (quotient)
				quotient->bits[dividend_bit] = !partial_remainder[0];
		}

		if (!remainder)
			return;

		self_type restored;
		const auto is_negative = partial_remainder[0];
		if (is_negative.is_constant())
		{
			if (is_negative.bit_state->state)
			{
				bit_tracker carry = false;
				details::add_bits_with_carry(
					partial_remainder, extended_divisor, carry,
					propagate, generate, sum_propagate);
			}

			for (size_t i = 0; i < width; ++i)
				restored.bits[N - width + i] = partial_remainder[i + 1];
		}
		else
		{
			for (size_t i = 0; i < scratch_size; ++i)
				operand[i] = partial_remainder[i];
			bit_tracker carry = false;
			details::add_bits_with_carry(
				operand, extended_divisor, carry, propagate, generate, sum_propagate);
			for (size_t i = 0; i < width; ++i)
			{
				const auto& uncorrected = partial_remainder[i + 1];
				restored.bits[N - width + i] = uncorrected ^
					(is_negative & (operand[i + 1] ^ uncorrected));
			}
		}

		*remainder = std::move(restored);
	}

	constexpr self_type divide(const self_type& divisor) const
	{
		self_type quotient;
		divide_profile(details::divisor_profile<N>(divisor.bits), &quotient, nullptr);
		return quotient;
	}

	constexpr self_type modulo(const self_type& divisor) const
	{
		self_type remainder;
		divide_profile(details::divisor_profile<N>(divisor.bits), nullptr, &remainder);
		return remainder;
	}

	constexpr self_type divmod(const self_type& divisor, self_type& remainder) const
	{
		self_type quotient;
		divide_profile(details::divisor_profile<N>(divisor.bits), &quotient, &remainder);
		return quotient;
	}

	constexpr self_type divide(const prepared_divisor<N>& divisor) const
	{
		self_type quotient;
		divide_profile(divisor.profile_, &quotient, nullptr);
		return quotient;
	}

	constexpr self_type modulo(const prepared_divisor<N>& divisor) const
	{
		self_type remainder;
		divide_profile(divisor.profile_, nullptr, &remainder);
		return remainder;
	}

	constexpr self_type divmod(
		const prepared_divisor<N>& divisor,
		self_type& remainder) const
	{
		self_type quotient;
		divide_profile(divisor.profile_, &quotient, &remainder);
		return quotient;
	}

	friend constexpr self_type operator*(self_type lhs, const self_type& rhs)
	{
		return lhs *= rhs;
	}

	friend constexpr self_type operator/(const self_type& lhs, const self_type& rhs)
	{
		return lhs.divide(rhs);
	}

	friend constexpr self_type operator%(const self_type& lhs, const self_type& rhs)
	{
		return lhs.modulo(rhs);
	}

	// One character per bit, most significant first: see bit_tracker::to_char.
	[[nodiscard]] std::string to_string() const
	{
		std::string str;
		str.reserve(N);
		for (auto& bit : bits)
			str.push_back(bit.to_char());
		return str;
	}

	[[nodiscard]] size_t max_depth() const
	{
		size_t max_depth = 0;
		for (auto& bit : bits)
			max_depth = std::max<size_t>(max_depth, bit.bit_state->max_depth);
		return max_depth;
	}

	[[nodiscard]] static bit_tracker are_equal(const self_type& lhs, const self_type& rhs)
	{
		bit_tracker res = true;
		for (size_t i = 0; i < N; ++i)
			res &= !(lhs.bits[i] ^ rhs.bits[i]);
		return res;
	}

private:
	// Stage i shifts by 2^i when bit i of `shift` is set. Shift bits at or
	// above log2(N) are ignored, unlike the size_t shifts, which saturate.
	constexpr self_type& barrel_shift(const self_type& shift, bool left)
	{
		self_type result = *this;
		for (size_t i = 0; i < N && (size_t{1} << i) < N; ++i)
		{
			const size_t amount = size_t{1} << i;
			result = if_then_else(
				shift.bits[N - i - 1],
				left ? result << amount : result >> amount,
				result);
		}
		return *this = std::move(result);
	}
};

template<size_t N>
constexpr int_tracker<N> if_then_else(
	const bit_tracker& condition,
	const int_tracker<N>& then_value,
	const int_tracker<N>& else_value)
{
	int_tracker<N> result;
	for (size_t i = 0; i < N; ++i)
		result.bits[i] = if_then_else(condition, then_value.bits[i], else_value.bits[i]);
	return result;
}

// True when any bit differs: the OR of the bitwise XOR.
template<size_t N>
[[nodiscard]] constexpr bit_tracker differs(
	const int_tracker<N>& lhs,
	const int_tracker<N>& rhs)
{
	bit_tracker result = false;
	for (size_t i = 0; i < N; ++i)
		result |= lhs.bits[i] ^ rhs.bits[i];
	return result;
}

template<size_t N>
constexpr prepared_divisor<N>::prepared_divisor(const int_tracker<N>& divisor) :
	profile_(divisor.bits)
{
	if (!profile_.is_constant())
		throw std::invalid_argument("prepared divisor must be constant");
}

template<size_t N>
constexpr int_tracker<N> prepared_divisor<N>::divide(
	const int_tracker<N>& dividend) const
{
	return dividend.divide(*this);
}

template<size_t N>
constexpr int_tracker<N> prepared_divisor<N>::modulo(
	const int_tracker<N>& dividend) const
{
	return dividend.modulo(*this);
}

template<size_t N>
constexpr int_tracker<N> prepared_divisor<N>::divmod(
	const int_tracker<N>& dividend,
	int_tracker<N>& remainder) const
{
	return dividend.divmod(*this, remainder);
}

// Classifies a constant divisor once, for repeated division by it.
template<size_t N>
[[nodiscard]] constexpr prepared_divisor<N> prepare_divisor(
	const int_tracker<N>& divisor)
{
	return prepared_divisor<N>(divisor);
}

template<size_t N>
[[nodiscard]] constexpr prepared_divisor<N> prepare_divisor(
	std::uintmax_t divisor)
{
	return prepare_divisor(int_tracker<N>(divisor));
}

template<size_t N>
[[nodiscard]] constexpr int_tracker<N> operator/(
	const int_tracker<N>& dividend,
	const prepared_divisor<N>& divisor)
{
	return divisor.divide(dividend);
}

template<size_t N>
[[nodiscard]] constexpr int_tracker<N> operator%(
	const int_tracker<N>& dividend,
	const prepared_divisor<N>& divisor)
{
	return divisor.modulo(dividend);
}

using itu8 = int_tracker<8>;
using itu16 = int_tracker<16>;
using itu32 = int_tracker<32>;
using itu64 = int_tracker<64>;

} // namespace dixelu::bitreverse

#endif // DIXELU_BITREVERSE_TRACKER_H
