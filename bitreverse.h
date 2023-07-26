#ifndef _DIXELU_BITREVERSE_H
#define _DIXELU_BITREVERSE_H

#include <array>
#include <memory>
#include <vector>
#include <utility>
#include <optional>
#include <stdexcept>
#include <cinttypes>
#include <type_traits>

#include "counted_ptr.h"

namespace dixelu
{
namespace bitreverse
{

namespace details
{

struct bitstate
{
	counted_ptr<bitstate> _1;
	counted_ptr<bitstate> _2;
	counted_ptr<bitstate> _3;

#ifndef WITHOUT_DEPTH_TRACKING
	size_t max_depth{0};
#endif

	std::uint8_t state : 1 {0};
	std::uint8_t operation : 7 {'='};
};

std::pair<bool, char> extract_value_and_operation(std::uint8_t opcode)
{
	bool value = (opcode >> 7);
	char operation = opcode & 0x7F;
	return { value, operation };
}

constexpr std::array<std::uint8_t, 128> get_operation_args_count()
{
	std::array<std::uint8_t, 128> a{};
	for (auto& el : a)
		el = 0;

	a['?'] = 3;
	a['^'] = a['|'] = a['&'] = 2;
	a['!'] = 1;
	a['='] = a['*'] /* unknown */ = 0;

	return a;
}

constexpr auto operation_args_count = get_operation_args_count();

counted_ptr<bitstate> make_bitstate_operation(
	std::uint8_t opcode,
	const counted_ptr<bitstate>& val1 = {},
	const counted_ptr<bitstate>& val2 = {},
	const counted_ptr<bitstate>& val3 = {})
{
	counted_ptr<bitstate> new_state = make_counted<bitstate>();
	auto [current_value, current_operation] = extract_value_and_operation(opcode);

	const counted_ptr<bitstate>* vals[] = { &val1, &val2, &val3 };
	bool is_inplace_calculable = current_operation != '*';

	for (size_t i = 0; i < operation_args_count[current_operation]; i++)
	{
		auto& viewed_bitstate = (**vals[i]);
		if (viewed_bitstate.operation != '=')
			is_inplace_calculable = false;
	}

	if (current_operation == '?' && val1->operation == '=')
		is_inplace_calculable = true;

	if (is_inplace_calculable)
	{
		new_state->operation = '=';

		switch (current_operation)
		{
		case '|': new_state->state = (val1->state | val2->state); break;
		case '&': new_state->state = (val1->state & val2->state); break;
		case '^': new_state->state = (val1->state ^ val2->state); break;
		case '?': new_state = (val1->state ? val2 : val3); break;
		case '!': case '~': new_state->state = ~val1->state; break;
		case '=': new_state->state = current_value; break;
		default:
			throw std::logic_error("Unknown operand");
		}
	}
	else
	{
		new_state->state = 0;
		new_state->operation = current_operation;
		new_state->_1 = val1;
		new_state->_2 = val2;
		new_state->_3 = val3;

#ifndef WITHOUT_DEPTH_TRACKING
		new_state->max_depth = 1 +
			std::max(
				(val1 ? val1->max_depth : 0),
				std::max(
					(val2 ? val2->max_depth : 0), 
					(val3 ? val3->max_depth : 0)));
#endif // !WITHOUT_DEPTH_TRACKING
	}

	return new_state;
}

} // namespace details

struct __UNKNOWN__ {};

constexpr __UNKNOWN__ unknown;

struct bit_tracker
{
	counted_ptr<details::bitstate> bit_state;

	bit_tracker() : bit_state(details::make_bitstate_operation('=')) {};
	bit_tracker(const bit_tracker&) = default; 
	bit_tracker(bit_tracker&&) = default;

	explicit bit_tracker(counted_ptr<details::bitstate>&& state) : bit_state(std::move(state)) {}

	bit_tracker(bool value) : bit_state(details::make_bitstate_operation('=' | (value << 7))) {}
	bit_tracker(size_t value) : bit_state(details::make_bitstate_operation('=' | (bool(value) << 7))) {}

	bit_tracker& operator=(const bit_tracker& rhs)
	{
		bit_state = rhs.bit_state;
		return *this;
	}

	bit_tracker& operator=(bit_tracker&& rhs)
	{
		bit_state = std::move(rhs.bit_state);
		return *this;
	}

	bit_tracker operator=(__UNKNOWN__ _) { bit_state = details::make_bitstate_operation('*'); return *this; }

	bit_tracker& operator|=(const bit_tracker& rhs)
	{
		bit_state = details::make_bitstate_operation('|', bit_state, rhs.bit_state);
		return *this;
	}

	bit_tracker& operator&=(const bit_tracker& rhs)
	{
		bit_state = details::make_bitstate_operation('&', bit_state, rhs.bit_state);
		return *this;
	}

	bit_tracker& operator^=(const bit_tracker& rhs)
	{
		bit_state = details::make_bitstate_operation('^', bit_state, rhs.bit_state);
		return *this;
	}

	bit_tracker operator|(const bit_tracker& rhs) const
	{
		bit_tracker tracker = *this;
		tracker |= rhs;
		return tracker;
	}

	bit_tracker operator&(const bit_tracker& rhs) const
	{
		bit_tracker tracker = *this;
		tracker &= rhs;
		return tracker;
	}	
	
	bit_tracker operator^(const bit_tracker& rhs) const
	{
		bit_tracker tracker = *this;
		tracker ^= rhs;
		return tracker;
	}	

	bit_tracker operator~() const
	{
		return bit_tracker(details::make_bitstate_operation('~', bit_state));
	}

	bit_tracker operator!() const
	{
		return bit_tracker(details::make_bitstate_operation('!', bit_state));
	}
};

bit_tracker execute_ternary_operation(
	const bit_tracker& source,
	const bit_tracker& val1,
	const bit_tracker& val2)
{
	return bit_tracker(details::make_bitstate_operation('?', source.bit_state, val1.bit_state, val2.bit_state));
}

template<size_t N>
struct int_tracker
{
	std::array<bit_tracker, N> bits;

	int_tracker()
	{
		for (auto& el : bits)
			el = false;
	}

	int_tracker(__UNKNOWN__ unknown_rhs)
	{
		for (auto& el : bits)
			el = unknown_rhs;
	}

	int_tracker(std::uintmax_t maxint_value)
	{
		for (auto& el : bits)
			el = false;

		for (size_t i = 0; i < N && maxint_value; ++i)
		{
			bool value = maxint_value & 1;
			bits[N - i - 1] = value;
			maxint_value >>= 1;
		}
	}

	int_tracker(bit_tracker bit):
		int_tracker()
	{
		bits.back() = std::move(bit);
	}

	int_tracker(int_tracker&&) = default;
	int_tracker(const int_tracker&) = default;
	int_tracker& operator=(const int_tracker& rhs)
	{
		for (size_t i = 0; i < N; ++i)
			bits[i] = rhs.bits[i];
		return *this;
	}

	int_tracker& operator=(int_tracker&& rhs)
	{
		for (size_t i = 0; i < N; ++i)
			bits[i] = std::move(rhs.bits[i]);
		return *this;
	}

	int_tracker& operator|=(const int_tracker& rhs)
	{
		for (size_t i = 0; i < N; ++i)
			bits[i] |= rhs.bits[i];
		return *this;
	}

	int_tracker& operator&=(const int_tracker& rhs)
	{
		for (size_t i = 0; i < N; ++i)
			bits[i] &= rhs.bits[i];
		return *this;
	}

	int_tracker& operator^=(const int_tracker& rhs)
	{
		for (size_t i = 0; i < N; ++i)
			bits[i] &= rhs.bits[i];
		return *this;
	}

	int_tracker operator|(const int_tracker& rhs) const
	{
		int_tracker copy = *this;
		copy |= rhs;
		return copy;
	}

	int_tracker operator&(const int_tracker& rhs) const
	{
		int_tracker copy = *this;
		copy &= rhs;
		return copy;
	}

	int_tracker operator^(const int_tracker& rhs) const
	{
		int_tracker copy = *this;
		copy ^= rhs;
		return copy;
	}

	int_tracker operator~() const
	{
		int_tracker value = *this;
		for (size_t i = 0; i < N; ++i)
			value.bits[i] = ~value.bits[i];
		return value;
	}

	int_tracker& operator=(__UNKNOWN__ unknown_rhs)
	{
		for (size_t i = 0; i < N; ++i)
			bits[i] = unknown_rhs;
		return *this;
	}

	explicit operator bit_tracker()
	{
		bit_tracker result; 
		for (size_t i = 0; i < N; ++i)
			result |= bits[i];
		return result;
	}

	bit_tracker operator!()
	{
		return !(bit_tracker)*this;
	}
};

} // namespace bitreverse
} // namespace dixelu

#endif