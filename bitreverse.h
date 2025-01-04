#ifndef _DIXELU_BITREVERSE_H
#define _DIXELU_BITREVERSE_H

#include <set>
#include <map>
#include <deque>
#include <array>
#include <memory>
#include <vector>
#include <utility>
#include <stdexcept>
#include <cinttypes>
#include <functional>
#include <type_traits>

#include "counted_ptr.h"

namespace dixelu
{
namespace bitreverse
{

namespace details
{

constexpr bool enable_optimisers = true;

struct universe;

struct bitstate
{
	std::set<counted_ptr<universe>> universes;

	counted_ptr<bitstate> _1;
	counted_ptr<bitstate> _2;

#ifndef WITHOUT_DEPTH_TRACKING
	size_t max_depth{0};
#endif

	std::uint8_t state : 1 {0};
	std::uint8_t operation : 7 {'='};
};

struct universe:
	enable_counted_from_this<universe>
{
	using universe_ptr = counted_ptr<universe>;
	counted_ptr<universe> parent_universe;
	std::map<counted_ptr<bitstate>, bool> linked_states;
};

constexpr std::pair<bool, char> extract_value_and_operation(std::uint8_t opcode)
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

	a['^'] = a['|'] = a['&'] = 2;
	a['!'] = 1;
	a['='] = a['*'] /* unknown */ = 0;

	return a;
}

constexpr auto operation_args_count = get_operation_args_count();

constexpr counted_ptr<bitstate> make_bitstate_operation(
	std::uint8_t opcode,
	const counted_ptr<bitstate>& val1 = {},
	const counted_ptr<bitstate>& val2 = {});

constexpr bool __call_optimisers(
	std::uint8_t current_operation,
	const counted_ptr<bitstate>& val1,
	const counted_ptr<bitstate>& val2,
	counted_ptr<bitstate>& new_state)
{
	switch (current_operation)
	{
	case '|':
	{
		/* optimize constant expressions */
		if (val1->operation == '=' && val1->state == true)
			return new_state = val1, true;
		if (val1->operation == '=' && val1->state == false)
			return new_state = val2, true;
		if (val2->operation == '=' && val2->state == true)
			return new_state = val2, true;
		if (val2->operation == '=' && val2->state == false)
			return new_state = val1, true;

		break;
	}
	case '&':
	{
		/* optimize constant expressions */
		if (val1->operation == '=' && val1->state == false)
			return new_state = val1, true;
		if (val1->operation == '=' && val1->state == true)
			return new_state = val2, true;
		if (val2->operation == '=' && val2->state == false)
			return new_state = val2, true;
		if (val2->operation == '=' && val2->state == true)
			return new_state = val1, true;

		break;
	}
	case '^':
	{
		/* optimize constant expressions */
		if (val1->operation == '=' && val1->state == false)
			return new_state = val2, true;
		if (val1->operation == '=' && val1->state == true)
			return new_state = details::make_bitstate_operation('!', val2), true;
		if (val2->operation == '=' && val2->state == false)
			return new_state = val1, true;
		if (val2->operation == '=' && val2->state == true)
			return new_state = details::make_bitstate_operation('!', val1), true;

		break;
	}
	}
	return false; // optimisation unsuccessfull
}

constexpr counted_ptr<bitstate> make_bitstate_operation(
	std::uint8_t opcode,
	const counted_ptr<bitstate>& val1,
	const counted_ptr<bitstate>& val2)
{
	counted_ptr<bitstate> new_state = make_counted<bitstate>();
	auto [current_value, current_operation] = extract_value_and_operation(opcode);

	const counted_ptr<bitstate>* vals[] = { &val1, &val2 };
	bool is_inplace_calculable = current_operation != '*';

	for (size_t i = 0; i < operation_args_count[current_operation]; i++)
	{
		auto& viewed_bitstate = (**vals[i]);
		if (viewed_bitstate.operation != '=')
			is_inplace_calculable = false;
	}

	if (is_inplace_calculable)
	{
		new_state->operation = '=';

		switch (current_operation)
		{
		case '|': new_state->state = (val1->state | val2->state); break;
		case '&': new_state->state = (val1->state & val2->state); break;
		case '^': new_state->state = (val1->state ^ val2->state); break;
		case '!':
		case '~': new_state->state = ~val1->state; break;
		case '=': new_state->state = current_value; break;
		default:
			throw std::logic_error("Unknown operand");
		}
	}
	else
	{
		if constexpr (enable_optimisers)
		{
			auto optimisation_successfull =
				__call_optimisers(current_operation, val1, val2, new_state);
			if (optimisation_successfull)
				return new_state;
		}

		new_state->state = 0;
		new_state->operation = current_operation;
		new_state->_1 = val1;
		new_state->_2 = val2;

#ifndef WITHOUT_DEPTH_TRACKING
		new_state->max_depth = 1 +
			std::max(
				(val1 ? val1->max_depth : 0),
				(val2 ? val2->max_depth : 0)
			);
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

	constexpr bit_tracker() : bit_state(details::make_bitstate_operation('=')) {};
	constexpr bit_tracker(const bit_tracker&) = default;
	constexpr bit_tracker(bit_tracker&&) = default;

	explicit constexpr bit_tracker(counted_ptr<details::bitstate>&& state) : bit_state(std::move(state)) {}

	constexpr bit_tracker(bool value) : bit_state(details::make_bitstate_operation('=' | (value << 7))) {}
	//constexpr bit_tracker(size_t value) : bit_state(details::make_bitstate_operation('=' | (bool(value) << 7))) {}

	constexpr bit_tracker& operator=(const bit_tracker& rhs)
	{
		bit_state = rhs.bit_state;
		return *this;
	}

	constexpr bit_tracker& operator=(bit_tracker&& rhs)
	{
		bit_state = std::move(rhs.bit_state);
		return *this;
	}

	constexpr bit_tracker operator=(__UNKNOWN__ _)
	{
		bit_state = details::make_bitstate_operation('*');
		return *this;
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
		return bit_tracker(details::make_bitstate_operation('~', bit_state));
	}

	constexpr bit_tracker operator!() const
	{
		return bit_tracker(details::make_bitstate_operation('!', bit_state));
	}

	constexpr char __get_representative_char() const
	{
		if (bit_state->operation == '=')
			return '0' + bit_state->state;
		return bit_state->operation;
	}
};

constexpr bit_tracker execute_ternary_operation(
	const bit_tracker& source,
	const bit_tracker& val1,
	const bit_tracker& val2)
{
	return ((!source) & val2) | (source & val1);
}

template<size_t N>
struct int_tracker
{
	using self_type = int_tracker<N>;
	std::array<bit_tracker, N> bits;

	constexpr int_tracker()
	{
		for (auto& el : bits)
			el = false;
	}

	constexpr int_tracker(__UNKNOWN__ unknown_rhs)
	{
		for (auto& el : bits)
			el = unknown_rhs;
	}

	constexpr int_tracker(std::uintmax_t maxint_value)
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

	template<typename convertable_to_int>
	int_tracker(
		typename std::enable_if<
			std::is_convertible<convertable_to_int, std::uintmax_t>::value,
			convertable_to_int>::type maxint_value) :
		int_tracker((std::uintmax_t)maxint_value)
	{
	}

	template<size_t Q>
	constexpr int_tracker(const int_tracker<Q>& rhs)
	{
		auto rhs_rit = rhs.bits.crbegin();
		auto this_rit = bits.rbegin();

		for (; rhs_rit != rhs.bits.crend() && this_rit != bits.rend(); ++rhs_rit, ++this_rit)
			*this_rit = *rhs_rit;
	}

	constexpr int_tracker(bit_tracker bit):
		int_tracker()
	{
		bits.back() = std::move(bit);
	}

	constexpr int_tracker(self_type&&) = default;
	constexpr int_tracker(const self_type&) = default;
	constexpr int_tracker& operator=(const self_type& rhs)
	{
		for (size_t i = 0; i < N; ++i)
			bits[i] = rhs.bits[i];
		return *this;
	}

	explicit constexpr int_tracker(std::array<bit_tracker, N>&& bits):
		bits(std::move(bits)) { }

	constexpr self_type& operator=(self_type&& rhs)
	{
		for (size_t i = 0; i < N; ++i)
			bits[i] = std::move(rhs.bits[i]);
		return *this;
	}

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

	constexpr self_type& operator=(__UNKNOWN__ unknown_rhs)
	{
		for (size_t i = 0; i < N; ++i)
			bits[i] = unknown_rhs;
		return *this;
	}

	explicit constexpr operator bit_tracker()
	{
		bit_tracker result;
		for (size_t i = 0; i < N; ++i)
			result |= bits[i];
		return result;
	}

	constexpr bit_tracker operator!() const
	{
		return !(bit_tracker)*this;
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

		for (ptrdiff_t i = N - 1; i >= shift; --i)
			bits[i] = std::move(bits[i - shift]);
		for (size_t i = 0; i < shift; ++i)
			bits[i] = false;

		return *this;
	}

	constexpr static self_type __execute_ternary_assign(
		const bit_tracker& condition,
		const self_type& lhs,
		const self_type& rhs)
	{
		self_type t;
		for (size_t i = 0; i < N; ++i)
			t.bits[i] = execute_ternary_operation(condition, lhs.bits[i], rhs.bits[i]);
		return t;
	}

	constexpr self_type& operator>>=(self_type& shift)
	{
		self_type copy = *this;
		size_t shift_count = 0;
		for (size_t i = 1; i < N; ++i)
		{
			size_t bit_index = N - i - 1;
			shift_count << 1;
			copy = __execute_ternary_assign(shift.bits[bit_index], copy, copy >> i);
		}
		return (*this = std::move(copy));
	}

	constexpr self_type& operator<<=(self_type& shift)
	{
		self_type copy = *this;
		size_t shift_count = 0;
		for (size_t i = 1; i < N; ++i)
		{
			size_t bit_index = N - i - 1;
			shift_count << 1;
			copy = __execute_ternary_assign(shift.bits[bit_index], copy, copy << i);
		}
		return (*this = std::move(copy));
	}

	constexpr self_type& operator+=(const self_type& rhs)
    {
        bit_tracker carry = false;
        for (size_t i = 0; i < N; ++i)
        {
            auto& lhs_bit = bits[N - 1 - i];
            auto& rhs_bit = rhs.bits[N - 1 - i];

			auto xor_bit = lhs_bit ^ rhs_bit ^ carry;
			carry = (rhs_bit & carry & !lhs_bit) | (lhs_bit & (rhs_bit | carry));
			lhs_bit = xor_bit;
        }
		return *this;
    }

	constexpr self_type& operator-=(const self_type& rhs)
	{
		auto rhs_complement = (~rhs) + 1;
		return (*this += rhs_complement);
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
		auto rhs_complement = this->operator~() + 1;
        return rhs_complement;
    }

	std::string __to_string() const
	{
		std::string str;
		str.reserve(N);
		for (auto& bit : bits)
			str.push_back(bit.__get_representative_char());
		return str;
	}

	size_t __max_depth() const
	{
		size_t max_depth = 0;
#ifndef WITHOUT_DEPTH_TRACKING
		for (auto& bit : bits)
			max_depth = std::max(max_depth, bit.bit_state->max_depth);
#endif
		return max_depth;
	}
};

using itu8 = int_tracker<8>;
using itu16 = int_tracker<16>;
using itu32 = int_tracker<32>;
using itu64 = int_tracker<64>;

namespace collision_resolution
{
	std::set<counted_ptr<details::universe>> __create_universes_on_current_operation(
            const counted_ptr<details::bitstate>& bit_state,
            bool probable_state)
	{
        std::set<counted_ptr<details::universe>> universes;

        auto all_zeros_func = [&universes](
                const counted_ptr<details::bitstate>& lhs,
                const counted_ptr<details::bitstate>& rhs)
        {
            auto all_zeros = make_counted<details::universe>();

            all_zeros->linked_states[lhs] = false;
            if(rhs) // for single operand operators.
                all_zeros->linked_states[rhs] = false;

            universes.insert(std::move(all_zeros));
        };

        auto all_ones_func = [&universes](
                const counted_ptr<details::bitstate>& lhs,
                const counted_ptr<details::bitstate>& rhs)
        {
            auto all_zeros = make_counted<details::universe>();

            all_zeros->linked_states[lhs] = true;
            if(rhs) // for single operand operators.
                all_zeros->linked_states[rhs] = true;

            universes.insert(std::move(all_zeros));
        };

        auto lhs_is_greater_func = [&universes](
                const counted_ptr<details::bitstate>& lhs,
                const counted_ptr<details::bitstate>& rhs)
        {
            auto lhs_is_greater = make_counted<details::universe>();

            lhs_is_greater->linked_states[lhs] = true;
            lhs_is_greater->linked_states[rhs] = false;

            universes.insert(std::move(lhs_is_greater));
        };

        auto rhs_is_greater_func = [&lhs_is_greater_func](
                const counted_ptr<details::bitstate>& lhs,
                const counted_ptr<details::bitstate>& rhs)
        {
            return lhs_is_greater_func(rhs, lhs);
        };

		switch (bit_state->operation)
		{
            case '^':
            {
                auto& lhs = bit_state->_1;
                auto& rhs = bit_state->_2;

                if(!probable_state)
                {
                    all_ones_func(lhs, rhs);
                    all_zeros_func(lhs, rhs);
                }
                else
                {
                    lhs_is_greater_func(lhs, rhs);
                    rhs_is_greater_func(lhs, rhs);
                }

                break;
            }
            case '|':
            {
                auto& lhs = bit_state->_1;
                auto& rhs = bit_state->_2;

                if(probable_state)
                {
                    all_ones_func(lhs, rhs);
                    lhs_is_greater_func(lhs, rhs);
                    rhs_is_greater_func(lhs, rhs);
                }
                else
                    all_zeros_func(lhs, rhs);

                break;
            }
            case '&':
            {
                auto& lhs = bit_state->_1;
                auto& rhs = bit_state->_2;

                if(!probable_state)
                {
                    all_zeros_func(lhs, rhs);
                    lhs_is_greater_func(lhs, rhs);
                    rhs_is_greater_func(lhs, rhs);
                }
                else
                    all_ones_func(lhs, rhs);

                break;
            }
            case '!':
            {
                auto& arg = bit_state->_1;

                if(probable_state)
                    all_zeros_func(arg, {});
                else
                    all_ones_func(arg, {});

                break;
            }
            case '*':
            {
                all_zeros_func(bit_state, {});
                all_ones_func(bit_state, {});

                break;
            }
            case '=':
            {
                auto value = bit_state->state;

                if(value == probable_state)
                {
                    auto all_zeros =
                            make_counted<details::universe>();
                    all_zeros->linked_states[bit_state] = value;
                    universes.insert(std::move(all_zeros));
                }
            }
		}

        return universes;
	}
}

void __build_universe_tree_deferred_recursion(
	counted_ptr<details::bitstate> bit_state,
	std::reference_wrapper<std::deque<std::function<void()>>> deferred_execution_array_ref)
{
	for (auto& current_universe : bit_state->universes)
	{
		for(auto& single_state: current_universe->linked_states)
		{
			auto new_universes =
				collision_resolution::__create_universes_on_current_operation(
					single_state.first,
					single_state.second);

			for(auto new_universe: new_universes)
				new_universe->parent_universe = current_universe;

			// todo: handle the lack of new universes (due to incorrect execution path)
			// todo: save all endpoints (def. unknown/known values)

			auto state_universe_ptr = single_state.first; // force copy
			state_universe_ptr->universes.insert(new_universes.begin(), new_universes.end());

			deferred_execution_array_ref.get().push_back(std::bind(
				__build_universe_tree_deferred_recursion,
				state_universe_ptr,
				deferred_execution_array_ref));
		}
	}
}

void __build_universal_tree(bit_tracker& bit_tracker)
{
	std::deque<std::function<void()>> deferred_execution_array;
	auto deferred_execution_array_ref = std::ref(deferred_execution_array);

	deferred_execution_array_ref.get().push_back(std::bind(
		__build_universe_tree_deferred_recursion,
		bit_tracker.bit_state,
		deferred_execution_array_ref));

	while(deferred_execution_array.size())
	{
		std::cout << (std::to_string(deferred_execution_array.size()) + "\n") << std::flush;
		deferred_execution_array.front()();
		deferred_execution_array.pop_front();
	}
}

void __resolve_bit_collisions(bit_tracker& bit_tracker)
{
	__build_universal_tree(bit_tracker);
	// merge universes from the begining?

}

void assert_equality(const bit_tracker& lhs, const bit_tracker& rhs)
{
	auto is_not_equal = (lhs ^ rhs);

	auto equality_universe_zeros = make_counted<details::universe>();

	equality_universe_zeros->linked_states[is_not_equal.bit_state] = 0;
	is_not_equal.bit_state->universes.insert(equality_universe_zeros);
	__resolve_bit_collisions(is_not_equal);
}

template<size_t N>
void assert_equality(const int_tracker<N>& lhs, const int_tracker<N>& rhs)
{
	bit_tracker result = 0;
	for (size_t index = 0; index < N; ++index)
		result |= (lhs.bits[index] ^ rhs.bits[index]);
	assert_equality(result, 0);
	__resolve_bit_collisions(result);
}

} // namespace bitreverse
} // namespace dixelu

#endif