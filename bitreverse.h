#ifndef _DIXELU_BITREVERSE_H
#define _DIXELU_BITREVERSE_H

#include <map>
#include <deque>
#include <array>
#include <string>
#include <memory>
#include <utility>
#include <stdexcept>
#include <cinttypes>
#include <optional>
#include <set>
#include <type_traits>

#include "counted_ptr.h"

namespace dixelu
{
namespace bitreverse
{
namespace details
{
constexpr bool enable_optimisers = true;

struct bitstate
{
	counted_ptr<bitstate> _1;
	counted_ptr<bitstate> _2;

#ifndef WITHOUT_DEPTH_TRACKING
	size_t max_depth{0};
#endif

	std::uint8_t state : 1 {0};
	std::uint8_t operation : 7 {'='};
};

constexpr std::pair<bool, char> extract_value_and_operation(std::uint8_t opcode)
{
	bool value = (opcode >> 7);
	char operation = opcode & 0x7F;
	return {value, operation};
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
				return new_state = make_bitstate_operation('!', val2),
					true;
			if (val2->operation == '=' && val2->state == false)
				return new_state = val1, true;
			if (val2->operation == '=' && val2->state == true)
				return new_state = make_bitstate_operation('!', val1),
					true;

			break;
		}
		default:
			break;
	}
	return false; // optimisation unsuccessful
}

/*
Got:		 ***0*010
Expected:	 00000010
 */

constexpr counted_ptr<bitstate> make_bitstate_operation(
	std::uint8_t opcode,
	const counted_ptr<bitstate>& val1,
	const counted_ptr<bitstate>& val2)
{
	counted_ptr<bitstate> new_state = make_counted<bitstate>();
	auto [current_value, current_operation] = extract_value_and_operation(opcode);

	const counted_ptr<bitstate>* vals[] = {&val1, &val2};
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
			case '|':
				new_state->state = (val1->state | val2->state);
				break;
			case '&':
				new_state->state = (val1->state & val2->state);
				break;
			case '^':
				new_state->state = (val1->state ^ val2->state);
				break;
			case '!':
			case '~':
				new_state->state = ~val1->state;
				break;
			case '=':
				new_state->state = current_value;
				break;
			default:
				throw std::logic_error("Unknown operand");
		}
	}
	else
	{
		if constexpr (enable_optimisers)
		{
			auto successful =
				__call_optimisers(current_operation, val1, val2, new_state);
			if (successful)
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

	constexpr bit_tracker() : bit_state(details::make_bitstate_operation('='))
	{
	};
	constexpr bit_tracker(const bit_tracker&) = default;
	constexpr bit_tracker(bit_tracker&&) = default;

	explicit constexpr bit_tracker(counted_ptr<details::bitstate>&& state) : bit_state(
		std::move(state))
	{
	}

	constexpr bit_tracker(bool value) : bit_state(
		details::make_bitstate_operation('=' | (value << 7)))
	{
	}

	constexpr bit_tracker& operator=(const bit_tracker& rhs) = default;

	constexpr bit_tracker& operator=(bit_tracker&& rhs) noexcept
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

	[[nodiscard]] constexpr char __get_representative_char() const
	{
		if (bit_state->operation == '=')
			return static_cast<char>(bit_state->state + '0');

		return static_cast<char>(bit_state->operation);
	}
};

constexpr bit_tracker execute_ternary_operation(
	const bit_tracker& source,
	const bit_tracker& val1,
	const bit_tracker& val2)
{
	return ((!source) & val2) | (source & val1);
}

template <size_t N>
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

	template <typename convertable_to_int>
	int_tracker(
		typename std::enable_if<
			std::is_convertible<convertable_to_int, std::uintmax_t>::value,
			convertable_to_int>::type maxint_value) :
		int_tracker(static_cast<std::uintmax_t>(maxint_value))
	{
	}

	template <size_t Q>
	constexpr int_tracker(const int_tracker<Q>& rhs)
	{
		auto rhs_rit = rhs.bits.crbegin();
		auto this_rit = bits.rbegin();

		for (; rhs_rit != rhs.bits.crend() && this_rit != bits.rend(); ++rhs_rit, ++this_rit)
			*this_rit = *rhs_rit;
	}

	constexpr int_tracker(bit_tracker bit) :
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

	explicit constexpr int_tracker(std::array<bit_tracker, N>&& bits) :
		bits(std::move(bits))
	{
	}

	constexpr self_type& operator=(self_type&& rhs) noexcept
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

	constexpr self_type& operator>>=(const self_type& shift)
	{
		self_type copy = *this;
		for (size_t i = 1; i < N; ++i)
		{
			size_t bit_index = N - i - 1;
			copy = __execute_ternary_assign(shift.bits[bit_index], copy, copy >> i);
		}
		return (*this = std::move(copy));
	}

	constexpr self_type& operator<<=(const self_type& shift)
	{
		self_type copy = *this;
		for (size_t i = 1; i < N; ++i)
		{
			size_t bit_index = N - i - 1;
			copy = __execute_ternary_assign(shift.bits[bit_index], copy, copy << i);
		}
		return (*this = std::move(copy));
	}

	constexpr self_type operator>>(const self_type& shift)
	{
		self_type copy = *this;
		copy >>= shift;
		return copy;
	}

	constexpr self_type operator<<(const self_type& shift)
	{
		self_type copy = *this;
		copy <<= shift;
		return copy;
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

struct crs_state
{
	std::deque<std::pair<counted_ptr<details::bitstate>, bool>> worklist;
	std::map<counted_ptr<details::bitstate>, bool> assignments;
	std::set<counted_ptr<details::bitstate>> undecided;
};

void propagate(crs_state &crs, const counted_ptr<details::bitstate>& state, bool value)
{
	auto& op = state->operation;
	auto& v1 = state->_1;
	auto& v2 = state->_2;

	crs.undecided.erase(state);

	if (op == '=')
	{
		if (value != state->state)
			__debugbreak();

		return;
	}

	auto get_value = [&](const counted_ptr<details::bitstate>& s) -> std::optional<bool>
	{
		if (!s)
			return std::nullopt;
		if (s->operation == '=')
			return s->state != 0;

		auto it = crs.assignments.find(s);
		if (it != crs.assignments.end())
			return it->second;
		return std::nullopt;
	};

	// if the value is not yet known -> put it into undecided.

	auto v1_val = get_value(v1);
	auto v2_val = get_value(v2);

	if (v1 && !v1_val)
		crs.undecided.insert(v1);
	if (v2 && !v2_val)
		crs.undecided.insert(v2);

	if (op == '^')
	{
		if (v1_val && v2_val && *v1_val ^ *v2_val != value)
			__debugbreak();

		// If one input is known, the other is determined
		if (v1_val)
			crs.worklist.push_back({v2, *v1_val ^ value});

		if (v2_val)
			crs.worklist.push_back({v1, *v2_val ^ value});
	}
	else if (op == '&')
	{
		// If A&B=1, then A=1 and B=1
		if (value == true)
		{
			crs.worklist.push_back({v1, true});
			crs.worklist.push_back({v2, true});
			return;
		}

		if (v1_val && *v1_val == true)
			crs.worklist.push_back({v2, false});

		if (v2_val && *v2_val == true)
			crs.worklist.push_back({v1, false});
	}
	else if (op == '|')
	{
		// If A|B=0, then A=0 and B=0
		if (value == false)
		{
			crs.worklist.push_back({v1, false});
			crs.worklist.push_back({v2, false});
			return;
		}

		if (v1_val && *v1_val == false)
			crs.worklist.push_back({v2, true});

		if (v2_val && *v2_val == false)
			crs.worklist.push_back({v1, true});
	}
	else if (op == '!')
		crs.worklist.push_back({v1, !value});

	// Base case: op is '*' (unknown) or '=' (constant). No further propagation.
}

bool solve(crs_state& crs)
{
	while (!crs.worklist.empty())
	{
		auto [current_state, required_value] = crs.worklist.front();
		crs.worklist.pop_front();

		std::optional<bool> curr_val = std::nullopt;
		if (current_state->operation == '=')
			curr_val = current_state->state != 0;
		else
		{
			auto iter = crs.assignments.find(current_state);
			if (iter != crs.assignments.end())
				curr_val = iter->second;
		}

		if (curr_val.has_value())
		{
			if (*curr_val != required_value)
				return false;

			continue;
		}

		// Assign (skip constants, as they're fixed)
		if (current_state->operation != '=')
			crs.assignments[current_state] = required_value;

		propagate(crs, current_state, required_value);
	}

	return true;
}

crs_state resolve_bit_collisions(bit_tracker& bit, bool state)
{
	std::deque<crs_state> states;

	states.emplace_back();
	states.back().worklist.emplace_back(bit.bit_state, state);
	states.back().undecided; // this is not the correct approach get_all_variables(bit.bit_state);

	while (!states.empty())
	{
		auto& crs = states.back();

		if (!solve(crs))
		{
			states.pop_back();
			std::cout << "Branching failed\n";
			continue;
		}

		if (crs.undecided.empty())
		{
			return crs;
		}

		// Incomplete, need to branch.
		auto iter = crs.undecided.begin();
		auto bit_ptr = *iter; // Pick a variable to branch on.

		// Pop the current ambiguous state from the stack.
		crs_state original_state = std::move(states.back());
		states.pop_back();

		// Create two new, clean states for each branch.
		crs_state false_branch_state = original_state;
		crs_state true_branch_state = std::move(original_state); // Efficiently move from the temp

		// Add the respective guess to the worklist of each new state.
		false_branch_state.worklist.push_back({bit_ptr, false});
		true_branch_state.worklist.push_back({bit_ptr, true});

		// Push the new branches onto the stack. The one pushed last will be processed first.
		// We push the "true" branch first, so the "false" branch is on top and gets priority.
		states.push_back(std::move(true_branch_state));
		states.push_back(std::move(false_branch_state));

		std::cout << "Branched @ " << bit_ptr->max_depth << " depth\n";
	}

	return {};
}

}

// Update the assert_equality functions to call the resolver
std::map<counted_ptr<details::bitstate>, bool>
	assert_equality(const bit_tracker& lhs, const bit_tracker& rhs)
{
	auto is_not_equal = (lhs ^ rhs);
	auto assignments = collision_resolution::resolve_bit_collisions(is_not_equal, false);
	if (!assignments.undecided.empty())
		throw std::runtime_error("Unsatisfiable constraints");

	return assignments.assignments;
}

template <size_t N>
std::map<counted_ptr<details::bitstate>, bool>
	assert_equality(const int_tracker<N>& lhs, const int_tracker<N>& rhs)
{
	bit_tracker result = 0;
	for (size_t index = 0; index < N; ++index)
		result |= (lhs.bits[index] ^ rhs.bits[index]);
	return assert_equality(result, 0);
}

void assign_assert_result(bit_tracker& value, const std::map<counted_ptr<details::bitstate>, bool>& assignments)
{
	const auto iter = assignments.find(value.bit_state);
	if (iter == assignments.end())
		return;

	value = iter->second;
}

template <size_t N>
void assign_assert_result(int_tracker<N>& value, const std::map<counted_ptr<details::bitstate>, bool>& assignments)
{
	for (auto& bit_tracker : value.bits)
		assign_assert_result(bit_tracker, assignments);
}

} // namespace bitreverse
} // namespace dixelu

#endif
