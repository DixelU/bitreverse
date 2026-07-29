#ifndef DIXELU_BITREVERSE_H
#define DIXELU_BITREVERSE_H

#include <map>
#include <deque>
#include <array>
#include <bit>
#include <functional>
#include <unordered_map>
#include <string>
#include <vector>
#include <memory>
#include <utility>
#include <iostream>
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
	counted_ptr<bitstate> _1{};
	counted_ptr<bitstate> _2{};

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

constexpr std::array<std::uint8_t, 256> get_operation_args_count()
{
	std::array<std::uint8_t, 256> a{};
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

constexpr bool is_same_node(
	const counted_ptr<bitstate>& lhs,
	const counted_ptr<bitstate>& rhs)
{
	return lhs == rhs;
}

constexpr bool is_negation_of(
	const counted_ptr<bitstate>& expression,
	const counted_ptr<bitstate>& candidate)
{
	return expression &&
		expression->operation == '!' &&
		is_same_node(expression->_1, candidate);
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
		(is_same_node(expression->_1, operand) ||
			is_same_node(expression->_2, operand));
}

constexpr counted_ptr<bitstate> make_boolean_constant(bool value)
{
	return make_bitstate_operation(
		static_cast<std::uint8_t>('=' | (static_cast<std::uint8_t>(value) << 7)));
}

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
			// Constant, idempotence, complement, and absorption identities.
			if (val1->operation == '=' && val1->state == true)
				return new_state = val1, true;
			if (val1->operation == '=' && val1->state == false)
				return new_state = val2, true;
			if (val2->operation == '=' && val2->state == true)
				return new_state = val2, true;
			if (val2->operation == '=' && val2->state == false)
				return new_state = val1, true;
			if (is_same_node(val1, val2))
				return new_state = val1, true;
			if (are_complements(val1, val2))
				return new_state = make_boolean_constant(true), true;
			if (contains_operand(val2, '&', val1))
				return new_state = val1, true;
			if (contains_operand(val1, '&', val2))
				return new_state = val2, true;

			break;
		}
		case '&':
		{
			// Constant, idempotence, complement, and absorption identities.
			if (val1->operation == '=' && val1->state == false)
				return new_state = val1, true;
			if (val1->operation == '=' && val1->state == true)
				return new_state = val2, true;
			if (val2->operation == '=' && val2->state == false)
				return new_state = val2, true;
			if (val2->operation == '=' && val2->state == true)
				return new_state = val1, true;
			if (is_same_node(val1, val2))
				return new_state = val1, true;
			if (are_complements(val1, val2))
				return new_state = make_boolean_constant(false), true;
			if (contains_operand(val2, '|', val1))
				return new_state = val1, true;
			if (contains_operand(val1, '|', val2))
				return new_state = val2, true;

			break;
		}
		case '^':
		{
			// Constant, cancellation, and complement identities.
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
			if (is_same_node(val1, val2))
				return new_state = make_boolean_constant(false), true;
			if (are_complements(val1, val2))
				return new_state = make_boolean_constant(true), true;

			break;
		}
		case '!':
			// Involution: NOT(NOT(x)) == x.
			if (val1->operation == '!')
				return new_state = val1->_1, true;
			break;
		default:
			break;
	}
	return false; // optimisation unsuccessful
}

constexpr counted_ptr<bitstate> make_bitstate_operation(
	std::uint8_t opcode,
	const counted_ptr<bitstate>& val1,
	const counted_ptr<bitstate>& val2)
{
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
		counted_ptr<bitstate> new_state = make_counted<bitstate>();
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

		return new_state;
	}

	if constexpr (enable_optimisers)
	{
		counted_ptr<bitstate> optimised_state;
		const bool successful =
			__call_optimisers(current_operation, val1, val2, optimised_state);
		if (successful)
			return optimised_state;
	}

	counted_ptr<bitstate> new_state = make_counted<bitstate>();
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

	return new_state;
}

void __print_bt(const std::string& prefix, const bitstate* node, bool isLeft)
{
	if( node != nullptr )
	{
		std::cout << prefix;

		std::cout << (isLeft ? "V---" : "L---" );

		// print the value of the node
		if (node->operation == '=')
			std::cout << static_cast<int>(node->state) << '\n';
		else if (node->operation == '*')
			std::cout << "* @x" << std::hex << reinterpret_cast<size_t>(node) << std::dec << "\n";
		else
			std::cout << static_cast<char>(node->operation) << '\n';

		// enter the next tree level - left and right branch
		__print_bt( prefix + (isLeft ? "V   " : "    "), node->_1.get(), true);
		__print_bt( prefix + (isLeft ? "V   " : "    "), node->_2.get(), false);
	}
}

void print_bs(const bitstate& node)
{
	__print_bt("", &node, false);
	std::cout << std::flush;
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

	constexpr bit_tracker& operator=(__UNKNOWN__)
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
		return bit_tracker(details::make_bitstate_operation('!', bit_state));
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

template<typename T, bool _const = true>
struct ref_handler
{
	using ref_t = std::conditional<_const, const T&, T&>::type;

	ref_t ref;

	ref_handler(ref_t ref) : ref(ref) {};

	ref_handler(T&&) = delete;
	ref_handler(const ref_handler&) = delete;

	constexpr operator ref_t() const { return ref; }
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

		for (size_t i = N; i-- > shift;)
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
		self_type result = *this;

		// Применяем сдвиг для каждого бита в shift
		for (size_t i = 0; i < N; ++i)
		{
			size_t shift_amount = 1 << i;  // 2^i
			if (shift_amount >= N)  // Если сдвиг превышает размер, прекращаем
				break;

			// Если бит установлен, применяем соответствующий сдвиг
			self_type shifted = result >> shift_amount;
			result = __execute_ternary_assign(shift.bits[N - i - 1], shifted, result);
		}

		return (*this = std::move(result));
	}

	constexpr self_type& operator<<=(const self_type& shift)
	{
		self_type result = *this;

		// Применяем сдвиг для каждого бита в shift
		for (size_t i = 0; i < N; ++i)
		{
			size_t shift_amount = 1 << i;  // 2^i
			if (shift_amount >= N)  // Если сдвиг превышает размер, прекращаем
				break;

			// Если бит установлен, применяем соответствующий сдвиг
			self_type shifted = result << shift_amount;
			result = __execute_ternary_assign(shift.bits[N - i - 1], shifted, result);
		}

		return (*this = std::move(result));
	}

	constexpr self_type operator>>(ref_handler<self_type> shift) const
	{
		self_type copy = *this;
		copy >>= shift;
		return copy;
	}

	constexpr self_type operator<<(ref_handler<self_type> shift) const
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

	[[nodiscard]] std::string __to_string() const
	{
		std::string str;
		str.reserve(N);
		for (auto& bit : bits)
			str.push_back(bit.__get_representative_char());
		return str;
	}

	[[nodiscard]] size_t __max_depth() const
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

struct worklist_data
{
	counted_ptr<details::bitstate> state;
	bool value;
	bool force_check;
};

struct crs_state
{
	struct parent_data
	{
		counted_ptr<details::bitstate> parent{};
		bool state{false};
	};

	std::deque<worklist_data> worklist;
	std::map<const counted_ptr<details::bitstate>, bool> assignments;
	std::map<const counted_ptr<details::bitstate>, parent_data> undecided; // unresolved parent constraints

	auto operator<=>(const crs_state& state) const
	{
		return assignments <=> state.assignments;
	}
};

bool inline_execute(uint8_t operation, bool lhs, bool rhs)
{
	switch (operation)
	{
		case '&': return lhs && rhs;
		case '|': return lhs || rhs;
		case '^': return lhs != rhs;
		case '!': return !lhs;
		default:
			//__debugbreak();
			return false;
	}
}

bool is_const_operand(char op) { return op == '=' || op == '*'; }
std::optional<bool> get_value(const counted_ptr<details::bitstate>& s, const crs_state& crs);

bool propagate(crs_state &crs, const counted_ptr<details::bitstate>& state, bool value)
{
	auto& op = state->operation;
	const auto& v1 = state->_1;
	const auto& v2 = state->_2;

	crs.undecided.erase(state);

	if (op == '=')
	{
		if (value != state->state)
			return false;

		return true;
	}

	// if the value is not yet known -> put it into undecided.

	auto v1_val = get_value(v1, crs);
	auto v2_val = get_value(v2, crs);

	if (v1_val && v2_val)
	{
		if (inline_execute(op, *v1_val, *v2_val) != value)
			return false;

		crs.worklist.push_back({v1, *v1_val, true});
		crs.worklist.push_back({v2, *v2_val, true});
		return true;
	}

	if (op == '^')
	{
		// If one input is known, the other is determined
		if (v1_val)
			crs.worklist.push_back(worklist_data{v2, static_cast<bool>(*v1_val ^ value), false});

		if (v2_val)
			crs.worklist.push_back(worklist_data{v1, static_cast<bool>(*v2_val ^ value), false});
	}
	else if (op == '&')
	{
		// If A&B=1, then A=1 and B=1
		if (value == true)
		{
			crs.worklist.push_back({v1, true, false});
			crs.worklist.push_back({v2, true, false});
			return true;
		}

		if (v1_val && *v1_val == true)
			crs.worklist.push_back({v2, false, false});

		if (v2_val && *v2_val == true)
			crs.worklist.push_back({v1, false, false});
	}
	else if (op == '|')
	{
		// If A|B=0, then A=0 and B=0
		if (value == false)
		{
			crs.worklist.push_back({v1, false, false});
			crs.worklist.push_back({v2, false, false});
			return true;
		}

		if (v1_val && *v1_val == false)
			crs.worklist.push_back({v2, true, false});

		if (v2_val && *v2_val == false)
			crs.worklist.push_back({v1, true, false});
	}
	else if (op == '!')
		crs.worklist.push_back({v1, !value, false});

	if (op != '!' && ((v1 && !v1_val) || (v2 && !v2_val)))
		crs.undecided[state] = crs_state::parent_data{state, value};

	// Base case: op is '*' (unknown) or '=' (constant). No further propagation.
	return true;
}

bool solve(crs_state& crs)
{
	while (!crs.worklist.empty())
	{
		auto [current_state, required_value, force_propagate] = std::move(crs.worklist.front());
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

			if (!force_propagate)
				continue;
		}

		// Assign (skip constants, as they're fixed)
		if (current_state->operation != '=')
			crs.assignments[current_state] = required_value;

		if (!propagate(crs, current_state, required_value))
			return false;
	}

	return true;
}

std::optional<bool> get_value(const counted_ptr<details::bitstate>& s, const crs_state& crs)
{
	if (!s)
		return std::nullopt;
	if (s->operation == '=')
		return static_cast<bool>(s->state);

	auto it = crs.assignments.find(s);
	if (it != crs.assignments.end())
		return it->second;

	return std::nullopt;
}

void smart_assume(
	std::deque<crs_state>& states,
	crs_state original_state,
	const crs_state::parent_data& pd)
{
	auto parent = pd.parent;
	bool parent_value = pd.state;

	// Smart branching based on parent's operation
	auto v1 = parent->_1;
	auto v2 = parent->_2;
	char op = parent->operation;

	//if (!v2)
	//	throw std::runtime_error("Assumptions are not possible for single variable operands");

	auto gv1 = get_value(v1, original_state);
	auto gv2 = get_value(v2, original_state);

	// Binary operations
	std::vector<std::pair<bool, bool>> combos;

	if (op == '&')
	{
		if (parent_value)
			combos = {{true, true}};
		else
			combos = {{false, false}, {false, true}, {true, false}};
	}
	else if (op == '|')
	{
		if (parent_value)
			combos = {{false, true}, {true, false}, {true, true}};
		else
			combos = {{false, false}};
	}
	else if (op == '^')
	{
		if (parent_value)
			combos = {{false, true}, {true, false}};
		else
			combos = {{false, false}, {true, true}};
	}
	else
		throw std::runtime_error("Unknown operation");

	for (auto [val1, val2] : combos)
	{
		if (gv1.has_value() && *gv1 != val1)
			continue;

		if (gv2.has_value() && *gv2 != val2)
			continue;

		crs_state branch = original_state;
		if (!gv1.has_value())
			branch.assignments[v1] = val1;
		if (!gv2.has_value())
			branch.assignments[v2] = val2;

		branch.worklist.push_back({parent, parent_value, false});
		states.push_back(std::move(branch));
	}
}

size_t unresolved_branch_count(const crs_state& state, const crs_state::parent_data& pd)
{
	const auto parent = pd.parent;
	const auto gv1 = get_value(parent->_1, state);
	const auto gv2 = get_value(parent->_2, state);

	size_t count = 0;
	const auto accepts = [&](bool lhs, bool rhs)
	{
		if (gv1.has_value() && *gv1 != lhs)
			return false;
		if (gv2.has_value() && *gv2 != rhs)
			return false;
		return true;
	};

	switch (parent->operation)
	{
		case '&':
			if (pd.state)
				count += accepts(true, true);
			else
			{
				count += accepts(false, false);
				count += accepts(false, true);
				count += accepts(true, false);
			}
			break;
		case '|':
			if (pd.state)
			{
				count += accepts(false, true);
				count += accepts(true, false);
				count += accepts(true, true);
			}
			else
				count += accepts(false, false);
			break;
		case '^':
			if (pd.state)
			{
				count += accepts(false, true);
				count += accepts(true, false);
			}
			else
			{
				count += accepts(false, false);
				count += accepts(true, true);
			}
			break;
		default:
			return 4;
	}

	return count;
}

void retain_only_unknown_assignments(crs_state& state)
{
	for (auto it = state.assignments.begin(); it != state.assignments.end();)
	{
		const auto& node = it->first;
		if (!node || node->operation != '*')
		{
			it = state.assignments.erase(it);
			continue;
		}

		++it;
	}
}

std::optional<bool> evaluate_from_unknowns(
	const counted_ptr<details::bitstate>& node,
	const std::map<const counted_ptr<details::bitstate>, bool>& assignments,
	std::map<const counted_ptr<details::bitstate>, std::optional<bool>>& memo)
{
	if (!node)
		return std::nullopt;

	const auto memo_it = memo.find(node);
	if (memo_it != memo.end())
		return memo_it->second;

	std::optional<bool> value = std::nullopt;
	const char op = node->operation;

	if (op == '=')
		value = static_cast<bool>(node->state);
	else if (op == '*')
	{
		const auto assignment_it = assignments.find(node);
		if (assignment_it != assignments.end())
			value = assignment_it->second;
	}
	else if (op == '!')
	{
		const auto lhs = evaluate_from_unknowns(node->_1, assignments, memo);
		if (lhs.has_value())
			value = !(*lhs);
	}
	else if (op == '&' || op == '|' || op == '^')
	{
		const auto lhs = evaluate_from_unknowns(node->_1, assignments, memo);
		const auto rhs = evaluate_from_unknowns(node->_2, assignments, memo);
		if (lhs.has_value() && rhs.has_value())
			value = inline_execute(op, *lhs, *rhs);
	}

	memo.emplace(node, value);
	return value;
}

bool solution_matches_target(
	const counted_ptr<details::bitstate>& root,
	bool expected,
	const std::map<const counted_ptr<details::bitstate>, bool>& assignments)
{
	std::map<const counted_ptr<details::bitstate>, std::optional<bool>> memo;
	const auto value = evaluate_from_unknowns(root, assignments, memo);
	return value.has_value() && *value == expected;
}

std::set<crs_state> resolve_bit_collisions(bit_tracker& bit, bool state)
{
	std::deque<crs_state> states;
	std::set<crs_state> solutions;

	states.emplace_back();
	states.back().worklist.emplace_back(bit.bit_state, state, false);

	while (!states.empty())
	{
		auto& crs = states.back();

		if (!solve(crs))
		{
			states.pop_back();
			//std::cout << "Branching failed\n";
			continue;
		}

		if (crs.undecided.empty())
		{
			crs_state solved = std::move(crs);
			states.pop_back();

			if (!solution_matches_target(bit.bit_state, state, solved.assignments))
				continue;

			retain_only_unknown_assignments(solved);

			auto [_, success] = solutions.insert(std::move(solved));

			if (success)
				std::cout << "Solution found, total " << solutions.size() << "\n";

			continue;
		}

		// Incomplete, need to branch.
		// Pick the unresolved parent with the fewest valid branches.
		auto iter = crs.undecided.cbegin();
		size_t best_count = unresolved_branch_count(crs, iter->second);
		for (auto it = std::next(iter); it != crs.undecided.cend(); ++it)
		{
			const size_t candidate_count = unresolved_branch_count(crs, it->second);
			if (candidate_count < best_count)
			{
				best_count = candidate_count;
				iter = it;
			}
		}
		const auto & parent_data = iter->second;

		// Pop the current ambiguous state from the stack.
		crs_state original_state = std::move(states.back());
		states.pop_back();

		if (!parent_data.parent)
			throw std::runtime_error("Parent is null");

		original_state.assignments.erase(parent_data.parent); // force recompute through branch assumptions
		smart_assume(states, std::move(original_state), parent_data);

		//std::cout << "Branched @ " << bit_ptr->max_depth << " depth\n";
	}

	return solutions;
}

using solutions_t = std::set<crs_state>;

// ---------------------------------------------------------------------------
// DPLL-style core.
//
// Differences from resolve_bit_collisions above:
//   * Branches only on the '*' (unknown) leaves, never on intermediate gates.
//   * Boolean constraint propagation (BCP) visits each node at most once per
//     assignment via an explicit work-queue + occurrence lists, instead of
//     re-walking already-settled sub-trees.
//   * Backtracking uses a single mutable state with an undo trail, instead of
//     deep-copying the whole crs_state per branch.
//
// Semantics: enumerates *all complete assignments* of the reachable unknown
// leaves that drive the root to `target` (a don't-care leaf is enumerated in
// both states). Pass first_only=true to stop at the first solution.
// ---------------------------------------------------------------------------
namespace dpll
{

struct engine
{
	using node_t = const details::bitstate*;
	using node_id = size_t;
	using solution_callback = std::function<bool(const crs_state&)>;
	static constexpr node_id no_node = static_cast<node_id>(-1);

	counted_ptr<details::bitstate> root_ptr;
	bool target;
	bool first_only;
	bool collect_solutions;
	solution_callback on_solution;
	bool stop_requested{false};
	size_t solution_count{0};

	node_id root_id{no_node};
	std::vector<counted_ptr<details::bitstate>> nodes;
	std::unordered_map<node_t, node_id> node_ids; // build-time lookup only
	std::vector<std::array<node_id, 2>> inputs;
	std::vector<std::vector<node_id>> parents;
	std::vector<node_id> vars;                // '*' decision leaves
	std::vector<int8_t> values;               // -1 unassigned, 0 false, 1 true
	std::vector<node_id> trail;                // undo log
	std::vector<node_id> propagation_queue;    // reused for every decision

	// Hybrid affine reasoning. UNKNOWN/AND/OR outputs are Boolean atoms;
	// XOR and NOT regions are represented as affine forms over those atoms.
	static constexpr size_t max_affine_atoms = 4096;
	bool affine_enabled{false};
	size_t affine_atom_count{0};
	size_t affine_word_count{0};
	std::vector<node_id> atom_nodes;
	std::vector<node_id> node_atom_columns;
	std::vector<std::uint64_t> affine_coefficients;
	std::vector<std::uint8_t> affine_constants;
	std::vector<std::uint8_t> affine_ready;

	solutions_t solutions;

	engine(
		counted_ptr<details::bitstate> root,
		bool tgt,
		bool first,
		bool collect = true,
		solution_callback callback = {}) :
		root_ptr(std::move(root)),
		target(tgt),
		first_only(first),
		collect_solutions(collect),
		on_solution(std::move(callback)) {}

	int8_t value_of(node_id id) const
	{
		if (id == no_node)
			return -1;

		const auto& node = nodes[id];
		if (node->operation == '=')
			return static_cast<int8_t>(node->state);
		return values[id];
	}

	node_id add_node(const counted_ptr<details::bitstate>& node)
	{
		if (!node)
			return no_node;

		const node_t raw = node.get();
		const auto existing = node_ids.find(raw);
		if (existing != node_ids.end())
			return existing->second;

		const node_id id = nodes.size();
		node_ids.emplace(raw, id);
		nodes.push_back(node);
		inputs.push_back({no_node, no_node});
		parents.emplace_back();
		values.push_back(-1);
		return id;
	}

	void build()
	{
		root_id = add_node(root_ptr);
		for (node_id id = 0; id < nodes.size(); ++id)
		{
			const auto& current = nodes[id];
			const std::uint8_t op = current->operation;
			if (op == '*')
				vars.push_back(id);

			const std::uint8_t argc = details::operation_args_count[op];
			const counted_ptr<details::bitstate>* children[2] =
				{&current->_1, &current->_2};
			for (std::uint8_t i = 0; i < argc; ++i)
			{
				const node_id child = add_node(*children[i]);
				inputs[id][i] = child;
				if (child != no_node)
					parents[child].push_back(id);
			}
		}

		// A high-fanout input participates in more constraints and is more
		// likely to expose a contradiction early.
		std::stable_sort(
			vars.begin(),
			vars.end(),
			[&](node_id lhs, node_id rhs)
			{
				return parents[lhs].size() > parents[rhs].size();
			});

		propagation_queue.reserve(nodes.size());
		trail.reserve(nodes.size());
		initialize_affine_reasoning();
	}

	std::uint64_t* affine_words(node_id id)
	{
		return affine_coefficients.data() + id * affine_word_count;
	}

	const std::uint64_t* affine_words(node_id id) const
	{
		return affine_coefficients.data() + id * affine_word_count;
	}

	void build_affine_form(node_id id)
	{
		if (affine_ready[id] == 1)
			return;
		if (affine_ready[id] == 2)
			throw std::logic_error("Cyclic bit expression");

		affine_ready[id] = 2;
		std::uint64_t* destination = affine_words(id);
		const std::uint8_t op = nodes[id]->operation;

		if (node_atom_columns[id] != no_node)
		{
			const node_id column = node_atom_columns[id];
			destination[column / 64] |=
				std::uint64_t{1} << (column % 64);
		}
		else if (op == '=')
			affine_constants[id] = nodes[id]->state;
		else if (op == '!')
		{
			const node_id lhs = inputs[id][0];
			build_affine_form(lhs);
			std::copy_n(
				affine_words(lhs),
				affine_word_count,
				destination);
			affine_constants[id] = !affine_constants[lhs];
		}
		else if (op == '^')
		{
			const node_id lhs = inputs[id][0];
			const node_id rhs = inputs[id][1];
			build_affine_form(lhs);
			build_affine_form(rhs);

			const std::uint64_t* lhs_words = affine_words(lhs);
			const std::uint64_t* rhs_words = affine_words(rhs);
			for (size_t word = 0; word < affine_word_count; ++word)
				destination[word] = lhs_words[word] ^ rhs_words[word];
			affine_constants[id] =
				affine_constants[lhs] ^ affine_constants[rhs];
		}
		else
			throw std::logic_error("Unsupported affine expression");

		affine_ready[id] = 1;
	}

	void initialize_affine_reasoning()
	{
		size_t affine_gate_count = 0;
		node_atom_columns.assign(nodes.size(), no_node);

		for (node_id id = 0; id < nodes.size(); ++id)
		{
			const std::uint8_t op = nodes[id]->operation;
			if (op == '^' || op == '!')
				++affine_gate_count;

			if (op == '*' || op == '&' || op == '|')
			{
				node_atom_columns[id] = atom_nodes.size();
				atom_nodes.push_back(id);
			}
		}

		affine_atom_count = atom_nodes.size();
		if (affine_gate_count == 0 ||
			affine_atom_count == 0 ||
			affine_atom_count > max_affine_atoms)
			return;

		affine_word_count = (affine_atom_count + 63) / 64;
		affine_coefficients.assign(
			nodes.size() * affine_word_count,
			std::uint64_t{0});
		affine_constants.assign(nodes.size(), 0);
		affine_ready.assign(nodes.size(), 0);

		for (node_id id = 0; id < nodes.size(); ++id)
			build_affine_form(id);

		affine_enabled = true;
	}

	struct affine_row
	{
		std::vector<std::uint64_t> coefficients;
		bool rhs{false};
	};

	bool propagate_affine()
	{
		if (!affine_enabled)
			return true;

		std::vector<affine_row> rows;
		rows.reserve(trail.size());

		for (const node_id id : trail)
		{
			affine_row row;
			row.coefficients.assign(
				affine_words(id),
				affine_words(id) + affine_word_count);
			row.rhs =
				static_cast<bool>(values[id]) ^
				static_cast<bool>(affine_constants[id]);
			rows.push_back(std::move(row));
		}

		size_t rank = 0;
		for (size_t column = 0;
			column < affine_atom_count && rank < rows.size();
			++column)
		{
			const size_t word = column / 64;
			const std::uint64_t bit =
				std::uint64_t{1} << (column % 64);

			size_t pivot = rank;
			while (pivot < rows.size() &&
				!(rows[pivot].coefficients[word] & bit))
				++pivot;
			if (pivot == rows.size())
				continue;

			std::swap(rows[rank], rows[pivot]);
			for (size_t row_index = 0; row_index < rows.size(); ++row_index)
			{
				if (row_index == rank ||
					!(rows[row_index].coefficients[word] & bit))
					continue;

				for (size_t current_word = 0;
					current_word < affine_word_count;
					++current_word)
					rows[row_index].coefficients[current_word] ^=
						rows[rank].coefficients[current_word];
				rows[row_index].rhs =
					rows[row_index].rhs != rows[rank].rhs;
			}
			++rank;
		}

		for (const auto& row : rows)
		{
			size_t set_bits = 0;
			size_t only_column = 0;
			for (size_t word = 0; word < affine_word_count; ++word)
			{
				const size_t word_bits =
					std::popcount(row.coefficients[word]);
				if (word_bits != 0)
				{
					set_bits += word_bits;
					if (set_bits == 1)
						only_column =
							word * 64 +
							std::countr_zero(row.coefficients[word]);
				}
			}

			if (set_bits == 0)
			{
				if (row.rhs)
					return false;
				continue;
			}

			if (set_bits == 1 &&
				!set_value(atom_nodes[only_column], row.rhs))
				return false;
		}

		return true;
	}

	bool set_value(node_id id, bool value)
	{
		const int8_t current = value_of(id);
		if (current != -1)
			return current == static_cast<int8_t>(value);

		values[id] = static_cast<int8_t>(value);
		trail.push_back(id);
		propagation_queue.push_back(id);
		return true;
	}

	// Enforce the local relation of gate g over {g, inputs}. Each supported
	// operation has direct implication rules, avoiding repeated truth-table
	// enumeration in this hot path.
	bool imply(node_id gate)
	{
		const std::uint8_t op = nodes[gate]->operation;
		if (op == '=' || op == '*')
			return true;

		const node_id lhs_id = inputs[gate][0];
		const node_id rhs_id = inputs[gate][1];
		int8_t output = value_of(gate);
		int8_t lhs = value_of(lhs_id);
		int8_t rhs = value_of(rhs_id);

		if (op == '!')
		{
			if (output != -1 && lhs != -1)
				return output == static_cast<int8_t>(!lhs);
			if (output != -1)
				return set_value(lhs_id, !output);
			if (lhs != -1)
				return set_value(gate, !lhs);
			return true;
		}

		if (op == '^')
		{
			if (lhs != -1 && rhs != -1)
				return set_value(gate, lhs != rhs);
			if (output != -1 && lhs != -1)
				return set_value(rhs_id, output != lhs);
			if (output != -1 && rhs != -1)
				return set_value(lhs_id, output != rhs);
			return true;
		}

		if (op == '&')
		{
			if (lhs == 0 || rhs == 0)
			{
				if (!set_value(gate, false))
					return false;
			}
			else if (lhs == 1 && rhs == 1)
			{
				if (!set_value(gate, true))
					return false;
			}

			output = value_of(gate);
			lhs = value_of(lhs_id);
			rhs = value_of(rhs_id);

			if (output == 1)
				return set_value(lhs_id, true) && set_value(rhs_id, true);
			if (output == 0 && lhs == 1)
				return set_value(rhs_id, false);
			if (output == 0 && rhs == 1)
				return set_value(lhs_id, false);
			return true;
		}

		if (op == '|')
		{
			if (lhs == 1 || rhs == 1)
			{
				if (!set_value(gate, true))
					return false;
			}
			else if (lhs == 0 && rhs == 0)
			{
				if (!set_value(gate, false))
					return false;
			}

			output = value_of(gate);
			lhs = value_of(lhs_id);
			rhs = value_of(rhs_id);

			if (output == 0)
				return set_value(lhs_id, false) && set_value(rhs_id, false);
			if (output == 1 && lhs == 0)
				return set_value(rhs_id, true);
			if (output == 1 && rhs == 0)
				return set_value(lhs_id, true);
			return true;
		}

		throw std::logic_error("Unknown gate operation");
	}

	bool propagate()
	{
		size_t cursor = 0;
		while (true)
		{
			while (cursor < propagation_queue.size())
			{
				const node_id id = propagation_queue[cursor++];

				if (!imply(id)) // id as the output of its own gate
					return false;

				for (const node_id parent : parents[id])
					if (!imply(parent)) // id as an input of a parent gate
						return false;
			}

			const size_t previous_trail_size = trail.size();
			if (!propagate_affine())
				return false;
			if (trail.size() == previous_trail_size)
				return true;
		}
	}

	bool assign(node_id id, bool value)
	{
		propagation_queue.clear();
		if (!set_value(id, value))
			return false;
		return propagate();
	}

	void undo_to(size_t mark)
	{
		while (trail.size() > mark)
		{
			values[trail.back()] = -1;
			trail.pop_back();
		}
	}

	void record()
	{
		crs_state s;
		for (const node_id variable : vars)
		{
			const int8_t value = value_of(variable);
			if (value != -1)
				s.assignments[nodes[variable]] = value != 0;
		}

		++solution_count;
		if (on_solution && !on_solution(s))
			stop_requested = true;

		if (collect_solutions)
			solutions.insert(std::move(s));
	}

	bool preferred_phase(node_id variable) const
	{
		size_t false_votes = 0;
		size_t true_votes = 0;

		for (const node_id parent : parents[variable])
		{
			const int8_t output = value_of(parent);
			if (output == -1)
				continue;

			switch (nodes[parent]->operation)
			{
				case '&':
					(output == 1 ? true_votes : false_votes) += 2;
					break;
				case '|':
					(output == 0 ? false_votes : true_votes) += 2;
					break;
				default:
					break;
			}
		}

		return true_votes > false_votes;
	}

	void search()
	{
		if (stop_requested || (first_only && solution_count != 0))
			return;

		node_id pick = no_node;
		for (const node_id variable : vars)
			if (value_of(variable) == -1)
			{
				pick = variable;
				break;
			}

		if (pick == no_node) // every decision leaf is assigned
		{
			record();
			return;
		}

		const bool first_phase = preferred_phase(pick);
		for (const bool phase : {first_phase, !first_phase})
		{
			const size_t mark = trail.size();
			if (assign(pick, phase))
				search();
			undo_to(mark);

			if (stop_requested || (first_only && solution_count != 0))
				return;
		}
	}

	solutions_t run()
	{
		build();
		if (!assign(root_id, target)) // seed the requirement root == target
			return solutions; // unsatisfiable
		search();
		return solutions;
	}
};

inline solutions_t resolve(bit_tracker& bit, bool state, bool first_only = false)
{
	engine e(bit.bit_state, state, first_only);
	return e.run();
}

inline size_t resolve_stream(
	bit_tracker& bit,
	bool state,
	engine::solution_callback on_solution)
{
	engine e(
		bit.bit_state,
		state,
		false,
		false,
		std::move(on_solution));
	(void)e.run();
	return e.solution_count;
}

} // namespace dpll

}

// Update the assert_equality functions to call the resolver
collision_resolution::solutions_t
	assert_equality(ref_handler<bit_tracker> lhs, ref_handler<bit_tracker> rhs, bool first_only = false)
{
	auto is_not_equal = (lhs.ref ^ rhs.ref);
	//details::print_bs(*is_not_equal.bit_state);

	auto solutions = collision_resolution::dpll::resolve(is_not_equal, false, first_only);
	if (solutions.empty())
		throw std::runtime_error("Unsatisfiable constraints");

	return solutions;
}

// Stream complete assignments as they are found without retaining them.
// A void callback continues enumeration. Any other return type is converted
// to bool, where false requests an orderly early stop.
template <typename Callback>
requires std::is_invocable_v<
	std::decay_t<Callback>&,
	const collision_resolution::crs_state&>
size_t assert_equality(
	ref_handler<bit_tracker> lhs,
	ref_handler<bit_tracker> rhs,
	Callback&& callback)
{
	using callback_t = std::decay_t<Callback>;
	using callback_result_t = std::invoke_result_t<
		callback_t&,
		const collision_resolution::crs_state&>;

	auto adapter =
		[handler = callback_t(std::forward<Callback>(callback))]
		(const collision_resolution::crs_state& solution) mutable -> bool
		{
			if constexpr (std::is_void_v<callback_result_t>)
			{
				std::invoke(handler, solution);
				return true;
			}
			else
				return static_cast<bool>(
					std::invoke(handler, solution));
		};

	auto is_not_equal = (lhs.ref ^ rhs.ref);
	const size_t solution_count =
		collision_resolution::dpll::resolve_stream(
			is_not_equal,
			false,
			std::move(adapter));
	if (solution_count == 0)
		throw std::runtime_error("Unsatisfiable constraints");

	return solution_count;
}

template <size_t N>
collision_resolution::solutions_t
	assert_equality(ref_handler<int_tracker<N>> lhs, ref_handler<int_tracker<N>> rhs, bool first_only = false)
{
	bit_tracker result = 0;
	bit_tracker _false(false);

	for (size_t index = 0; index < N; ++index)
		result |= (lhs.ref.bits[index] ^ rhs.ref.bits[index]);

	return assert_equality(result, _false, first_only);
}

template <size_t N, typename Callback>
requires std::is_invocable_v<
	std::decay_t<Callback>&,
	const collision_resolution::crs_state&>
size_t assert_equality(
	ref_handler<int_tracker<N>> lhs,
	ref_handler<int_tracker<N>> rhs,
	Callback&& callback)
{
	bit_tracker result = 0;
	bit_tracker _false(false);

	for (size_t index = 0; index < N; ++index)
		result |= (lhs.ref.bits[index] ^ rhs.ref.bits[index]);

	return assert_equality(
		result,
		_false,
		std::forward<Callback>(callback));
}

void assign_assert_result(ref_handler<bit_tracker, false> value, const std::map<const counted_ptr<details::bitstate>, bool>& assignments)
{
	const auto iter = assignments.find(value.ref.bit_state);
	if (iter == assignments.end())
		return;

	value.ref = iter->second;
}

template <size_t N>
void assign_assert_result(ref_handler<int_tracker<N>, false> value, const std::map<const counted_ptr<details::bitstate>, bool>& assignments)
{
	for (auto& bit_tracker : value.ref.bits)
		assign_assert_result(bit_tracker, assignments);
}

} // namespace bitreverse
} // namespace dixelu

#endif
