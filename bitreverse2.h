#ifndef _DIXELU_BITREVERSE2_H
#define _DIXELU_BITREVERSE2_H

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
#include <list>

#include "counted_ptr.h"

#include "btree/set.h"

namespace dixelu::bitreverse2
{

struct __UNKNOWN__ {};

namespace details
{

struct bitsource
{
	enum class state : uint8_t
	{
		FALSE = 0,
		TRUE = 1,
		UNDEF = 255,
	};

	state _state;
};

bitsource::state operator&(bitsource::state lhs, bitsource::state rhs)
{
	switch (lhs)
	{
		case bitsource::state::FALSE:
			return bitsource::state::FALSE;
		case bitsource::state::UNDEF:
			return bitsource::state::UNDEF;
		case bitsource::state::TRUE:
			return rhs;
	}

	// std::unreachable();

	return bitsource::state::UNDEF;
}

bitsource::state operator!(bitsource::state value)
{
	switch (value)
	{
		case bitsource::state::FALSE:
			return bitsource::state::TRUE;
		case bitsource::state::TRUE:
			return bitsource::state::FALSE;
		case bitsource::state::UNDEF:
			return bitsource::state::UNDEF;
	}

	// std::unreachable();

	return bitsource::state::UNDEF;
}

struct bitstate
{
	struct monome
	{
		counted_ptr<bitsource> _source;
		mutable bool _negated;

		inline bool operator<(const monome& rhs) const noexcept
		{
			return _source < rhs._source || (_source == rhs._source && _negated < rhs._negated);
		}

		inline bool static weak_less(const monome& lhs, const monome& rhs) noexcept
		{
			return lhs._source < rhs._source;
		}

		inline bool operator==(const monome& rhs) const noexcept
		{
			return _source == rhs._source && _negated == rhs._negated;
		}
	};

	inline static counted_ptr<bitsource> zero = make_counted<bitsource>(bitsource::state::FALSE);
	inline static counted_ptr<bitsource> one = make_counted<bitsource>(bitsource::state::TRUE);

	using disj = std::vector<monome>;

	btree::set<disj> _conjunction;

	bitstate(counted_ptr<bitsource> source) { init(std::move(source)); }
	bitstate(bool value = false) { init(value ? one : zero); }
	bitstate( __UNKNOWN__ ) { init(make_counted<bitsource>(bitsource::state::UNDEF)); }

	bitstate(const bitstate& rhs) : _conjunction(rhs._conjunction) { }
	bitstate(bitstate&& rhs) noexcept : _conjunction(std::move(rhs._conjunction)) { }

	// (a | b) & (!a | b) = b
	// (a | b) & (!a) = (a & !a) | (b & !a) = (b & !a)

	bitstate& operator&=(const bitstate& rhs)
	{
		// constants optimisation
		bitsource::state rhs_state = bitsource::state::UNDEF;
		bitsource::state lhs_state = bitsource::state::UNDEF;

		bool rhs_is_constant = rhs.is_constant(&rhs_state);
		bool lhs_is_constant = is_constant(&lhs_state);

		if (rhs_is_constant && lhs_is_constant)
		{
			set_constant(rhs_state & lhs_state);
			return *this;
		}

		if (lhs_is_constant)
		{
			if (lhs_state == bitsource::state::TRUE)
				_conjunction = rhs._conjunction;
			return *this;
		}

		if (rhs_is_constant)
		{
			if (rhs_state == bitsource::state::FALSE)
				set_constant(bitsource::state::FALSE);
			return *this;
		}

		std::list<disj> appendable;
		std::map<disj, std::list<disj>> to_replace;

		for (auto& single_rhs_disj: rhs._conjunction)
		{
			for (auto& single_lhs_disj : _conjunction)
			{
				bool rhs_in_lhs = std::includes(
					single_lhs_disj.begin(),
					single_lhs_disj.end(),
					single_rhs_disj.begin(),
					single_rhs_disj.end(),
					monome::weak_less);

				bool lhs_in_rhs = std::includes(
					single_rhs_disj.begin(),
					single_rhs_disj.end(),
					single_lhs_disj.begin(),
					single_lhs_disj.end(),
					monome::weak_less);

				if (!(rhs_in_lhs || lhs_in_rhs))
				{
					appendable.push_back(single_rhs_disj);
					continue;
				}

				if (rhs_in_lhs && lhs_in_rhs)
				{
					disj intersection;
					std::set_intersection(
						single_lhs_disj.begin(),
						single_lhs_disj.end(),
						single_rhs_disj.begin(),
						single_rhs_disj.end(),
						std::back_inserter(intersection));

					if (intersection == single_lhs_disj)
						continue;

//					if (std::equal(
//						single_rhs_disj.begin(),
//						single_rhs_disj.end(),
//						single_lhs_disj.begin(),
//						single_lhs_disj.end(),
//						__weakly_equal))
//					{
						appendable.push_back(single_rhs_disj);
						continue;
//					}

					// this is unreachable?

					auto& replace_candidates = to_replace[single_lhs_disj];
					replace_candidates.push_back(std::move(intersection));

					continue;
				}

				const disj& bigger = rhs_in_lhs ? single_lhs_disj : single_rhs_disj;
				const disj& smaller = lhs_in_rhs ? single_lhs_disj : single_rhs_disj;

				if (std::includes(
					bigger.begin(),
					bigger.end(),
					smaller.begin(),
					smaller.end()))
				{
					if (lhs_in_rhs)
					{
						auto& replace_candidates = to_replace[single_lhs_disj];
						replace_candidates.push_back(single_rhs_disj);
					}
					else
					{
						// do nothing
					}
				}
				else
				{
					// todo: add optimisation here!
					/*
					 *
					auto& replace_candidates = to_replace[single_lhs_disj];
					replace_candidates.push_back(std::move(intersection)); something like this?
					 *
					 * */
					appendable.push_back(single_rhs_disj);
				}
			}
		}

		for (auto& [from, to_list]: to_replace)
		{
			_conjunction.erase(from);
			for (auto& el: to_list)
			{
				if (el.empty())
				{
					set_constant(bitsource::state::FALSE);
					return *this;
				}
				_conjunction.insert(std::move(el));
			}
		}

		for (auto& el: appendable)
			_conjunction.insert(std::move(el));

		return *this;
	}

	// !((a | b) & (c | d) & (e | f | g)) = !(a | b) | !(c | d) | !(e | f | g) =
	// 		(!a & !b) | (!c & !d) | (!e & !f & !g) ...

	[[nodiscard]] bitstate negate() const
	{
		// constants optimisation
		bitsource::state state;
		if (is_constant(&state))
		{
			bitstate new_state;
			new_state.set_constant(!state);
			return new_state;
		}

		std::vector<std::vector<std::vector<monome>>> dnf_clauses;
		dnf_clauses.reserve(_conjunction.size());

		for (const auto& single_clause : _conjunction)
		{
			std::vector<std::vector<monome>> negated_clause;
			negated_clause.reserve(single_clause.size());

			for (auto monome : single_clause)
			{
				monome._negated = !monome._negated;
				negated_clause.push_back({std::move(monome)});
			}
			dnf_clauses.push_back(std::move(negated_clause));
		}

		bitstate result(true), temp;

#ifdef DUMB_OPTIMZER_DELIGATION
		btree::set<std::vector<monome>> proto_conjunction;
		__internal_cartesian_product(dnf_clauses, proto_conjunction);

		for (auto& disj: proto_conjunction)
		{
			temp._conjunction.clear();
			temp._conjunction.insert(std::move(disj));
			result &= temp;
		}
#else
		result._conjunction.clear();
		__internal_cartesian_product(dnf_clauses, result._conjunction);
#endif

		if(result._conjunction.empty())
			result = false;

		result.self_optimizer();

		return result;
	}

	// a | b = !!(a | b) = !(!a & !b)
	inline bitstate operator|(const bitstate& rhs) const
	{
		bitstate neg_rhs = rhs.negate();
		bitstate neg_lhs = negate();

		neg_lhs &= neg_rhs;

		return neg_lhs.negate();
	}

	inline bitstate operator!() const
	{
		return negate();
	}

	inline bitstate operator&(const bitstate& rhs) const
	{
		bitstate state = *this;
		state &= rhs;
		return state;
	}

	inline bitstate& operator|=(const bitstate& rhs)
	{
		auto result = *this | rhs;
		_conjunction = std::move(result._conjunction);
		return *this;
	}

	/*inline bitstate operator^(const bitstate& rhs) const
	{
		auto& lhs = *this;
		return (lhs | rhs) & !(lhs & rhs);
	}*/

	inline bitstate operator^(const bitstate& rhs) const
	{
		auto& lhs = *this;
		auto _or = lhs | rhs;
		auto _and = lhs & rhs;
		auto _nand = !_and;
		auto res = _or & _nand;
		return res;
	}

	// (a )

	inline bitstate operator^=(const bitstate& rhs)
	{
		auto result = *this ^ rhs;
		_conjunction = std::move(result._conjunction);
		return *this;
	}

	inline bitstate& operator=(const bitstate& rhs)
	{
		_conjunction = rhs._conjunction;
		return *this;
	}

	inline bitstate& operator=(bitstate&& rhs)
	{
		_conjunction = std::move(rhs._conjunction);
		return *this;
	}

	inline bitstate operator~() const
	{
		return !*this;
	}

	char __get_representative_char() const
	{
		bitsource::state state = bitsource::state::FALSE;
		if (is_constant(&state))
			return '0' + static_cast<uint8_t>(state);
		if (state == bitsource::state::UNDEF)
			return 'U'; // undef
		return '$'; // is an exression
	}

private:

	static inline bool __strictly_equal(const monome &rhs, const monome &lhs)
	{
		return !(rhs < lhs) && !(lhs < rhs);
	};

	static inline bool __weakly_equal(const monome &rhs, const monome &lhs)
	{
		return !monome::weak_less(rhs, lhs) && !monome::weak_less(lhs, rhs);
	};

	static void __remove_equal(std::vector<monome>& monomes)
	{
		if (monomes.size() < 2)
			return;

		auto current = monomes.begin();
		auto weak_equivalence_end = current + 1;
		auto end = monomes.end();

		while (current < end)
		{
			auto distance = current - monomes.begin();

			while (weak_equivalence_end != end)
			{
				if (!__weakly_equal(*weak_equivalence_end, *current))
					break;

				++weak_equivalence_end;
			}

			auto last_equivalent_element = weak_equivalence_end - 1;

			if (__strictly_equal(*last_equivalent_element, *current))
			{
				if (last_equivalent_element != current && weak_equivalence_end != end)
				{
					std::rotate(current + 1, weak_equivalence_end, end);
					end -= last_equivalent_element - current;
				}
				else if (last_equivalent_element != current)
					end = current + 1;

				++current;
				weak_equivalence_end = current + 1;
			}
			else
			{
				// contradicting variables in an disjunction -> true
				monomes.clear();
				return;
				// if it would be ever needed to just throw em away - uncomment this
				// vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
				// end = std::rotate(current, weak_equivalence_end, end);
			}
		}

		monomes.erase(current, monomes.end());
	}

	// Cartesian product function
	static void __internal_cartesian_product(
		const std::vector<std::vector<std::vector<monome>>>& dnf_clauses,
		btree::set<std::vector<monome>>& result)
	{
		if (dnf_clauses.empty())
			return;

		// Start with the first clause
		result.clear();
		for (auto& el: dnf_clauses.front())
			result.insert(el);

		// Iteratively expand the product
		for (size_t i = 1; i < dnf_clauses.size(); ++i)
		{
			btree::set<std::vector<monome>> temp_result;
			for (const auto& existing_clause : result)
			{
				for (const auto& new_clause : dnf_clauses[i])
				{
					// Combine clauses
					std::vector<monome> combined_clause = existing_clause;
					combined_clause.insert(
						combined_clause.end(),
						new_clause.begin(),
						new_clause.end());

					// Sort and remove duplicates
					std::sort(
						combined_clause.begin(),
						combined_clause.end());

					__remove_equal(combined_clause);

					/*auto it = std::unique(combined_clause.begin(), combined_clause.end());
					if (it != combined_clause.end())
						combined_clause.erase(it, combined_clause.end());*/

					if (!combined_clause.empty())
						temp_result.insert(std::move(combined_clause));
				}
			}
			result = std::move(temp_result);
		}
	}

	void self_optimizer()
	{
		//todo: implement later i guess
	}

	bool is_constant(bitsource::state* state_out_ptr = nullptr) const
	{
		if (_conjunction.size() != 1)
			return false;

		if (_conjunction.begin()->size() != 1)
			return false;

		if (state_out_ptr)
			*state_out_ptr = _conjunction.begin()->front()._source->_state;

		return
			_conjunction.begin()->front()._source->_state != bitsource::state::UNDEF;
	}

	void set_constant(bitsource::state state)
	{
		_conjunction.clear();

		init(state == bitsource::state::FALSE ? zero : one);
	}

	void init(counted_ptr<bitsource> bit)
	{
		_conjunction.insert({monome{ ._source = std::move(bit), ._negated = false } });
	}
};

inline bitstate execute_ternary_operation(
	const bitstate& source,
	const bitstate& val1,
	const bitstate& val2)
{
	return ((!source) & val2) | (source & val1);
}

} // details

__UNKNOWN__ unknown;

template<size_t N>
struct int_tracker
{
	using bit_tracker = details::bitstate;

	using self_type = int_tracker<N>;
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

	template<typename convertable_to_int>
	int_tracker(
		typename std::enable_if<
			std::is_convertible<convertable_to_int, std::uintmax_t>::value,
			convertable_to_int>::type maxint_value) :
		int_tracker((std::uintmax_t)maxint_value)
	{
	}

	template<size_t Q>
	int_tracker(const int_tracker<Q>& rhs)
	{
		auto rhs_rit = rhs.bits.crbegin();
		auto this_rit = bits.rbegin();

		for (; rhs_rit != rhs.bits.crend() && this_rit != bits.rend(); ++rhs_rit, ++this_rit)
			*this_rit = *rhs_rit;
	}

	int_tracker(bit_tracker bit):
		int_tracker()
	{
		bits.back() = std::move(bit);
	}

	int_tracker(self_type&&) = default;
	int_tracker(const self_type&) = default;
	int_tracker& operator=(const self_type& rhs)
	{
		for (size_t i = 0; i < N; ++i)
			bits[i] = rhs.bits[i];
		return *this;
	}

	explicit int_tracker(std::array<bit_tracker, N>&& bits):
		bits(std::move(bits)) { }

	self_type& operator=(self_type&& rhs)
	{
		for (size_t i = 0; i < N; ++i)
			bits[i] = std::move(rhs.bits[i]);
		return *this;
	}

	self_type& operator|=(const self_type& rhs)
	{
		for (size_t i = 0; i < N; ++i)
			bits[i] |= rhs.bits[i];
		return *this;
	}

	self_type& operator&=(const self_type& rhs)
	{
		for (size_t i = 0; i < N; ++i)
			bits[i] &= rhs.bits[i];
		return *this;
	}

	self_type& operator^=(const self_type& rhs)
	{
		for (size_t i = 0; i < N; ++i)
			bits[i] ^= rhs.bits[i];
		return *this;
	}

	self_type operator|(const self_type& rhs) const
	{
		self_type copy = *this;
		copy |= rhs;
		return copy;
	}

	self_type operator&(const self_type& rhs) const
	{
		self_type copy = *this;
		copy &= rhs;
		return copy;
	}

	self_type operator^(const self_type& rhs) const
	{
		self_type copy = *this;
		copy ^= rhs;
		return copy;
	}

	self_type operator~() const
	{
		self_type value = *this;
		for (size_t i = 0; i < N; ++i)
			value.bits[i] = !value.bits[i];
		return value;
	}

	self_type& operator=(__UNKNOWN__ unknown_rhs)
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

	bit_tracker operator!() const
	{
		return !(bit_tracker)*this;
	}

	self_type operator<<(size_t shift) const
	{
		self_type value = *this;
		value <<= shift;
		return value;
	}

	self_type& operator<<=(size_t shift)
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

	self_type operator>>(size_t shift) const
	{
		self_type value = *this;
		value >>= shift;
		return value;
	}

	self_type& operator>>=(size_t shift)
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

	static self_type __execute_ternary_assign(
		const bit_tracker& condition,
		const self_type& lhs,
		const self_type& rhs)
	{
		self_type t;
		for (size_t i = 0; i < N; ++i)
			t.bits[i] = execute_ternary_operation(condition, lhs.bits[i], rhs.bits[i]);
		return t;
	}

	self_type& operator>>=(self_type& shift)
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

	self_type& operator<<=(self_type& shift)
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

	self_type& operator+=(const self_type& rhs)
	{
		bit_tracker carry = false;
		for (size_t i = 0; i < N; ++i)
		{
			auto& lhs_bit = bits[N - 1 - i];
			auto& rhs_bit = rhs.bits[N - 1 - i];

			auto tmp_xor = lhs_bit ^ rhs_bit;
			auto xor_bit = tmp_xor ^ carry;
			carry = (rhs_bit & carry & !lhs_bit) | (lhs_bit & (rhs_bit | carry));
			lhs_bit = xor_bit;
		}
		return *this;
	}

	self_type& operator-=(const self_type& rhs)
	{
		auto rhs_complement = (~rhs) + 1;
		return (*this += rhs_complement);
	}

	self_type operator+(const self_type& rhs) const
	{
		self_type lhs = *this;
		lhs += rhs;
		return lhs;
	}

	self_type operator-(const self_type& rhs) const
	{
		self_type lhs = *this;
		lhs -= rhs;
		return lhs;
	}

	self_type operator-() const
	{
		const self_type one = 1;
		auto negated_this = this->operator~();
		auto rhs_complement = negated_this + one;
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

	std::pair<size_t, size_t> __max_depth() const
	{
		size_t max_depth = 0;
		size_t max_width = 0;
		for (auto& bit : bits)
		{
			max_width = std::max<size_t>(max_depth, bit._conjunction.size());
			for (auto& disj: bit._conjunction)
				max_depth = std::max(max_width, disj.size());
		}
		return {max_depth, max_width};
	}

	std::string __max_depth_str() const
	{
		auto [depth, width] = __max_depth();
		return "[" + std::to_string(depth) + ", " + std::to_string(width) + "]";
	}
};

using itu8 = int_tracker<8>;
using itu16 = int_tracker<16>;
using itu32 = int_tracker<32>;
using itu64 = int_tracker<64>;

} //  dixelu::bitreverse2


#endif //_DIXELU_BITREVERSE2_H
