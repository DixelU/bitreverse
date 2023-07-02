#ifndef _DIXELU_BITREVERSE_H
#define _DIXELU_BITREVERSE_H

#include <vector>
#include <utility>
#include <optional>
#include <cinttypes>
#include <type_traits>

namespace dixelu
{

template<typename T>
struct counted_ptr;

template<typename T>
struct enable_counted_from_this
{
public:
	enable_counted_from_this() {}
	~enable_counted_from_this() { _weak._base = nullptr; }
	friend struct counted_ptr<T>;
	counted_ptr<T> counted_from_this() const { return _weak.count() ? _weak : counted_ptr<T>{}; }
private:
	void __set_weak(counted_ptr<T>& ptr) const { _weak._base = ptr._base; }
	mutable counted_ptr<T> _weak;
};

template<typename T>
struct counted_ptr
{
	struct base
	{
		T _p;
		std::size_t _c;
	};

	counted_ptr(): _base(nullptr) {};
	counted_ptr(counted_ptr<T>&& p) noexcept :
		_base(p._base)
	{
		p._base = nullptr;
	}
	counted_ptr(const counted_ptr<T>& p):
		_base(p._base)
	{
		if(_base)
			_base->_c++;
	}

	~counted_ptr() { __destroy(); }

	counted_ptr<T>& operator=(counted_ptr<T>&& p)
	{
		if (p._base == _base)
			return *this;

		__destroy();
		_base = p._base;
		p._base = nullptr;
		return *this;
	}

	counted_ptr<T>& operator=(const counted_ptr<T>& p)
	{
		if (p._base == _base)
			return *this;

		__destroy();
		_base = p._base;
		_base->_c++;
		return *this;
	}

	operator bool() const
	{
		return _base;
	}

	T& operator*()
	{
		return _base->_p;
	}

	const T& operator*() const
	{
		return _base->_p;
	}

	T* operator->()
	{
		return &_base->_p;
	}

	const T* operator->() const
	{
		return &_base->_p;
	}

	void reset()
	{
		__destroy();
	}

	std::size_t count() const
	{
		if (_base)
			return _base->_c;
		return 0;
	}

	template<typename Q>
	typename std::enable_if<(std::is_base_of<Q, T>::value || std::is_base_of<T, Q>::value), counted_ptr<Q>>::type cast()
	{
		return counted_ptr<Q>();
	}

	template<typename Q = T, class... Args>
	inline static counted_ptr<Q> __make_counted(Args... args)
	{
		counted_ptr<Q> ptr;

		ptr._base = new counted_ptr<Q>::base{
			Q(std::forward<Args>(args)...),
			1 };
		__assign_counted_from_this(ptr);

		return ptr;
	}

private:

	friend enable_counted_from_this<T>;

	template<typename Q = T, class... Args>
	inline static std::enable_if<(std::is_base_of<enable_counted_from_this<Q>, Q>::value), void*>::type
		__assign_counted_from_this(counted_ptr<Q>& ptr)
	{
		ptr->__set_weak(ptr);
		return nullptr;
	}

	friend enable_counted_from_this<T>;
	template<typename Q = T, class... Args>
	inline static std::enable_if<(!std::is_base_of<enable_counted_from_this<Q>, Q>::value), void*>::type
		__assign_counted_from_this(counted_ptr<Q>& ptr) { return nullptr; }

	void __destroy()
	{
		if (_base)
		{
			auto& count = --_base->_c;
			if (!count)
				delete _base;
			_base = nullptr;
		}
	}

	base* _base;
};

template<typename T, class... Args>
counted_ptr<T> make_counted(Args&&... args)
{
	return counted_ptr<T>::__make_counted(std::forward<Args>(args)...);
}

struct bit_states_tracker:
	protected enable_counted_from_this<bit_states_tracker>
{
	struct operation_status
	{
		enum operation
		{
			/*ohnary operation*/
			nul_op,
			/*binary opearations 1, 2*/
			and_op, or_op, xor_op, swap_op,
			/*unary operation 1,*/
			not_op,
			/* ternary operation if(1) this = 2, else this = 3 */ 
			/* is this reversible? */
			cond_move
		};

		counted_ptr<bit_states_tracker> state1;
		counted_ptr<bit_states_tracker> state2;
		counted_ptr<bit_states_tracker> state3;
		operation op;

		void reset()
		{
			state1.reset();
			state2.reset();
			state3.reset();
			op = nul_op;
		}
	};

	struct known_fact
	{
		enum fact_type
		{
			equal
		};

		struct fact_data
		{
			bool value;
		};

		fact_type type;
		fact_data data;

		static known_fact is(bool value)
		{
			return { .type = equal, .data = {.value = value} };
		}
	};

private:
	counted_ptr<operation_status> last_operation;
	std::vector<counted_ptr<known_fact>> known_info;
public:

	explicit bit_states_tracker(bool value = false):
		known_info(make_counted<known_fact>(known_fact::is(value)))
	{
	}

	bit_states_tracker(bit_states_tracker&&) = default;
	bit_states_tracker(const bit_states_tracker&) = default;

	bit_states_tracker& operator=(const bit_states_tracker& value)
	{
		known_info = value.known_info;
		last_operation = value.last_operation;
		return *this;
	}

	bit_states_tracker& operator=(bit_states_tracker&& value)
	{
		known_info = std::move(value.known_info);
		last_operation = std::move(value.last_operation);
		return *this;
	}

	bit_states_tracker operator|(const bit_states_tracker& value)
	{
		bit_states_tracker bit_result;

		auto rhs_value = __get_known_fact_equal(*this);
		auto lhs_value = __get_known_fact_equal(value);
		if (rhs_value && lhs_value)
			bit_result.known_info.push_back(
				make_counted<known_fact>(
					known_fact::is(rhs_value.value() || lhs_value.value())));

		bit_result.last_operation = make_counted<operation_status>(
			counted_from_this(), 
			value.counted_from_this(),
			counted_ptr<bit_states_tracker>{},
			operation_status::or_op);

		return bit_result;
	}

private:

	inline static std::optional<bool> __get_known_fact_equal(const bit_states_tracker& value)
	{
		for (auto& fact : value.known_info)
			if (fact->type == known_fact::equal)
				return fact->data.value;
		return {};
	}

};

} // namespace dixelu

#endif