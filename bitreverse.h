#ifndef _DIXELU_BITREVERSE_H
#define _DIXELU_BITREVERSE_H

#include <utility>
#include <cinttypes>
#include <type_traits>

namespace dixelu
{

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

	template<class... Args>
	inline static counted_ptr<T> __make_counted(Args... args)
	{
		counted_ptr<T> ptr;

		ptr._base = new counted_ptr<T>::base{
			T(std::forward<Args...>(args...)),
			1 };

		return ptr;
	}

	template<typename U>
	friend struct enable_counted_from_this;

private:
	

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

template<typename T>
struct enable_counted_from_this
{
	enable_counted_from_this()
	{

	}
	~enable_counted_from_this()
	{
		_weak._base = nullptr;
	}
private:
	counted_ptr<T> _weak;
};

struct bit_states_tracker
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
			/* is this reverible? */
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

		union fact_data
		{
			bool value;
		};

		fact_type type;
		fact_data data;
	};

	counted_ptr<operation_status> last_operation;
	counted_ptr<known_fact> known_info;

	explicit bit_states_tracker(bool value = false):
		known_info(make_counted<known_fact>(known_fact{ known_fact::equal, value }))
	{
	}

	bit_states_tracker& operator=(const bit_states_tracker& value)
	{
		known_info = value.known_info;
		last_operation = value.last_operation;
	}

	bit_states_tracker& operator=(bit_states_tracker&& value)
	{
		known_info = value.known_info;
		last_operation = value.last_operation;
		value.known_info.reset();
		value.last_operation.reset();
	}

	bit_states_tracker& operator|(const bit_states_tracker & value)
	{

	}


};

} // namespace dixelu

#endif