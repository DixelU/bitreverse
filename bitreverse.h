#ifndef _DIXELU_BITREVERSE_H
#define _DIXELU_BITREVERSE_H

#include <utility>
#include <cinttypes>
#include <type_traits>

namespace dixelu
{
namespace details
{

using byte = unsigned char;

}
/*
struct bit_state
{
	details::byte length: 3;
	details::byte valid_flag: 1;
	details::byte state1: 1;
	details::byte state2: 1;
	details::byte state3: 1;
	details::byte state4: 1;

	bit_state() = default;
	bit_state(bit_state&&) = default;
	bit_state(const bit_state&) = default;
	bit_state& operator=(bit_state&& b)
	{
		length = b.length;
		valid_flag = b.valid_flag;
		state1 = b.state1;
		state2 = b.state2;
		state3 = b.state3;
		state4 = b.state4;
		return *this;
	}
	bit_state& operator=(const bit_state& b)
	{
		length = b.length;
		valid_flag = b.valid_flag;
		state1 = b.state1;
		state2 = b.state2;
		state3 = b.state3;
		state4 = b.state4;
		return *this;
	}

	constexpr bit_state& operator=(bool override)
	{
		length = 1;
		valid_flag = 1;
		state1 = override;
		state2 = override;
		state3 = override;
		state4 = override;
		return *this;
	}

	struct __state_wrapper
	{
		bit_state& state;
		details::byte index;
		constexpr __state_wrapper(
			bit_state& state,
			details::byte index):
			state(state),
			index(index)
		{

		}
		constexpr operator bool()
		{
			switch(index)
			{
				case 0: return state.state1;
				case 1: return state.state2;
				case 2: return state.state3;
				case 3: return state.state4;
			}
			return false;
		}
		__state_wrapper& operator=(bool value)
		{
			switch(index)
			{
				case 0: return state.state1 = value, *this;
				case 1: return state.state2 = value, *this;
				case 2: return state.state3 = value, *this;
				case 3: return state.state4 = value, *this;
			}
		}
	};

	constexpr __state_wrapper operator[](details::byte index)
	{
		return __state_wrapper(*this, index % 4);
	}

	constexpr details::byte size() const
	{
		return length;
	}

	constexpr details::byte serialize() const
	{
		details::byte value;
		value = length; value <<= 3;
		value = valid_flag; value <<= 1;
		value = state1; value <<= 1;
		value = state2; value <<= 1;
		value = state3; value <<= 1;
		value = state4; value <<= 1;
		return value;
	}
};
*/

template<typename T>
struct counted_ptr
{
	struct base
	{
		T _p;
		std::size_t _c;
	};

	counted_ptr(): _base(nullptr) {};
	counted_ptr(counted_ptr<T>&& p):
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

		explicit operation_status()
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
		known_info(make_counted<known_fact>(known_fact::equal, value))
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
};

} // namespace dixelu

#endif