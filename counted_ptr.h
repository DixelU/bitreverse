#ifndef DIXELU_COUNTED_PTR_H
#define DIXELU_COUNTED_PTR_H

#include <cstddef>
#include <type_traits>
#include <utility>

#include "buffered_object_pool.h"

namespace dixelu
{

template<typename T>
struct counted_ptr;

template<typename T>
struct counted_control_block
{
	using t_type = std::remove_cvref_t<T>;

	template<typename... Args>
	constexpr explicit counted_control_block(Args&&... args) :
		_p(std::forward<Args>(args)...),
		_c(1)
	{}

	t_type _p;
	std::size_t _c;
};

namespace details
{

template<typename T>
buffered_object_pool<counted_control_block<T>>& counted_control_block_pool()
{
	// counted_ptr itself is non-atomic and intentionally single-threaded, so
	// its allocation path does not pay for locks or atomic synchronization.
	static buffered_object_pool<counted_control_block<T>> pool;
	return pool;
}

} // namespace details

template<typename T>
struct enable_counted_from_this
{
	using t_type = std::remove_cvref_t<T>;

	friend struct counted_ptr<t_type>;

	counted_ptr<t_type> counted_from_this() const
	{
		if (!_ctrl || !_ctrl->_c)
			return {};

		++_ctrl->_c;
		counted_ptr<t_type> result;
		result._base = _ctrl;
		return result;
	}

private:
	void set_control_block(counted_control_block<t_type>* ctrl) const { _ctrl = ctrl; }
	mutable counted_control_block<t_type>* _ctrl = nullptr;
};

template<typename T>
struct counted_ptr
{
	using t_type = std::remove_cvref_t<T>;
	using self_type = counted_ptr<t_type>;

	friend struct enable_counted_from_this<t_type>;

	constexpr counted_ptr() : _base(nullptr) {};
	constexpr counted_ptr(counted_ptr&& p) noexcept :
		_base(p._base)
	{
		p._base = nullptr;
	}

	constexpr counted_ptr(const counted_ptr& p):
		_base(p._base)
	{
		if (_base)
			++_base->_c;
	}

	constexpr ~counted_ptr() { destroy_(); }

	constexpr counted_ptr& operator=(counted_ptr&& p) noexcept
	{
		if (this == &p)
			return *this;

		destroy_();

		_base = p._base;
		p._base = nullptr;

		return *this;
	}

	constexpr counted_ptr& operator=(const counted_ptr& p)
	{
		if (this == &p)
			return *this;

		destroy_();

		_base = p._base;
		if (_base)
			++_base->_c;

		return *this;
	}

	constexpr operator bool() const { return _base; }

	constexpr t_type& operator*() { return _base->_p; }
	constexpr const t_type& operator*() const { return _base->_p; }

	constexpr bool operator<(const counted_ptr& rhs) const { return _base < rhs._base; }
	constexpr bool operator==(const counted_ptr& rhs) const { return _base == rhs._base; }

	constexpr t_type* operator->() { return &_base->_p; }
	constexpr const t_type* operator->() const { return &_base->_p; }

	constexpr void reset() { destroy_(); }

	[[nodiscard]] constexpr std::size_t count() const
	{
		if (_base)
			return _base->_c;
		return 0;
	}

	template<class... Args>
	constexpr static self_type make_counted(Args&&... args)
	{
		self_type ptr;

		if consteval
		{
			ptr._base = new counted_control_block<t_type>(
				std::forward<Args>(args)...);
		}
		else
		{
			ptr._base =
				details::counted_control_block_pool<t_type>().create(
					std::forward<Args>(args)...);
		}

		assign_ctrl_block(ptr);
		return ptr;
	}

	[[nodiscard]] constexpr const t_type* get() const { return _base ? &_base->_p : nullptr; }
	[[nodiscard]] constexpr t_type* get() { return _base ? &_base->_p : nullptr; }

private:
	template<typename Q = t_type>
	constexpr static
	std::enable_if_t<std::is_base_of_v<enable_counted_from_this<Q>, Q>>
	assign_ctrl_block(counted_ptr<Q>& ptr)
	{
		ptr->set_control_block(ptr._base);
	}

	template<typename Q = t_type>
	constexpr static
	std::enable_if_t<!std::is_base_of_v<enable_counted_from_this<Q>, Q>>
	assign_ctrl_block(counted_ptr<Q>&) {}

	constexpr void destroy_()
	{
		if (!_base)
			return;

		auto count = --_base->_c;

		if (!count)
		{
			if consteval
			{
				delete _base;
			}
			else
			{
				details::counted_control_block_pool<t_type>().destroy(_base);
			}
		}

		_base = nullptr;

	}

	counted_control_block<t_type>* _base;
};


template<typename T, class... Args>
constexpr counted_ptr<T> make_counted(Args&&... args)
{
	return counted_ptr<T>::make_counted(std::forward<Args>(args)...);
}

} // namespace dixelu

#endif // DIXELU_COUNTED_PTR_H
