#ifndef _DIXELU_COUNTED_PTR_H
#define _DIXELU_COUNTED_PTR_H

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
	using t_type = std::remove_cvref<T>::type;

	struct base
	{
		t_type _p;
		std::size_t _c;
	};

	using self_type = counted_ptr<t_type>;

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

	constexpr ~counted_ptr() { __destroy(); }

	constexpr counted_ptr& operator=(counted_ptr&& p)
	{
		if (p._base == _base)
			return *this;

		__destroy();
		_base = p._base;
		p._base = nullptr;
		return *this;
	}

	constexpr counted_ptr& operator=(const counted_ptr& p)
	{
		if (p._base == _base)
			return *this;

		__destroy();
		_base = p._base;
		++_base->_c;
		return *this;
	}

	constexpr operator bool() const
	{
		return _base;
	}

	constexpr t_type& operator*()
	{
		return _base->_p;
	}

	constexpr const t_type& operator*() const
	{
		return _base->_p;
	}

	constexpr bool operator<(const counted_ptr& rhs) const
	{
		return _base < rhs._base;
	}

	constexpr bool operator==(const counted_ptr& rhs) const
	{
		return _base == rhs._base;
	}

	constexpr t_type* operator->()
	{
		return &_base->_p;
	}

	constexpr const t_type* operator->() const
	{
		return &_base->_p;
	}

	constexpr void reset()
	{
		__destroy();
	}

	[[nodiscard]] constexpr std::size_t count() const
	{
		if (_base)
			return _base->_c;
		return 0;
	}

	template<typename Q>
	constexpr typename std::enable_if<
	    (std::is_base_of<Q, t_type>::value || std::is_base_of<t_type, Q>::value),
			counted_ptr<Q>>::type cast()
	{
		return counted_ptr<Q>();
	}

	template<class... Args>
	inline constexpr static self_type __make_counted(Args... args)
	{
		self_type ptr;

		ptr._base = new typename self_type::base{
			t_type(std::forward<Args>(args)...),
			1 };
		__assign_counted_from_this(ptr);

		return ptr;
	}

	[[nodiscard]] constexpr const T* get() const { return _base ? &_base->_p : nullptr; }
	[[nodiscard]] constexpr T* get() { return _base ? &_base->_p : nullptr; }

private:

	friend enable_counted_from_this<t_type>;

	template<typename Q = t_type, class... Args>
	inline constexpr static typename std::enable_if<(std::is_base_of<enable_counted_from_this<Q>, Q>::value), void*>::type
		__assign_counted_from_this(counted_ptr<Q>& ptr)
	{
		ptr->__set_weak(ptr);
		return nullptr;
	}

	template<typename Q = t_type, class... Args>
	inline constexpr static typename std::enable_if<(!std::is_base_of<enable_counted_from_this<Q>, Q>::value), void*>::type
		__assign_counted_from_this(counted_ptr<Q>&) { return nullptr; }

	constexpr void __destroy()
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
constexpr counted_ptr<T> make_counted(Args&&... args)
{
	return counted_ptr<T>::__make_counted(std::forward<Args>(args)...);
}

} // namespace dixelu

#endif //* _DIXELU_COUNTED_PTR_H