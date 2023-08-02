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
	struct base
	{
		T _p;
		std::size_t _c;
	};

	counted_ptr() : _base(nullptr) {};
	counted_ptr(counted_ptr<T>&& p) noexcept :
		_base(p._base)
	{
		p._base = nullptr;
	}
	counted_ptr(const counted_ptr<T>& p) :
		_base(p._base)
	{
		if (_base)
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

		ptr._base = new typename counted_ptr<Q>::base{
			Q(std::forward<Args>(args)...),
			1 };
		__assign_counted_from_this(ptr);

		return ptr;
	}

private:

	friend enable_counted_from_this<T>;

	template<typename Q = T, class... Args>
	inline static typename std::enable_if<(std::is_base_of<enable_counted_from_this<Q>, Q>::value), void*>::type
		__assign_counted_from_this(counted_ptr<Q>& ptr)
	{
		ptr->__set_weak(ptr);
		return nullptr;
	}

	friend enable_counted_from_this<T>;
	template<typename Q = T, class... Args>
	inline static typename std::enable_if<(!std::is_base_of<enable_counted_from_this<Q>, Q>::value), void*>::type
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

} // namespace dixelu

#endif //* _DIXELU_COUNTED_PTR_H