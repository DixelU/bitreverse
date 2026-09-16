#pragma once

#ifndef SPH_BUFFERED_QUEUE_SPSC
#define SPH_BUFFERED_QUEUE_SPSC

#include <atomic>
#include <cstddef>
#include <new>
#include <utility>

// Adapted from SAFC_InnerModules/buffered_queue_spsc.h. The configurable slab
// size and emplace() operation make it suitable as stable-address object
// storage in addition to its original SPSC queue use.
template<
	typename T,
	std::size_t SlabSize = (1u << 15),
	std::size_t MaxRecycledSlabs = 6>
class buffered_queue_spsc
{
	static_assert(SlabSize > 0);

	struct slab
	{
		alignas(64) alignas(T) std::byte data[sizeof(T) * SlabSize];
		T* begin;
		std::atomic<T*> end;
		std::atomic<slab*> next_slab;

		slab() :
			begin(data_begin()),
			end(data_begin()),
			next_slab(nullptr)
		{}

		T* data_begin()
		{
			return reinterpret_cast<T*>(data);
		}

		T* capacity_end()
		{
			return data_begin() + SlabSize;
		}

		bool empty_consumer() const
		{
			return begin == end.load(std::memory_order_acquire);
		}

		bool full_producer()
		{
			return end.load(std::memory_order_relaxed) == capacity_end();
		}

		template<typename... Args>
		T& emplace_producer(Args&&... args)
		{
			T* current_end = end.load(std::memory_order_relaxed);
			::new (current_end) T(std::forward<Args>(args)...);
			end.store(current_end + 1, std::memory_order_release);
			return *current_end;
		}

		void pop_consumer()
		{
			begin->~T();
			++begin;
		}

		void reset_for_reuse()
		{
			begin = data_begin();
			end.store(data_begin(), std::memory_order_relaxed);
			next_slab.store(nullptr, std::memory_order_relaxed);
		}

		void clear_consumer()
		{
			while (!empty_consumer())
				pop_consumer();
		}
	};

	alignas(64) slab* tail_ = nullptr;
	std::size_t pushed_local_ = 0;

	alignas(64) slab* head_ = nullptr;
	std::size_t popped_local_ = 0;

	alignas(64) std::atomic<slab*> recycle_head_{ nullptr };
	alignas(64) std::atomic<std::size_t> recycle_count_{ 0 };
	alignas(64) std::atomic<std::size_t> pushed_{ 0 };
	alignas(64) std::atomic<std::size_t> popped_{ 0 };

	slab* producer_get_slab()
	{
		slab* recycled = recycle_head_.load(std::memory_order_acquire);
		while (recycled)
		{
			slab* next = recycled->next_slab.load(std::memory_order_relaxed);
			if (recycle_head_.compare_exchange_weak(
				recycled,
				next,
				std::memory_order_acquire,
				std::memory_order_relaxed))
			{
				recycle_count_.fetch_sub(1, std::memory_order_relaxed);
				recycled->reset_for_reuse();
				return recycled;
			}
		}
		return new slab();
	}

	void consumer_recycle_slab(slab* reusable)
	{
		if (recycle_count_.load(std::memory_order_relaxed) >= MaxRecycledSlabs)
		{
			delete reusable;
			return;
		}

		slab* old_head = recycle_head_.load(std::memory_order_relaxed);
		do
		{
			reusable->next_slab.store(old_head, std::memory_order_relaxed);
		} while (!recycle_head_.compare_exchange_weak(
			old_head,
			reusable,
			std::memory_order_release,
			std::memory_order_relaxed));
		recycle_count_.fetch_add(1, std::memory_order_relaxed);
	}

	void ensure_initialized_producer()
	{
		if (!tail_)
		{
			tail_ = producer_get_slab();
			head_ = tail_;
		}
	}

public:
	buffered_queue_spsc() = default;

	~buffered_queue_spsc()
	{
		while (head_)
		{
			slab* next = head_->next_slab.load(std::memory_order_relaxed);
			head_->clear_consumer();
			delete head_;
			head_ = next;
		}

		slab* recycled = recycle_head_.load(std::memory_order_relaxed);
		while (recycled)
		{
			slab* next = recycled->next_slab.load(std::memory_order_relaxed);
			delete recycled;
			recycled = next;
		}
	}

	buffered_queue_spsc(const buffered_queue_spsc&) = delete;
	buffered_queue_spsc& operator=(const buffered_queue_spsc&) = delete;

	template<typename... Args>
	T& emplace(Args&&... args)
	{
		ensure_initialized_producer();

		if (tail_->full_producer())
		{
			slab* next = producer_get_slab();
			T& result = next->emplace_producer(std::forward<Args>(args)...);
			tail_->next_slab.store(next, std::memory_order_release);
			tail_ = next;
			pushed_.store(++pushed_local_, std::memory_order_relaxed);
			return result;
		}

		T& result = tail_->emplace_producer(std::forward<Args>(args)...);
		pushed_.store(++pushed_local_, std::memory_order_relaxed);
		return result;
	}

	void push(T&& value)
	{
		emplace(std::move(value));
	}

	T& back()
	{
		return *(tail_->end.load(std::memory_order_relaxed) - 1);
	}

	void pop()
	{
		if (!head_)
			return;

		head_->pop_consumer();
		popped_.store(++popped_local_, std::memory_order_relaxed);
		if (head_->empty_consumer())
		{
			slab* next = head_->next_slab.load(std::memory_order_acquire);
			if (next)
			{
				slab* old = head_;
				head_ = next;
				consumer_recycle_slab(old);
			}
		}
	}

	bool empty() const
	{
		if (!head_)
			return true;
		if (!head_->empty_consumer())
			return false;
		return head_->next_slab.load(std::memory_order_acquire) == nullptr;
	}

	T& front()
	{
		while (head_->empty_consumer())
		{
			slab* next = head_->next_slab.load(std::memory_order_acquire);
			if (!next)
				break;
			slab* old = head_;
			head_ = next;
			consumer_recycle_slab(old);
		}
		return *head_->begin;
	}

	void clear()
	{
		while (head_)
		{
			head_->clear_consumer();
			slab* next = head_->next_slab.load(std::memory_order_relaxed);
			if (next)
			{
				consumer_recycle_slab(head_);
				head_ = next;
			}
			else
			{
				head_->reset_for_reuse();
				tail_ = head_;
				break;
			}
		}

		pushed_local_ = 0;
		popped_local_ = 0;
		pushed_.store(0, std::memory_order_relaxed);
		popped_.store(0, std::memory_order_relaxed);
	}

	std::size_t approximate_size() const
	{
		const std::size_t pushed = pushed_.load(std::memory_order_relaxed);
		const std::size_t popped = popped_.load(std::memory_order_relaxed);
		return pushed > popped ? pushed - popped : 0;
	}
};

#endif
