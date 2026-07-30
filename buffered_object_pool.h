#pragma once

#ifndef DIXELU_BUFFERED_OBJECT_POOL_H
#define DIXELU_BUFFERED_OBJECT_POOL_H

#include <cstddef>
#include <memory>
#include <new>
#include <utility>

namespace dixelu
{
namespace details
{

// A stable-address slab allocator for the project's single-threaded ownership
// model. Unlike buffered_queue_spsc, released objects need not arrive in FIFO
// order: every vacant slot is linked into an intrusive free list.
template<typename T, std::size_t TargetSlabBytes = (1u << 20)>
class buffered_object_pool
{
	static_assert(TargetSlabBytes > 0);

	union slot
	{
		slot* next;
		alignas(T) std::byte storage[sizeof(T)];
	};

	static constexpr std::size_t slots_per_slab =
		TargetSlabBytes / sizeof(slot) == 0
			? 1
			: TargetSlabBytes / sizeof(slot);

	struct slab
	{
		slab* next;
		slot slots[slots_per_slab];
	};

	slab* slabs_ = nullptr;
	slot* free_ = nullptr;
	slot* next_unused_ = nullptr;
	slot* unused_end_ = nullptr;
	std::size_t slab_count_ = 0;
	std::size_t live_count_ = 0;
	std::size_t high_watermark_ = 0;

	void add_slab()
	{
		slab* fresh = new slab;
		fresh->next = slabs_;
		slabs_ = fresh;
		++slab_count_;
		next_unused_ = fresh->slots;
		unused_end_ = fresh->slots + slots_per_slab;
	}

public:
	struct statistics
	{
		std::size_t slabs;
		std::size_t capacity;
		std::size_t live;
		std::size_t high_watermark;
	};

	buffered_object_pool() = default;

	~buffered_object_pool()
	{
		while (slabs_)
		{
			slab* next = slabs_->next;
			delete slabs_;
			slabs_ = next;
		}
	}

	buffered_object_pool(const buffered_object_pool&) = delete;
	buffered_object_pool& operator=(const buffered_object_pool&) = delete;

	template<typename... Args>
	T* create(Args&&... args)
	{
		slot* storage;
		if (free_)
		{
			storage = free_;
			free_ = storage->next;
		}
		else
		{
			if (next_unused_ == unused_end_)
				add_slab();
			storage = next_unused_++;
		}

		try
		{
			T* result = std::construct_at(
				reinterpret_cast<T*>(storage),
				std::forward<Args>(args)...);
			++live_count_;
			if (live_count_ > high_watermark_)
				high_watermark_ = live_count_;
			return result;
		}
		catch (...)
		{
			storage->next = free_;
			free_ = storage;
			throw;
		}
	}

	void destroy(T* object) noexcept
	{
		std::destroy_at(object);

		slot* released = reinterpret_cast<slot*>(object);
		released->next = free_;
		free_ = released;
		--live_count_;
	}

	[[nodiscard]] statistics get_statistics() const noexcept
	{
		return {
			.slabs = slab_count_,
			.capacity = slab_count_ * slots_per_slab,
			.live = live_count_,
			.high_watermark = high_watermark_};
	}
};

} // namespace details
} // namespace dixelu

#endif // DIXELU_BUFFERED_OBJECT_POOL_H
