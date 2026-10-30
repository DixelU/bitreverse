// The slab pool and the counted_ptr ownership built on it.

#include <array>
#include <cstddef>
#include <initializer_list>
#include <set>
#include <utility>
#include <vector>

#include "buffered_object_pool.h"
#include "counted_ptr.h"
#include "test_support.h"

using test::require;

namespace
{

void pool_slabs_and_recycling()
{
	constexpr size_t slots_per_slab = 4;
	dixelu::details::buffered_object_pool<size_t, slots_per_slab * sizeof(size_t)> pool;

	std::array<size_t*, 10> objects;
	for (size_t i = 0; i < objects.size(); ++i)
		objects[i] = pool.create(i);
	const auto grown = pool.get_statistics();
	require(grown.slabs == 3 && grown.capacity == 12 && grown.live == objects.size(),
		"bump allocation must grow across multiple slabs");

	std::set<size_t*> released;
	for (const size_t index : {1, 4, 8})
	{
		released.insert(objects[index]);
		pool.destroy(objects[index]);
		objects[index] = nullptr;
	}
	std::vector<size_t*> replacements;
	for (size_t i = 0; i < released.size(); ++i)
	{
		replacements.push_back(pool.create(100 + i));
		require(released.contains(replacements.back()), "destroyed slots must be recycled before virgin storage");
	}
	require(pool.get_statistics().slabs == grown.slabs, "recycling slots must not allocate another slab");

	for (size_t* object : objects)
		if (object)
			pool.destroy(object);
	for (size_t* replacement : replacements)
		pool.destroy(replacement);
	require(pool.get_statistics().live == 0, "pool must account for every destroyed object");
}

struct probe
{
	static inline size_t live = 0;
	static inline size_t destroyed = 0;

	explicit probe(size_t value) : value(value)
	{
		++live;
	}

	~probe()
	{
		--live;
		++destroyed;
	}

	size_t value;
};

void counted_ptr_pooling()
{
	constexpr size_t object_count = 513;
	auto& pool = dixelu::details::counted_control_block_pool<probe>();
	const auto initial = pool.get_statistics();

	std::vector<dixelu::counted_ptr<probe>> objects;
	for (size_t i = 0; i < object_count; ++i)
		objects.push_back(dixelu::make_counted<probe>(i));
	const auto populated = pool.get_statistics();
	require(populated.slabs == initial.slabs + 1 && populated.live == object_count,
		"counted_ptr must allocate objects in a buffered slab");

	std::set<const probe*> released;
	for (size_t i = 0; i < object_count; i += 3)
	{
		released.insert(objects[i].get());
		objects[i].reset();
	}
	std::vector<dixelu::counted_ptr<probe>> replacements;
	for (size_t i = 0; i < released.size(); ++i)
	{
		replacements.push_back(dixelu::make_counted<probe>(object_count + i));
		require(released.contains(replacements.back().get()), "counted_ptr pool must recycle arbitrary-order releases");
	}
	const auto reused = pool.get_statistics();
	require(reused.slabs == populated.slabs && reused.live == object_count && probe::live == object_count,
		"counted_ptr pool lost track of a live object");

	objects.clear();
	replacements.clear();
	require(pool.get_statistics().live == 0 && probe::live == 0 &&
		probe::destroyed == object_count + released.size(),
		"counted_ptr pool must destroy each object exactly once");
}

struct chain_link
{
	static inline size_t destroyed = 0;

	explicit chain_link(dixelu::counted_ptr<chain_link>&& next) : next(std::move(next))
	{
	}

	~chain_link()
	{
		++destroyed;
	}

	dixelu::counted_ptr<chain_link> next;
};

// Recursive destruction of this chain would overflow the stack.
void iterative_destruction()
{
	constexpr size_t chain_length = 250000;
	auto& pool = dixelu::details::counted_control_block_pool<chain_link>();
	const auto initial = pool.get_statistics();

	dixelu::counted_ptr<chain_link> chain;
	for (size_t i = 0; i < chain_length; ++i)
		chain = dixelu::make_counted<chain_link>(std::move(chain));
	require(pool.get_statistics().live == initial.live + chain_length, "deep ownership chain construction lost nodes");

	chain.reset();
	require(chain_link::destroyed == chain_length && pool.get_statistics().live == initial.live,
		"deep ownership chain must be destroyed iteratively");
}

} // namespace

int main()
{
	return test::run("memory", {
		{"pool_slabs_and_recycling", pool_slabs_and_recycling},
		{"counted_ptr_pooling", counted_ptr_pooling},
		{"iterative_destruction", iterative_destruction},
	});
}
