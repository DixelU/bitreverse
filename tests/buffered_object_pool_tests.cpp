#include <array>
#include <cstddef>
#include <set>
#include <stdexcept>

#include "buffered_object_pool.h"

namespace
{

void require(bool condition, const char* message)
{
	if (!condition)
		throw std::runtime_error(message);
}

} // namespace

int main()
{
	constexpr std::size_t slots_per_test_slab = 4;
	dixelu::details::buffered_object_pool<
		std::size_t,
		slots_per_test_slab * sizeof(std::size_t)> pool;

	std::array<std::size_t*, 10> objects;
	for (std::size_t i = 0; i < objects.size(); ++i)
		objects[i] = pool.create(i);

	const auto grown = pool.get_statistics();
	require(
		grown.slabs == 3 &&
			grown.capacity == 12 &&
			grown.live == objects.size(),
		"bump allocation must grow across multiple slabs");

	constexpr std::array<std::size_t, 3> released_indices{1, 4, 8};
	std::set<std::size_t*> released_addresses;
	for (const std::size_t index : released_indices)
	{
		released_addresses.insert(objects[index]);
		pool.destroy(objects[index]);
		objects[index] = nullptr;
	}

	std::array<std::size_t*, released_indices.size()> replacements;
	for (std::size_t i = 0; i < replacements.size(); ++i)
	{
		replacements[i] = pool.create(100 + i);
		require(
			released_addresses.contains(replacements[i]),
			"destroyed slots must be recycled before virgin storage");
	}

	require(
		pool.get_statistics().slabs == grown.slabs,
		"recycling slots must not allocate another slab");

	for (std::size_t* object : objects)
	{
		if (object)
			pool.destroy(object);
	}
	for (std::size_t* replacement : replacements)
		pool.destroy(replacement);

	require(
		pool.get_statistics().live == 0,
		"pool must account for every destroyed object");
}
