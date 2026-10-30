#include "partial_assert_demo_support.h"
#include "reference_hashes.h"

#include <string>
#include <vector>

namespace br = dixelu::bitreverse;
namespace demo = partial_assert_demo;
using u32 = br::int_tracker<32>;

namespace
{
constexpr std::uint32_t checkpoint_mask = 0xffff0000U;

// The CRC register, before the final complement, after `count` bytes.
std::uint32_t native_state(const demo::bytes& message, size_t count)
{
	std::uint32_t crc = 0xffffffffU;
	for (size_t index = 0; index < count; ++index) crc = reference::crc32_update(crc, message[index]);
	return crc;
}

// Two unknown prefix bytes; the checkpoint is the register after them.
struct crc32_example
{
	static constexpr std::string_view name = "CRC32";
	static constexpr size_t unknown_bytes = 2;

	static std::uint32_t native_checkpoint(const demo::bytes& message)
	{
		return native_state(message, unknown_bytes) & checkpoint_mask;
	}

	static std::uint32_t native_digest(const demo::bytes& message)
	{
		return ~native_state(message, message.size());
	}

	static u32 tracked_digest(std::uint32_t digest) { return u32(digest); }

	// Deliberately separate from crc32.h: the checkpoint is inside the byte loop.
	static u32 hash(const std::vector<br::itu8>& message, demo::checkpoint& checkpoint)
	{
		u32 crc = 0xffffffffU;
		for (size_t index = 0; index < message.size(); ++index)
		{
			crc ^= u32(message[index]);
			for (unsigned bit = 0; bit < 8; ++bit)
			{
				const auto mask = -(crc & u32(1));
				crc = (crc >> 1) ^ (u32(0xedb88320U) & mask);
			}
			if (index + 1 == unknown_bytes)
				checkpoint.reach(crc & u32(checkpoint_mask), crc);
		}
		return ~crc;
	}
};
}

int main()
{
	try
	{
		demo::require(reference::crc32("123456789") == 0xcbf43926U,
			"CRC32 native reference failed its standard check vector");
		const std::string text = "AB/checkpoints";
		const demo::bytes known(text.begin(), text.end());
		const auto observed = crc32_example::native_checkpoint(known);
		demo::require_unique_checkpoint<crc32_example>(known, observed);
		std::cout << "CRC32: two unknown prefix bytes, known suffix /checkpoints.\n"
			<< "Additional observation: internal CRC after byte 2 & 0xffff0000 = 0x" << std::hex << observed
			<< "; final digest = 0x" << crc32_example::native_digest(known) << std::dec << ".\n";
		demo::compare_modes<crc32_example>(known, observed);
		std::cout << "Native verification passed; recovered prefix AB. Node counts are retained DAGs, not peak allocations.\n";
		return 0;
	}
	catch (const std::exception& error)
	{
		std::cerr << "partial_assert_crc32: " << error.what() << '\n';
		return 1;
	}
}
