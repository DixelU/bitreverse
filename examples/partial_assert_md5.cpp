#include "partial_assert_demo_support.h"
#include "reference_hashes.h"

#include <array>
#include <bit>
#include <vector>

namespace br = dixelu::bitreverse;
namespace demo = partial_assert_demo;
using u32 = br::int_tracker<32>;
using u128 = br::int_tracker<128>;

namespace
{
constexpr std::uint32_t checkpoint_mask = 0x00007f80U;

u128 pack_digest(const u32& a, const u32& b, const u32& c, const u32& d)
{
	const auto swap_bytes = [](const u32& value)
	{
		return ((value & u32(0xff)) << 24) | ((value & u32(0xff00)) << 8) |
			((value & u32(0xff0000)) >> 8) | ((value & u32(0xff000000)) >> 24);
	};
	return (u128(swap_bytes(a)) << 96) | (u128(swap_bytes(b)) << 64) |
		(u128(swap_bytes(c)) << 32) | u128(swap_bytes(d));
}

std::uint32_t native_word(const demo::bytes& message)
{
	return std::uint32_t(message[0]) | (std::uint32_t(message[1]) << 8) |
		(std::uint32_t(message[2]) << 16) | (std::uint32_t(message[3]) << 24);
}

// A four-byte message with one unknown byte; the checkpoint is b after round 0.
struct md5_example
{
	static constexpr std::string_view name = "MD5";
	static constexpr size_t unknown_bytes = 1;

	static std::uint32_t native_checkpoint(const demo::bytes& message)
	{
		constexpr std::uint32_t a = 0x67452301U, b = 0xefcdab89U, c = 0x98badcfeU, d = 0x10325476U;
		return (b + std::rotl(a + ((b & c) | (~b & d)) + 0xd76aa478U + native_word(message), 7)) & checkpoint_mask;
	}

	static std::array<std::uint32_t, 4> native_digest(const demo::bytes& message)
	{
		return reference::md5_four_bytes(native_word(message));
	}

	static u128 tracked_digest(const std::array<std::uint32_t, 4>& state)
	{
		return pack_digest(u32(state[0]), u32(state[1]), u32(state[2]), u32(state[3]));
	}

	// A separate instrumented copy of the MD5 compression loop, specialized to
	// four message bytes and their single padding block.
	static u128 hash(const std::vector<br::itu8>& message, demo::checkpoint& checkpoint)
	{
		std::vector<u32> words(16);
		words[0] = u32(message[0]) | (u32(message[1]) << 8) |
			(u32(message[2]) << 16) | (u32(message[3]) << 24);
		words[1] = 0x80;
		words[14] = 32;
		const u32 a0 = 0x67452301U, b0 = 0xefcdab89U, c0 = 0x98badcfeU, d0 = 0x10325476U;
		u32 a = a0, b = b0, c = c0, d = d0;
		for (size_t round = 0; round < 64; ++round)
		{
			u32 function;
			size_t word;
			if (round < 16) { function = (b & c) | ((~b) & d); word = round; }
			else if (round < 32) { function = (d & b) | ((~d) & c); word = (5 * round + 1) % 16; }
			else if (round < 48) { function = b ^ c ^ d; word = (3 * round + 5) % 16; }
			else { function = c ^ (b | (~d)); word = (7 * round) % 16; }
			function += a;
			function += u32(reference::md5_constants[round]);
			function += words[word];
			a = d; d = c; c = b;
			const auto shift = reference::md5_shifts[round];
			b += (function << shift) | (function >> (32 - shift));
			// Later rounds reread M[0]; simplifying state alone would leave
			// the original unknown input live through that separate path.
			if (round == 0)
				checkpoint.reach(b & u32(checkpoint_mask), a, b, c, d, words);
		}
		return pack_digest(a0 + a, b0 + b, c0 + c, d0 + d);
	}
};
}

int main()
{
	try
	{
		const demo::bytes known{'a', 'b', 'c', 'd'};
		demo::require(md5_example::native_digest(known) ==
			std::array<std::uint32_t, 4>{0x4c71fce2U, 0x93ee2747U, 0xcd24f395U, 0x1f337f2eU},
			"MD5 native reference failed the abcd check vector");
		const auto observed = md5_example::native_checkpoint(known);
		demo::require_unique_checkpoint<md5_example>(known, observed);
		std::cout << "MD5: message ?bcd, one unknown byte.\n"
			<< "Additional observation: b after round 0 & 0x00007f80 = 0x" << std::hex << observed
			<< std::dec << "; final digest e2fc714c4727ee9395f324cd2e7f331f.\n";
		demo::compare_modes<md5_example>(known, observed);
		std::cout << "Native verification passed; recovered abcd. Node counts are retained DAGs, not peak allocations.\n";
		return 0;
	}
	catch (const std::exception& error)
	{
		std::cerr << "partial_assert_md5: " << error.what() << '\n';
		return 1;
	}
}
