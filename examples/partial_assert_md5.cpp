#include "partial_assert_demo_support.h"
#include "benchmark_native_md5.h"

#include <array>
#include <bit>

namespace br = dixelu::bitreverse;
namespace demo = partial_assert_demo;
using u32 = br::int_tracker<32>;
using u128 = br::int_tracker<128>;

namespace
{
constexpr std::uint32_t checkpoint_mask = 0x00007f80U;
constexpr std::array<unsigned, 64> shifts{
	7,12,17,22,7,12,17,22,7,12,17,22,7,12,17,22,
	5,9,14,20,5,9,14,20,5,9,14,20,5,9,14,20,
	4,11,16,23,4,11,16,23,4,11,16,23,4,11,16,23,
	6,10,15,21,6,10,15,21,6,10,15,21,6,10,15,21};
constexpr std::array<std::uint32_t, 64> constants{
	0xd76aa478,0xe8c7b756,0x242070db,0xc1bdceee,0xf57c0faf,0x4787c62a,0xa8304613,0xfd469501,
	0x698098d8,0x8b44f7af,0xffff5bb1,0x895cd7be,0x6b901122,0xfd987193,0xa679438e,0x49b40821,
	0xf61e2562,0xc040b340,0x265e5a51,0xe9b6c7aa,0xd62f105d,0x02441453,0xd8a1e681,0xe7d3fbc8,
	0x21e1cde6,0xc33707d6,0xf4d50d87,0x455a14ed,0xa9e3e905,0xfcefa3f8,0x676f02d9,0x8d2a4c8a,
	0xfffa3942,0x8771f681,0x6d9d6122,0xfde5380c,0xa4beea44,0x4bdecfa9,0xf6bb4b60,0xbebfbc70,
	0x289b7ec6,0xeaa127fa,0xd4ef3085,0x04881d05,0xd9d4d039,0xe6db99e5,0x1fa27cf8,0xc4ac5665,
	0xf4292244,0x432aff97,0xab9423a7,0xfc93a039,0x655b59c3,0x8f0ccc92,0xffeff47d,0x85845dd1,
	0x6fa87e4f,0xfe2ce6e0,0xa3014314,0x4e0811a1,0xf7537e82,0xbd3af235,0x2ad7d2bb,0xeb86d391};

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

std::uint32_t native_word(const std::vector<std::uint8_t>& message)
{
	return std::uint32_t(message[0]) | (std::uint32_t(message[1]) << 8) |
		(std::uint32_t(message[2]) << 16) | (std::uint32_t(message[3]) << 24);
}

std::uint32_t native_checkpoint(std::uint32_t word)
{
	constexpr std::uint32_t a = 0x67452301U, b = 0xefcdab89U, c = 0x98badcfeU, d = 0x10325476U;
	return (b + std::rotl(a + ((b & c) | (~b & d)) + 0xd76aa478U + word, 7)) & checkpoint_mask;
}

// A separate instrumented copy of the MD5 compression loop, specialized to
// four message bytes and their single padding block. md5.h is unchanged.
u128 instrumented_md5(const std::vector<br::itu8>& message, br::partial_assert_context& context,
	bool eager, std::uint32_t observed, br::bit_tracker& checkpoint, size_t& known_after_checkpoint)
{
	demo::require(message.size() == 4, "MD5 example requires exactly four message bytes");
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
		function += u32(constants[round]);
		function += words[word];
		a = d; d = c; c = b;
		b += (function << shifts[round]) | (function >> (32 - shifts[round]));
		if (round == 0)
		{
			checkpoint = u32::are_equal(b & u32(checkpoint_mask), u32(observed));
			if (eager)
			{
				context.partial_assert(checkpoint);
				known_after_checkpoint = context.known_variable_count();
				a = context.simplify(a); b = context.simplify(b);
				c = context.simplify(c); d = context.simplify(d);
				// Later rounds reread M[0]; simplifying state alone would leave
				// the original unknown input live through that separate path.
				words = context.simplify(words);
			}
		}
	}
	return pack_digest(a0 + a, b0 + b, c0 + c, d0 + d);
}

demo::result run(bool eager, const std::vector<std::uint8_t>& known,
	std::uint32_t observed, const std::array<std::uint32_t, 4>& expected)
{
	demo::result result;
	const auto started = demo::clock::now();
	br::partial_assert_context context;
	std::vector<br::itu8> message{br::itu8(br::unknown), br::itu8(known[1]), br::itu8(known[2]), br::itu8(known[3])};
	std::vector<br::bit_tracker> inputs;
	demo::append_bits(inputs, message.front());
	br::bit_tracker checkpoint;
	const auto digest = instrumented_md5(message, context, eager, observed, checkpoint, result.known_after_checkpoint);
	if (!eager)
	{
		context.partial_assert(checkpoint);
		result.known_after_checkpoint = context.known_variable_count();
	}
	context.partial_assert(digest, pack_digest(u32(expected[0]), u32(expected[1]), u32(expected[2]), u32(expected[3])));
	result.build_and_assert_ms = demo::milliseconds(demo::clock::now() - started);
	result.digest_nodes = demo::node_count(digest);
	auto retained = context.requirements();
	retained.insert(retained.end(), inputs.begin(), inputs.end());
	demo::append_bits(retained, digest);
	result.retained_nodes = demo::node_count(retained);
	result.assertions = context.assertion_count();
	br::solver_options solve_options;
	solve_options.max_search_steps = 2000000;
	br::solver_statistics stats;
	const auto solve_started = demo::clock::now();
	result.solutions = context.solve(inputs, [&](const br::collision_resolution::crs_state& model)
	{
		auto recovered = known;
		recovered[0] = demo::decoded_byte(message.front(), model);
		demo::require(native_checkpoint(native_word(recovered)) == observed,
			"MD5 recovered input violates the native checkpoint");
		demo::require(benchmark_reference::md5_four_bytes(native_word(recovered)) == expected,
			"MD5 recovered input fails independent native verification");
		demo::require(recovered == known, "MD5 recovered a different input despite unique checkpoint");
		result.recovered = std::move(recovered);
		return true;
	}, 2, solve_options, &stats);
	result.final_solve_ms = demo::milliseconds(demo::clock::now() - solve_started);
	result.solver_decisions = stats.decisions;
	result.solver_steps = stats.search_steps;
	demo::require(result.solutions == 1, "MD5 constraints did not have exactly one solution");
	return result;
}
}

int main()
{
	try
	{
		const std::vector<std::uint8_t> known{'a', 'b', 'c', 'd'};
		const auto observed = native_checkpoint(native_word(known));
		const auto digest = benchmark_reference::md5_four_bytes(native_word(known));
		demo::require(digest == std::array<std::uint32_t, 4>{0x4c71fce2U, 0x93ee2747U, 0xcd24f395U, 0x1f337f2eU},
			"MD5 native reference failed the abcd check vector");
		// Check uniqueness independently; these native trials are not supplied
		// to the context and are outside the printed construction timings.
		size_t matching_bytes = 0;
		for (unsigned byte = 0; byte < 256; ++byte)
			if (native_checkpoint(0x64636200U | byte) == observed) ++matching_bytes;
		demo::require(matching_bytes == 1, "MD5 masked checkpoint is not unique in this example");
		std::cout << "MD5: message ?bcd, one unknown byte.\n"
			<< "Additional observation: b after round 0 & 0x00007f80 = 0x" << std::hex << observed
			<< std::dec << "; final digest e2fc714c4727ee9395f324cd2e7f331f.\n"
			<< "Both modes receive exactly these two constraints. The checkpoint is extra information, not derived from the digest.\n";
		const auto eager = run(true, known, observed, digest);
		const auto deferred = run(false, known, observed, digest);
		demo::print("eager inside algorithm", eager);
		demo::print("deferred until algorithm ends", deferred);
		demo::require(eager.known_after_checkpoint == 8, "MD5 checkpoint did not determine all 8 unknown bits");
		demo::require(eager.digest_nodes < deferred.digest_nodes, "MD5 eager simplification did not shrink final expressions");
		demo::require(eager.recovered == deferred.recovered, "MD5 comparison recovered different inputs");
		std::cout << "Native verification passed; recovered abcd. Node counts are retained DAGs, not peak allocations.\n";
		return 0;
	}
	catch (const std::exception& error)
	{
		std::cerr << "partial_assert_md5: " << error.what() << '\n';
		return 1;
	}
}
