#ifndef BITREVERSE_SUPPORT_INVERSE_BENCHMARK_H
#define BITREVERSE_SUPPORT_INVERSE_BENCHMARK_H

// Shared parts of the inverse benchmarks: query timing, artifact round trips
// and the four-byte MD5 workload.

#include <array>
#include <chrono>
#include <cstdint>
#include <optional>
#include <sstream>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

#include "inverse.h"
#include "md5.h"
#include "reference_hashes.h"
#include "timing.h"

namespace bench
{
namespace br = dixelu::bitreverse;
namespace inv = br::inversion;

// Every timed result is folded into this value, which the benchmarks print,
// so the optimizer cannot discard a timed query.
inline std::uint64_t consumed = 0;

inline void consume(std::uint64_t& checksum, std::uint64_t value)
{
	checksum = checksum * 131U + value;
}

inline void consume(std::uint64_t& checksum, const std::optional<inv::values>& result)
{
	consume(checksum, result.has_value());
	if (result)
		for (const bool bit : *result) consume(checksum, bit);
}

// Mean nanoseconds per call of query(index, checksum), cycling through
// `count` queries in batches of 64 until 65536 calls or about 40 ms.
template<class Query>
double time_queries(size_t count, Query&& query)
{
	using support::clock;
	if (count == 0) throw std::runtime_error("benchmark has no timed queries");
	std::uint64_t checksum = 0;
	size_t completed = 0;
	const auto started = clock::now();
	do
	{
		for (size_t batch = 0; batch < 64; ++batch, ++completed)
			query(completed % count, checksum);
	} while (completed < 65536 && clock::now() - started < std::chrono::milliseconds(40));
	const auto elapsed = std::chrono::duration<double, std::nano>(clock::now() - started).count();
	consumed ^= checksum + completed;
	return elapsed / static_cast<double>(completed);
}

struct saved_program
{
	std::string artifact;
	inv::program loaded;
};

// Queries are measured on a reloaded artifact, as a user of a saved inverse would run them.
inline saved_program save_and_load(const inv::program& program)
{
	std::ostringstream output;
	program.save(output);
	std::istringstream input(output.str());
	auto loaded = inv::program::load(input);
	return {output.str(), std::move(loaded)};
}

// Fixed-seed generator, so every run uses the same random targets.
struct xorshift64
{
	std::uint64_t state;

	std::uint64_t operator()()
	{
		state ^= state << 13;
		state ^= state >> 7;
		state ^= state << 17;
		return state;
	}
};

// Least significant bit first, the order of inv::bits_of.
inline inv::values to_values(std::uint64_t value, size_t width)
{
	inv::values bits(width);
	for (size_t bit = 0; bit < width; ++bit) bits[bit] = ((value >> bit) & 1U) != 0;
	return bits;
}

inline std::uint64_t to_integer(const inv::values& bits, size_t width)
{
	if (bits.size() != width) throw std::runtime_error("inverse returned the wrong input width");
	std::uint64_t value = 0;
	for (size_t bit = 0; bit < width; ++bit) value |= static_cast<std::uint64_t>(bits[bit]) << bit;
	return value;
}

// The message "md5" plus a zero byte, whose last `unknown_bits` bits are
// unknown: from the low bit of the final byte towards the first byte.
inline inv::program md5_program(size_t unknown_bits)
{
	std::vector<br::itu8> message{'m', 'd', '5', 0};
	for (size_t bit = 0; bit < unknown_bits; ++bit) message[3 - bit / 8].bits[7 - bit % 8] = br::unknown;
	return inv::program::compile(inv::bits_of(message), inv::bits_of(br::hash::md5(message)));
}

// The md5_program message as a little-endian word, with its unknown bits
// taken from `assignment`, low bit first.
inline std::uint32_t md5_message_word(size_t unknown_bits, size_t assignment)
{
	std::uint32_t word = 0x0035646dU;
	for (size_t bit = 0; bit < unknown_bits; ++bit)
	{
		const auto mask = std::uint32_t{1} << ((3 - bit / 8) * 8 + bit % 8);
		word = (word & ~mask) | (((assignment >> bit) & 1U) ? mask : 0);
	}
	return word;
}

// A native MD5 state as the output bits of md5_program.
inline inv::values md5_digest_values(const std::array<std::uint32_t, 4>& state)
{
	inv::values digest(128);
	for (size_t byte = 0; byte < 16; ++byte)
		for (size_t bit = 0; bit < 8; ++bit)
			digest[(15 - byte) * 8 + bit] = ((state[byte / 4] >> (8 * (byte % 4) + bit)) & 1U) != 0;
	return digest;
}

inline void check_md5_reference()
{
	// md5("md5!") = b682c4079aa8cae8524986bf8de47f5c.
	if (reference::md5_four_bytes(0x2135646dU) != std::array<std::uint32_t, 4>{0x07c482b6U, 0xe8caa89aU, 0xbf864952U, 0x5c7fe48dU})
		throw std::runtime_error("native MD5 failed the md5! known vector");
}

} // namespace bench

#endif // BITREVERSE_SUPPORT_INVERSE_BENCHMARK_H
