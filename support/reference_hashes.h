#ifndef BITREVERSE_SUPPORT_REFERENCE_HASHES_H
#define BITREVERSE_SUPPORT_REFERENCE_HASHES_H

// Plain native hash implementations, independent of the tracked versions in
// include/, for checking and benchmarking them.

#include <array>
#include <bit>
#include <cstdint>
#include <stdexcept>
#include <string_view>

namespace reference
{

// MD5 per-round left-rotation amounts and additive constants.
inline constexpr std::array<unsigned, 64> md5_shifts{
	7,12,17,22,7,12,17,22,7,12,17,22,7,12,17,22,
	5,9,14,20,5,9,14,20,5,9,14,20,5,9,14,20,
	4,11,16,23,4,11,16,23,4,11,16,23,4,11,16,23,
	6,10,15,21,6,10,15,21,6,10,15,21,6,10,15,21};
inline constexpr std::array<std::uint32_t, 64> md5_constants{
	0xd76aa478,0xe8c7b756,0x242070db,0xc1bdceee,0xf57c0faf,0x4787c62a,0xa8304613,0xfd469501,
	0x698098d8,0x8b44f7af,0xffff5bb1,0x895cd7be,0x6b901122,0xfd987193,0xa679438e,0x49b40821,
	0xf61e2562,0xc040b340,0x265e5a51,0xe9b6c7aa,0xd62f105d,0x02441453,0xd8a1e681,0xe7d3fbc8,
	0x21e1cde6,0xc33707d6,0xf4d50d87,0x455a14ed,0xa9e3e905,0xfcefa3f8,0x676f02d9,0x8d2a4c8a,
	0xfffa3942,0x8771f681,0x6d9d6122,0xfde5380c,0xa4beea44,0x4bdecfa9,0xf6bb4b60,0xbebfbc70,
	0x289b7ec6,0xeaa127fa,0xd4ef3085,0x04881d05,0xd9d4d039,0xe6db99e5,0x1fa27cf8,0xc4ac5665,
	0xf4292244,0x432aff97,0xab9423a7,0xfc93a039,0x655b59c3,0x8f0ccc92,0xffeff47d,0x85845dd1,
	0x6fa87e4f,0xfe2ce6e0,0xa3014314,0x4e0811a1,0xf7537e82,0xbd3af235,0x2ad7d2bb,0xeb86d391};

// One MD5 compression of a padded 16-word block from the initial state.
inline std::array<std::uint32_t, 4> md5_block(const std::array<std::uint32_t, 16>& words)
{
	std::array<std::uint32_t, 4> state{0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476};
	auto a = state[0], b = state[1], c = state[2], d = state[3];
	for (unsigned round = 0; round < 64; ++round)
	{
		std::uint32_t f;
		unsigned word;
		if (round < 16) { f = (b & c) | (~b & d); word = round; }
		else if (round < 32) { f = (d & b) | (~d & c); word = (5 * round + 1) % 16; }
		else if (round < 48) { f = b ^ c ^ d; word = (3 * round + 5) % 16; }
		else { f = c ^ (b | ~d); word = 7 * round % 16; }
		const auto next_b = b + std::rotl(a + f + md5_constants[round] + words[word], static_cast<int>(md5_shifts[round]));
		a = d; d = c; c = b; b = next_b;
	}
	state[0] += a; state[1] += b; state[2] += c; state[3] += d;
	return state;
}

// MD5 of exactly four bytes packed little endian; no per-call conversion, so
// it can serve as a timed native baseline.
inline std::array<std::uint32_t, 4> md5_four_bytes(std::uint32_t message_word)
{
	return md5_block({message_word, 0x80U, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 32U, 0});
}

// MD5 of a message that fits one block (shorter than 56 bytes).
inline std::array<std::uint32_t, 4> md5_one_block(std::string_view message)
{
	if (message.size() >= 56)
		throw std::invalid_argument("one-block reference MD5 message is too long");

	std::array<std::uint32_t, 16> words{};
	for (size_t i = 0; i < message.size(); ++i)
		words[i / 4] |= std::uint32_t(static_cast<unsigned char>(message[i])) << (8 * (i % 4));
	words[message.size() / 4] |= 0x80U << (8 * (message.size() % 4));
	words[14] = static_cast<std::uint32_t>(message.size() * 8);
	return md5_block(words);
}

// Reflected CRC-32 (IEEE) register update, without the final complement.
inline std::uint32_t crc32_update(std::uint32_t crc, std::uint8_t byte)
{
	crc ^= byte;
	for (unsigned bit = 0; bit < 8; ++bit)
		crc = (crc >> 1) ^ ((crc & 1U) ? 0xedb88320U : 0U);
	return crc;
}

inline std::uint32_t crc32(std::string_view message)
{
	std::uint32_t crc = 0xffffffffU;
	for (const unsigned char byte : message)
		crc = crc32_update(crc, byte);
	return ~crc;
}

} // namespace reference

#endif // BITREVERSE_SUPPORT_REFERENCE_HASHES_H
