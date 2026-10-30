#ifndef DIXELU_BITREVERSE_MD5_H
#define DIXELU_BITREVERSE_MD5_H

#include <cstddef>
#include <cstdint>
#include <vector>

#include "bitreverse.h"
#include "hash_common.h"

namespace dixelu::bitreverse::hash
{

// RFC 1321.
template<template<size_t> typename integer>
integer<128> md5(std::vector<integer<8>> message)
{
	using uint32 = integer<32>;
	using detail::byte_order;

	constexpr std::uint8_t shifts[64] = {
		7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,
		5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,
		4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,
		6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21
	};

	constexpr std::uint32_t constants[64] = {
		0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee,
		0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501,
		0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be,
		0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821,
		0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa,
		0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8,
		0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed,
		0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a,
		0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c,
		0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70,
		0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05,
		0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665,
		0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039,
		0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1,
		0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1,
		0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391
	};

	detail::pad(message, byte_order::little);

	uint32 state[4] = {0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476};
	for (size_t block = 0; block < message.size(); block += 64)
	{
		uint32 words[16];
		for (size_t word = 0; word < 16; ++word)
			words[word] = detail::load_word(message, block + word * 4, byte_order::little);

		uint32 a = state[0], b = state[1], c = state[2], d = state[3];
		for (size_t round = 0; round < 64; ++round)
		{
			uint32 function;
			size_t word_index;

			if (round < 16)
			{
				function = (b & c) | ((~b) & d);
				word_index = round;
			}
			else if (round < 32)
			{
				function = (d & b) | ((~d) & c);
				word_index = (5 * round + 1) % 16;
			}
			else if (round < 48)
			{
				function = b ^ c ^ d;
				word_index = (3 * round + 5) % 16;
			}
			else
			{
				function = c ^ (b | (~d));
				word_index = (7 * round) % 16;
			}

			function += a;
			function += constants[round];
			function += words[word_index];

			a = d;
			d = c;
			c = b;
			b += detail::rotl(function, shifts[round]);
		}

		state[0] += a;
		state[1] += b;
		state[2] += c;
		state[3] += d;
	}

	return detail::pack_digest(state, byte_order::little);
}

} // namespace dixelu::bitreverse::hash

#endif // DIXELU_BITREVERSE_MD5_H
