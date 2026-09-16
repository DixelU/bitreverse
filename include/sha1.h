#ifndef DIXELU_BITREVERSE_SHA1_H
#define DIXELU_BITREVERSE_SHA1_H

#include <cstddef>
#include <cstdint>
#include <vector>

#include "bitreverse.h"

namespace dixelu::bitreverse::hash
{

// FIPS 180-4, sections 5.1.1 and 6.1.2. Digest words are in display order.
template<template<size_t> typename integer>
integer<160> sha1(std::vector<integer<8>> message)
{
	using uint32 = integer<32>;
	using uint160 = integer<160>;
	const std::uint64_t message_size_bits =
		static_cast<std::uint64_t>(message.size()) * 8;
	message.push_back(0x80);
	while (message.size() % 64 != 56)
		message.push_back(0);
	for (size_t byte = 0; byte < 8; ++byte)
		message.push_back(static_cast<std::uint8_t>(message_size_bits >> (56 - byte * 8)));

	const auto rotate = [](const uint32& word, size_t shift)
	{
		return (word << shift) | (word >> (32 - shift));
	};
	uint32 state[5] = {0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476, 0xc3d2e1f0};
	for (size_t block = 0; block < message.size(); block += 64)
	{
		uint32 words[80];
		for (size_t word = 0; word < 16; ++word)
		{
			const size_t offset = block + word * 4;
			words[word] = (uint32(message[offset]) << 24) |
				(uint32(message[offset + 1]) << 16) |
				(uint32(message[offset + 2]) << 8) | uint32(message[offset + 3]);
		}
		for (size_t word = 16; word < 80; ++word)
			words[word] = rotate(words[word - 3] ^ words[word - 8] ^
				words[word - 14] ^ words[word - 16], 1);

		uint32 a = state[0], b = state[1], c = state[2], d = state[3], e = state[4];
		for (size_t round = 0; round < 80; ++round)
		{
			uint32 function;
			std::uint32_t constant;
			if (round < 20)
			{
				function = (b & c) | ((~b) & d);
				constant = 0x5a827999;
			}
			else if (round < 40)
			{
				function = b ^ c ^ d;
				constant = 0x6ed9eba1;
			}
			else if (round < 60)
			{
				function = (b & c) | (b & d) | (c & d);
				constant = 0x8f1bbcdc;
			}
			else
			{
				function = b ^ c ^ d;
				constant = 0xca62c1d6;
			}
			uint32 next = rotate(a, 5);
			next += function;
			next += e;
			next += constant;
			next += words[round];
			e = d;
			d = c;
			c = rotate(b, 30);
			b = a;
			a = next;
		}
		state[0] += a;
		state[1] += b;
		state[2] += c;
		state[3] += d;
		state[4] += e;
	}

	uint160 digest = 0;
	for (size_t word = 0; word < 5; ++word)
		digest |= uint160(state[word]) << (128 - word * 32);
	return digest;
}

} // namespace dixelu::bitreverse::hash

#endif // DIXELU_BITREVERSE_SHA1_H
