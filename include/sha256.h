#ifndef DIXELU_BITREVERSE_SHA256_H
#define DIXELU_BITREVERSE_SHA256_H

#include <cstddef>
#include <cstdint>
#include <vector>

#include "bitreverse.h"

namespace dixelu::bitreverse::hash
{

// FIPS 180-4, sections 5.1.1 and 6.2.2. Digest words are in display order.
template<template<size_t> typename integer>
integer<256> sha256(std::vector<integer<8>> message)
{
	using uint32 = integer<32>;
	using uint256 = integer<256>;
	constexpr std::uint32_t constants[64] = {
		0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5,
		0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
		0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
		0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
		0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc,
		0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
		0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
		0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
		0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
		0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
		0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3,
		0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
		0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5,
		0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
		0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
		0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
	};
	const std::uint64_t message_size_bits =
		static_cast<std::uint64_t>(message.size()) * 8;
	message.push_back(0x80);
	while (message.size() % 64 != 56)
		message.push_back(0);
	for (size_t byte = 0; byte < 8; ++byte)
		message.push_back(static_cast<std::uint8_t>(message_size_bits >> (56 - byte * 8)));

	const auto rotate = [](const uint32& word, size_t shift)
	{
		return (word >> shift) | (word << (32 - shift));
	};
	uint32 state[8] = {
		0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a,
		0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19
	};
	for (size_t block = 0; block < message.size(); block += 64)
	{
		uint32 words[64];
		for (size_t word = 0; word < 16; ++word)
		{
			const size_t offset = block + word * 4;
			words[word] = (uint32(message[offset]) << 24) |
				(uint32(message[offset + 1]) << 16) |
				(uint32(message[offset + 2]) << 8) | uint32(message[offset + 3]);
		}
		for (size_t word = 16; word < 64; ++word)
		{
			const auto sigma0 = rotate(words[word - 15], 7) ^
				rotate(words[word - 15], 18) ^ (words[word - 15] >> 3);
			const auto sigma1 = rotate(words[word - 2], 17) ^
				rotate(words[word - 2], 19) ^ (words[word - 2] >> 10);
			words[word] = words[word - 16] + sigma0;
			words[word] += words[word - 7];
			words[word] += sigma1;
		}

		uint32 a = state[0], b = state[1], c = state[2], d = state[3];
		uint32 e = state[4], f = state[5], g = state[6], h = state[7];
		for (size_t round = 0; round < 64; ++round)
		{
			const auto sum1 = rotate(e, 6) ^ rotate(e, 11) ^ rotate(e, 25);
			const auto choose = (e & f) ^ ((~e) & g);
			uint32 temp1 = h + sum1;
			temp1 += choose;
			temp1 += constants[round];
			temp1 += words[round];
			const auto sum0 = rotate(a, 2) ^ rotate(a, 13) ^ rotate(a, 22);
			const auto majority = (a & b) ^ (a & c) ^ (b & c);
			const uint32 temp2 = sum0 + majority;
			h = g;
			g = f;
			f = e;
			e = d + temp1;
			d = c;
			c = b;
			b = a;
			a = temp1 + temp2;
		}
		state[0] += a;
		state[1] += b;
		state[2] += c;
		state[3] += d;
		state[4] += e;
		state[5] += f;
		state[6] += g;
		state[7] += h;
	}

	uint256 digest = 0;
	for (size_t word = 0; word < 8; ++word)
		digest |= uint256(state[word]) << (224 - word * 32);
	return digest;
}

} // namespace dixelu::bitreverse::hash

#endif // DIXELU_BITREVERSE_SHA256_H
