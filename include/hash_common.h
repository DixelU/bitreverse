#ifndef DIXELU_BITREVERSE_HASH_COMMON_H
#define DIXELU_BITREVERSE_HASH_COMMON_H

#include <cstddef>
#include <cstdint>
#include <vector>

// Message padding, word loads and rotations shared by MD5, SHA-1 and SHA-256.
// Rotations and byte moves only rewire existing bits, so they add no gates.
namespace dixelu::bitreverse::hash::detail
{

enum class byte_order
{
	little,
	big
};

// Bit offset of the i-th serialized byte of a `size`-byte integer.
constexpr size_t byte_shift(size_t i, size_t size, byte_order order)
{
	return 8 * (order == byte_order::little ? i : size - 1 - i);
}

// Merkle-Damgard padding: 0x80, zeros up to 56 mod 64 bytes, then the message
// length in bits as a 64-bit integer.
template<template<size_t> typename integer>
void pad(std::vector<integer<8>>& message, byte_order order)
{
	const auto length_bits = static_cast<std::uint64_t>(message.size()) * 8;
	message.push_back(0x80);
	while (message.size() % 64 != 56)
		message.push_back(0);

	for (size_t i = 0; i < 8; ++i)
		message.push_back(static_cast<std::uint8_t>(length_bits >> byte_shift(i, 8, order)));
}

template<template<size_t> typename integer>
integer<32> load_word(const std::vector<integer<8>>& message, size_t offset, byte_order order)
{
	integer<32> word = 0;
	for (size_t i = 0; i < 4; ++i)
		word |= integer<32>(message[offset + i]) << byte_shift(i, 4, order);
	return word;
}

// The state words serialized in `order`, read back as one big-endian integer
// so that the digest prints in its conventional hex form.
template<template<size_t> typename integer, size_t Words>
integer<32 * Words> pack_digest(const integer<32> (&state)[Words], byte_order order)
{
	constexpr size_t digest_bits = 32 * Words;
	integer<digest_bits> digest = 0;
	for (size_t word = 0; word < Words; ++word)
	{
		for (size_t i = 0; i < 4; ++i)
		{
			const integer<8> byte(state[word] >> byte_shift(i, 4, order));
			digest |= integer<digest_bits>(byte) << byte_shift(4 * word + i, 4 * Words, byte_order::big);
		}
	}
	return digest;
}

template<template<size_t> typename integer, size_t N>
integer<N> rotl(const integer<N>& value, size_t shift)
{
	return (value << shift) | (value >> (N - shift));
}

template<template<size_t> typename integer, size_t N>
integer<N> rotr(const integer<N>& value, size_t shift)
{
	return (value >> shift) | (value << (N - shift));
}

} // namespace dixelu::bitreverse::hash::detail

#endif // DIXELU_BITREVERSE_HASH_COMMON_H
