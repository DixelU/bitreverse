#ifndef BITREVERSE_SUPPORT_TEXT_H
#define BITREVERSE_SUPPORT_TEXT_H

// Conversions between command-line or printed text and numbers.

#include <charconv>
#include <cstddef>
#include <cstdint>
#include <span>
#include <stdexcept>
#include <string>
#include <string_view>
#include <system_error>
#include <vector>

namespace support
{

inline unsigned hex_digit(char digit)
{
	if (digit >= '0' && digit <= '9')
		return static_cast<unsigned>(digit - '0');
	if (digit >= 'a' && digit <= 'f')
		return static_cast<unsigned>(digit - 'a' + 10);
	if (digit >= 'A' && digit <= 'F')
		return static_cast<unsigned>(digit - 'A' + 10);
	throw std::invalid_argument("invalid hex digit; expected 0..9, a..f, or A..F");
}

inline char hex_char(unsigned nibble)
{
	return "0123456789abcdef"[nibble & 15];
}

// Two digits per byte, in text order.
inline std::vector<std::uint8_t> parse_hex_bytes(std::string_view text)
{
	if (text.size() % 2 != 0)
		throw std::invalid_argument("hex must contain complete bytes");
	std::vector<std::uint8_t> bytes;
	bytes.reserve(text.size() / 2);
	for (size_t digit = 0; digit < text.size(); digit += 2)
		bytes.push_back(static_cast<std::uint8_t>((hex_digit(text[digit]) << 4) | hex_digit(text[digit + 1])));
	return bytes;
}

inline std::string to_hex(std::span<const std::uint8_t> bytes)
{
	std::string text;
	text.reserve(bytes.size() * 2);
	for (const auto byte : bytes)
	{
		text.push_back(hex_char(byte >> 4));
		text.push_back(hex_char(byte));
	}
	return text;
}

// A hex number as bits, least significant first, four per digit.
inline std::vector<bool> parse_hex_number(std::string_view text)
{
	std::vector<bool> bits;
	bits.reserve(text.size() * 4);
	for (auto digit = text.rbegin(); digit != text.rend(); ++digit)
	{
		const unsigned nibble = hex_digit(*digit);
		for (unsigned bit = 0; bit < 4; ++bit)
			bits.push_back(((nibble >> bit) & 1U) != 0);
	}
	return bits;
}

// The inverse of parse_hex_number; bits.size() must be a multiple of four.
template<class Bits>
std::string to_hex_number(const Bits& bits)
{
	std::string text(bits.size() / 4, '0');
	for (size_t digit = 0; digit < text.size(); ++digit)
	{
		unsigned nibble = 0;
		for (unsigned bit = 0; bit < 4; ++bit)
			nibble |= static_cast<unsigned>(bits[digit * 4 + bit]) << bit;
		text[text.size() - digit - 1] = hex_char(nibble);
	}
	return text;
}

// `name` identifies the argument in the error message.
inline size_t parse_size(std::string_view text, std::string_view name)
{
	size_t value = 0;
	const auto [end, error] = std::from_chars(text.data(), text.data() + text.size(), value);
	if (text.empty() || error != std::errc{} || end != text.data() + text.size())
		throw std::invalid_argument(std::string(name) + " requires a nonnegative integer");
	return value;
}

} // namespace support

#endif // BITREVERSE_SUPPORT_TEXT_H
