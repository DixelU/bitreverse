// Checks the exported inverse of CRC-32 over one byte whose high nibble is 6,
// compiled in isolation from the project, against an independent CRC-32.

#include "learned.h"
#include "learned.h" // Generated headers must tolerate repeated inclusion.

#include <cstdint>

std::uint32_t crc32(unsigned char byte)
{
	std::uint32_t crc = 0xffffffffU ^ byte;
	for (unsigned bit = 0; bit < 8; ++bit)
		crc = (crc >> 1) ^ ((crc & 1U) ? 0xedb88320U : 0U);
	return ~crc;
}

int main()
{
	for (unsigned byte = 0; byte < 256; ++byte)
	{
		const auto crc = crc32(static_cast<unsigned char>(byte));
		std::array<bool, 32> target{};
		for (size_t bit = 0; bit < 32; ++bit)
			target[bit] = (crc >> bit) & 1U;
		const auto input = bitreverse_inverse(target);
		if ((byte >= 0x60 && byte <= 0x6f) != input.has_value())
			return 1;
		if (input)
			for (size_t bit = 0; bit < 8; ++bit)
				if ((*input)[bit] != bool((byte >> bit) & 1U))
					return 2;
	}
	return bitreverse_inverse(std::array<bool, 32>{}) ? 3 : 0;
}
