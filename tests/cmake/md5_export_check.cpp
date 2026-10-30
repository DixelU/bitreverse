// Checks the exported md5("md5" + one byte) inverse, compiled in isolation
// from the project. The digests were cross-checked with .NET's MD5.

#include "md5_inverse.h"
#include "md5_inverse.h" // Generated headers must tolerate repeated inclusion.

#include <array>
#include <string_view>

bool check(std::string_view digest, unsigned last_byte)
{
	std::array<bool, 128> target{};
	for (size_t digit = 0; digit < digest.size(); ++digit)
	{
		const char c = digest[digest.size() - digit - 1];
		const unsigned nibble = c <= '9' ? c - '0' : c - 'a' + 10;
		for (size_t bit = 0; bit < 4; ++bit)
			target[digit * 4 + bit] = ((nibble >> bit) & 1) != 0;
	}
	const auto recovered = bitreverse_inverse(target);
	if (!recovered)
		return false;
	const std::array<unsigned, 4> expected{'m', 'd', '5', last_byte};
	for (size_t byte = 0; byte < expected.size(); ++byte)
		for (size_t bit = 0; bit < 8; ++bit)
			if ((*recovered)[byte * 8 + bit] != (((expected[byte] >> bit) & 1) != 0))
				return false;
	return true;
}

int main()
{
	return check("b682c4079aa8cae8524986bf8de47f5c", '!') &&
		check("0b5aeacaa0f4f17d5fd7a8ea0a855e1f", '?') &&
		!bitreverse_inverse(std::array<bool, 128>{}) ? 0 : 1;
}
