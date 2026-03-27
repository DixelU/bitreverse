#include <iostream>
#include "bitreverse_v2.h"

template<template<size_t> typename int_tracker>
int_tracker<32> crc32(std::vector<int_tracker<8>> message)
{
	int_tracker<32> byte, mask;

	int_tracker<32> crc = 0xFFFFFFFF;
	const int_tracker<32> mask_const = 0xEDB88320;

	for (auto& ch : message)
	{
		byte = int_tracker<32>(ch);
		crc = crc ^ byte;

		for (int j = 7; j >= 0; j--)
		{
			mask = -(crc & 1);
			crc = (crc >> 1) ^ (mask_const & mask);
		}
	}
	return ~crc;
}

void real_crc32_reversal()
{
	std::vector<dixelu::bitreverse::v2::itu8> hashed_string_orig = { 'c', 'r', 'c', '3', '1' };

	dixelu::bitreverse::v2::int_tracker<32> target_hash = crc32(hashed_string_orig);

	std::vector<dixelu::bitreverse::v2::itu8> hashed_string{ nullptr, nullptr, nullptr, nullptr, nullptr };
	dixelu::bitreverse::v2::int_tracker<32> result(std::move(target_hash));

	dixelu::bitreverse::v2::bit_tracker hashed_string_is_not_ascii;
	for (auto& itu8 : hashed_string)
	{
		hashed_string_is_not_ascii |= dixelu::bitreverse::v2::bit_tracker(itu8 & 0x80); // is beyond ascii
		hashed_string_is_not_ascii |=
			dixelu::bitreverse::v2::bit_tracker((itu8 - 31) & 0x80) &  // is control symbol
			dixelu::bitreverse::v2::bit_tracker(itu8); // and not zero
	}

	auto crc32_result = crc32(hashed_string);
	auto crc32_result_differs = dixelu::bitreverse::v2::bit_tracker(crc32_result ^ result);
	auto is_false = crc32_result_differs | hashed_string_is_not_ascii;
	const dixelu::bitreverse::v2::bit_tracker _true(true);

	std::cout << "Real CRC32 reversal test" << std::endl;
	auto reversal_result = dixelu::bitreverse::v2::solver::assert_equality(is_false, _true);

	size_t counter = 0;
	std::cout << "Size: " << reversal_result.size() << std::endl;
	for (auto& solution : reversal_result)
	{
		++counter;
		std::cout << "=== SOLUTION " << counter << " ===" << std::endl;
		for (const auto& single_char : hashed_string)
		{
			auto char_copy = single_char;
			dixelu::bitreverse::v2::solver::assign_assert_result(char_copy, solution);

			std::cout << char_copy.to_string();
		}

		std::cout << std::endl;
	}
}


int main()
{
	real_crc32_reversal();

	return 0;
}
