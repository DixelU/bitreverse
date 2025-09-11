#include <iostream>

#include "bitreverse.h"

void bitwise_reversal_test()
{
	const size_t x = 0b1101;
	const size_t y = 0b0010;

	constexpr size_t d = 19;
	using cur_int = dixelu::bitreverse::int_tracker<d>;

	const cur_int known_value{x}, target_result{y};
	const cur_int unk = dixelu::bitreverse::unknown;

	std::cout << "[";
	for (auto& el : unk.bits)
		std::cout << el.bit_state.get() << ", ";
	std::cout << "]" << std::endl;

#define BIT_OPERAND -

	const cur_int result = known_value BIT_OPERAND unk;
	const cur_int result_expected = known_value BIT_OPERAND target_result;

	auto assertion_result = dixelu::bitreverse::assert_equality<d>(result, result_expected);

	uint32_t counter = 0;
	for (auto& solution : assertion_result)
	{
		std::cout << "=== SOLUTION " << counter << " ===" << std::endl;

		cur_int unk_copy = unk;

		dixelu::bitreverse::assign_assert_result<d>(unk_copy, solution.assignments);

		std::cout << "Got:\t\t " << unk_copy.__to_string() << std::endl;
		std::cout << "Expected:\t " << (target_result).__to_string() << std::endl;

		std::cout << "Operand result:\t " << (known_value BIT_OPERAND unk_copy).__to_string() << std::endl;
		std::cout << "Operand real:\t " << (result_expected).__to_string() << std::endl;

		++counter;
	}
}

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
	using dixelu::bitreverse::unknown;

	std::vector<dixelu::bitreverse::itu8> hashed_string = { unknown, unknown, unknown, unknown, unknown, unknown, unknown };
	std::array<dixelu::bitreverse::bit_tracker, 32> target_hash
	{ 0,0,0,0,0,1,0,0,1,1,0,0,1,1,1,1,1,1,1,0,1,1,0,1,0,0,0,0,0,0,0,1 };
	dixelu::bitreverse::int_tracker<32> result(std::move(target_hash));

	dixelu::bitreverse::bit_tracker hashed_string_is_not_ascii;
	for (auto& itu8 : hashed_string)
	{
		hashed_string_is_not_ascii |= dixelu::bitreverse::bit_tracker(itu8 & 0x80); // is beyond ascii
		hashed_string_is_not_ascii |=
			dixelu::bitreverse::bit_tracker((itu8 - 31) & 0x80) &  // is control symbol
			dixelu::bitreverse::bit_tracker(itu8); // and not zero
	}

	auto crc32_result = crc32(hashed_string);
	auto crc32_result_differs = dixelu::bitreverse::bit_tracker(crc32_result ^ result);
	auto is_false = crc32_result_differs | hashed_string_is_not_ascii;
	dixelu::bitreverse::bit_tracker _true(true);

	std::cout << "Real CRC32 reversal test" << std::endl;
	auto reversal_result = dixelu::bitreverse::assert_equality(is_false, _true);

	size_t counter = 0;
	std::cout << "Size: " << reversal_result.size() << std::endl;
	for (auto& solution : reversal_result)
	{
		++counter; 
		std::cout << "=== SOLUTION " << counter << " ===" << std::endl;
		for (auto single_char : hashed_string)
		{
			dixelu::bitreverse::assign_assert_result<8>(single_char, solution.assignments);
			std::cout << single_char.__to_string();
		}
		std::cout << std::endl;
	}
}

int main()
{
	// dixelu::bitreverse::itu8 x = 0b01101010;
	// dixelu::bitreverse::itu8 y = 0b00000011;
	// dixelu::bitreverse::itu8 z = 0b00010000;
	//
	// std::cout << "x: " << x.__to_string() << std::endl;
	// std::cout << "y: " << y.__to_string() << std::endl;
	// std::cout << "z: " << z.__to_string() << std::endl;
	// std::cout << "S: " << (x + y + z).__to_string() << std::endl;

	bitwise_reversal_test();

	return 0;
}
