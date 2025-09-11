#include <iostream>

#include "bitreverse.h"

void bitwise_reversal_test()
{
	const size_t x = 0b1101;
	const size_t y = 0b0010;

	using cur_int = dixelu::bitreverse::int_tracker<4>;

	const cur_int known_value{x}, target_result{y};
	const cur_int unk = dixelu::bitreverse::unknown;

	std::cout << "[";
	for (auto& el : unk.bits)
		std::cout << el.bit_state.get() << ", ";
	std::cout << "]" << std::endl;

#define BIT_OPERAND -

	const cur_int result = known_value BIT_OPERAND unk;
	const cur_int result_expected = known_value BIT_OPERAND target_result;

	auto assertion_result = dixelu::bitreverse::assert_equality(result, result_expected);

	uint32_t counter = 0;
	for (auto& solution : assertion_result)
	{
		std::cout << "=== SOLUTION " << counter << " ===" << std::endl;

		cur_int unk_copy = unk;

		dixelu::bitreverse::assign_assert_result(unk_copy, solution.assignments);

		std::cout << "Got:\t\t " << unk_copy.__to_string() << std::endl;
		std::cout << "Expected:\t " << (target_result).__to_string() << std::endl;

		std::cout << "Operand result:\t " << (known_value BIT_OPERAND unk_copy).__to_string() << std::endl;
		std::cout << "Operand real:\t " << (result_expected).__to_string() << std::endl;

		++counter;
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
