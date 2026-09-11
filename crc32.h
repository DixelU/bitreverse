#ifndef DIXELU_BITREVERSE_CRC32_H
#define DIXELU_BITREVERSE_CRC32_H

#include <cstddef>
#include <vector>

#include "bitreverse.h"

namespace dixelu::bitreverse::hash
{

// Reflected IEEE CRC-32, with initial/final XOR of 0xffffffff.
template<template<size_t> typename integer>
integer<32> crc32(const std::vector<integer<8>>& message)
{
	integer<32> byte;
	integer<32> mask;
	integer<32> crc = 0xFFFFFFFF;
	const integer<32> polynomial = 0xEDB88320;

	for (const auto& character : message)
	{
		byte = integer<32>(character);
		crc ^= byte;

		for (int bit = 0; bit < 8; ++bit)
		{
			mask = -(crc & 1);
			crc = (crc >> 1) ^ (polynomial & mask);
		}
	}

	return ~crc;
}

}

#endif
