﻿#include <iostream>

#include "bitreverse.h"
#include "bitreverse2.h"

#include "counted_ptr.h"

void counted_simple_test()
{
    std::cout << std::endl << "counted_simple_test" << std::endl;
    auto t = dixelu::make_counted<int>(206516);
    auto v = t;
    auto q = v;

    q = t;
    t = v;
    q = v;
    v = t;
    v = v;

    std::cout << *v << ":" << v.count() << std::endl;
    std::cout << *t << ":" << t.count() << std::endl;
    std::cout << *q << ":" << q.count() << std::endl;
}

void counted_test_with_enable_counted_from_this()
{
    std::cout << std::endl << "counted_test_with_enable_counted_from_this" << std::endl;
    struct A :
        public dixelu::enable_counted_from_this<A>
    {
        char c = 'Z';
    };

    struct B :
        public dixelu::enable_counted_from_this<B>
    {
    };

    auto bbb = dixelu::make_counted<B>();
    auto t = dixelu::make_counted<A>();
    auto v = t->counted_from_this();
    auto q = t->counted_from_this();

    q = t;
    t = v;
    q = v;
    v = t;
    v = v;

    std::cout << v->c << ":" << v.count() << std::endl;
    std::cout << t->c << ":" << t.count() << std::endl;
    std::cout << q->c << ":" << q.count() << std::endl;

    t.reset();
    q.reset();

    std::cout << v->c << ":" << v.count() << std::endl;
}

void inplace_calculation_test_1()
{
    std::cout << std::endl << "inplace_calculation_test 1" << std::endl;
    dixelu::bitreverse::int_tracker<32> a = 2173, b = 234789, c = 1, u = dixelu::bitreverse::unknown;
    b = a ^ u;
    std::cout << b.__to_string() << std::endl;
    a |= b;
    b ^= c;
    c = a | b & c;
    std::cout << c.__to_string() << std::endl;
    std::cout << "Bit depth: " << c.__max_depth() << std::endl;
}

void inplace_calculation_test_2()
{
	std::cout << std::endl << "inplace_calculation_test 2" << std::endl;
	dixelu::bitreverse2::int_tracker<32> a = 2173, b = 234789, c = 1, u = dixelu::bitreverse2::unknown;
	b = a ^ u;
	std::cout << b.__to_string() << std::endl;
	a |= b;
	b ^= c;
	c = a | b & c;
	std::cout << c.__to_string() << std::endl;
	std::cout << "Bit depth: " << c.__max_depth().second << std::endl;
}


template<typename F>
void hashing_test_1(size_t string_size, F f)
{
    std::cout << std::endl << "Hashing test 1. Checking the history depths on string size: " << string_size << std::endl;

    std::vector<dixelu::bitreverse::itu8> unknown_string(string_size, dixelu::bitreverse::unknown);

    for (auto& ch : unknown_string)
        ch &= 0x7F;//ascii symbols

    auto result = f(unknown_string);

    std::cout << result.__to_string() << " " << result.__max_depth() << std::endl;

    std::cout << "Result history depths: " << std::endl;
    for (auto& el : result.bits)
        std::cout << el.bit_state->max_depth << " ";
    std::cout << std::endl;
}


template<typename F>
void hashing_test_2(size_t string_size, F f)
{
	std::cout << std::endl << "Hashing test 2. Checking the history depths on string size: " << string_size << std::endl;

	std::vector<dixelu::bitreverse2::itu8> unknown_string(string_size, dixelu::bitreverse2::unknown);

	for (auto& ch : unknown_string)
		ch &= 0x7F;//ascii symbols

	auto result = f(unknown_string);

	std::cout << result.__to_string() << " " << result.__max_depth_str() << std::endl;

	std::cout << "Result history depths: " << std::endl;
	for (auto& el : result.bits)
		std::cout << el._conjunction.size() << " ";
	std::cout << std::endl;
}

template<template<size_t> typename int_tracker>
int_tracker<32> crc32(std::vector<int_tracker<8>> message)
{
	int_tracker<32> byte, mask;

	int_tracker<32> crc = 0xFFFFFFFF;
    const int_tracker<32> mask_const = 0xEDB88320;

	int c = 0;
    for (auto& ch : message)
    {
		std::cout << ++c << std::endl;
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

void add_substract_test_1()
{
    std::cout << std::endl << "add_substract_test 1" << std::endl;
    dixelu::bitreverse::itu16 a, b;
    a = dixelu::bitreverse::unknown;
    b = 16;
    auto apb = a + b;
    auto amb = a - b;
    auto ma = -a;
    auto nota = ~a;

    std::cout << "a + b " << apb.__to_string() << " depth: " << apb.__max_depth() << std::endl;
    std::cout << "a - b " << amb.__to_string() << " depth: " << amb.__max_depth() << std::endl;
    std::cout << "-a " << ma.__to_string() << " depth: " << ma.__max_depth() << std::endl;
    std::cout << "~a " << nota.__to_string() << " depth: " << nota.__max_depth() << std::endl;
}


void add_substract_test_2()
{
	std::cout << std::endl << "add_substract_test 2" << std::endl;
	dixelu::bitreverse2::itu16 a, b;
	a = dixelu::bitreverse2::unknown;
	b = 16;

	auto apb = a + b;
	auto amb = a - b;
	auto ma = -a;
	auto nota = ~a;

	std::cout << "a + b " << apb.__to_string() << " depth: " << apb.__max_depth_str() << std::endl;
	std::cout << "a - b " << amb.__to_string() << " depth: " << amb.__max_depth_str() << std::endl;
	std::cout << "-a " << ma.__to_string() << " depth: " << ma.__max_depth_str() << std::endl;
	std::cout << "~a " << nota.__to_string() << " depth: " << nota.__max_depth_str() << std::endl;
}

template<template<size_t> typename int_tracker>
int_tracker<128> md5(std::vector<int_tracker<8>> message)
{
    using itu128 = int_tracker<128>;
    using itu32 = int_tracker<32>;
    using itu8 = int_tracker<8>;

    constexpr uint8_t s[64] = {
        7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,
        5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,
        4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,
        6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21
    };

    constexpr uint32_t K[64] = {
        0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee,
        0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501,
        0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be,
        0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821,
        0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa,
        0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8,
        0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed,
        0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a,
        0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c,
        0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70,
        0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05,
        0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665,
        0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039,
        0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1,
        0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1,
        0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391
    };

    itu32 a0 = 0x67452301;
    itu32 b0 = 0xefcdab89;
    itu32 c0 = 0x98badcfe;
    itu32 d0 = 0x10325476;
    
    uint64_t message_size_bits = message.size() * 8;
    message.push_back(0x80);
    while (message.size() % 64 != 56)
        message.push_back(0);

    for(size_t i = 0; i < 8; ++i)
    {
        auto value = ((uint8_t*)&message_size_bits)[i];
        message.push_back(value);
    }

    auto combine = [](const itu8& a, const itu8& b, const itu8& c, const itu8& d)
    {
        return (itu32(a) << 24) | (itu32(b) << 16) | (itu32(c) << 8) | itu32(d);
    };

    auto leftrotate = [](itu32 value, size_t d) -> itu32
    {
        return (value << d) | (value >> (32 - d));
    };

    auto reverse_endianness = [](itu32 val)
    {
        return
            (((val) & 0xFF) << 24) |
            (((val >> 8) & 0xFF) << 16) |
            (((val >> 16) & 0xFF) << 8) |
            (((val >> 24) & 0xFF));
    };

    for (size_t q = 0; q < message.size(); q += 64)
    {
        itu32 M[16];
        for (size_t j = 0; j < 64; j += 4)
        {
            auto& M_elem = M[j >> 2];
            M_elem = combine(message[j + 3], message[j + 2], message[j + 1], message[j + 0]);
            //std::cout << M_elem.__to_string() << std::endl;
        }

        itu32 A = a0;
        itu32 B = b0;
        itu32 C = c0;
        itu32 D = d0;

        for (size_t i = 0; i < 64; ++i)
        {
            itu32 F;
            size_t g;
            if (i <= 15)
            {
                F = (B & C) | ((~B) & D);
                g = i;
            }
            else if (i <= 31)
            {
                F = (D & B) | ((~D) & C);
                g = (5 * i + 1) % 16;
            }
            else if (i <= 47)
            {
                F = B ^ C ^ D;
                g = (3 * i + 5) % 16;
            }
            else if (i <= 63)
            {
                F = C ^ (B | (~D));
                g = (7 * i) % 16;
            }

            F = F + A + K[i] + M[g];
            A = D;
            D = C;
            C = B;
            B = B + leftrotate(F, s[i]);
        }

        a0 += A;
        b0 += B;
        c0 += C;
        d0 += D;
    }

    return 
        (itu128(reverse_endianness(a0)) << (32 + 64)) |
        (itu128(reverse_endianness(b0)) << 64) |
        (itu128(reverse_endianness(c0)) << 32) |
        itu128(reverse_endianness(d0));
}

void real_md5_reversal()
{
    using dixelu::bitreverse::unknown;

    std::vector<dixelu::bitreverse::itu8> hashed_string = { 'm', 'd', '5', unknown, unknown, unknown, unknown };
    std::array<dixelu::bitreverse::bit_tracker, 128> target_hash
    { 0,0,0,1,1,0,1,1,1,1,0,0,0,0,1,0,1,0,0,1,1,0,1,1,0,0,1,1,0,1,1,0,1,1,1,1,0,1,1,0,0,
        0,1,0,0,0,1,1,1,0,1,1,1,0,1,0,1,0,0,0,0,0,1,0,1,0,1,0,1,0,1,0,1,1,1,1,0,1,1,0,0,
        1,1,1,0,0,1,0,0,1,0,0,1,1,1,1,1,1,0,1,0,0,1,1,1,0,1,1,0,0,0,1,0,1,1,0,0,1,1,1,0,0,0,1,1,0,0,0 };
    dixelu::bitreverse::int_tracker<128> result(std::move(target_hash));

    dixelu::bitreverse::bit_tracker hashed_string_is_not_ascii;
    for (auto& itu8 : hashed_string)
    {
        hashed_string_is_not_ascii |= dixelu::bitreverse::bit_tracker(itu8 & 0x80); // is beyond ascii
        hashed_string_is_not_ascii |= 
            dixelu::bitreverse::bit_tracker((itu8 - 31) & 0x80) &  // is control symbol
            dixelu::bitreverse::bit_tracker(itu8); // and not zero
    }

    auto md5_result = md5(hashed_string);
    auto md5_result_differs = dixelu::bitreverse::bit_tracker(md5_result ^ result);

    auto is_false = md5_result_differs & hashed_string_is_not_ascii;
    
    dixelu::bitreverse::assert_equality(is_false, 0);

    std::cout << "Real MD5 reversal test" << std::endl;
    std::cout << md5_result.__to_string() << std::endl;
    std::cout << is_false.bit_state->max_depth << std::endl;
}

template<size_t N>
void print_depth(dixelu::bitreverse2::int_tracker<N>& value, std::string str = "")
{
	auto [depth, width] = value.__max_depth();
	std::cout << str << ": [" << depth << ", " << width << "]" << std::endl;
}

void binary_undef_test()
{
	dixelu::bitreverse2::itu8 a = 5, b = 1;
	auto res = a - b;

	std::cout << "5 - 1: " << res.__to_string() << std::endl;
	print_depth(res, "5 - 1 depth");

	res = res & dixelu::bitreverse2::unknown;
	std::cout << "Single unknown bit: " << res.__to_string() << std::endl;
	print_depth(res, "Single unknown bit depth");

	auto ones = res | 0xFF;
	std::cout << "All ones: " << ones.__to_string() << std::endl;
	print_depth(ones, "All ones depth");

	auto smth = res + res;
	std::cout << "Shifted unknown: " << smth.__to_string() << std::endl;
	print_depth(res, "Shifted unknown depth");
}

void undef_xor_undef_test()
{
	std::cout << "Undef xor undef test " << std::endl;
	dixelu::bitreverse2::int_tracker<2> value = dixelu::bitreverse2::unknown;
	value ^= value;

	std::cout << value.__to_string() << std::endl;
}

void undef_test()
{
	std::cout << "0 - Undef test " << std::endl;
	dixelu::bitreverse2::int_tracker<5> value = dixelu::bitreverse2::unknown;
	value = -value;

	std::cout << value.__to_string() << std::endl;
	print_depth(value, "0 - Undef depth");
}

//  u  u  u  u
// ~u ~u ~u ~u <- ~
//

int main()
{
	undef_xor_undef_test();
	binary_undef_test();
	undef_test();

    //counted_simple_test();
    //counted_test_with_enable_counted_from_this();
    inplace_calculation_test_1();
	inplace_calculation_test_2();
    add_substract_test_1();
	add_substract_test_2();
    hashing_test_1(3, crc32<dixelu::bitreverse::int_tracker>);
	hashing_test_2(3, crc32<dixelu::bitreverse2::int_tracker>);

	auto res = crc32<dixelu::bitreverse2::int_tracker>({25});
	std::string res_string = res.__to_string();
	std::cout << res_string << std::endl;

	/*auto res = crc32<dixelu::bitreverse2::int_tracker>({65, 88});
	std::string res_string = res.__to_string();
	std::cout << res_string << std::endl;
	std::reverse(res_string.begin(), res_string.end());
	std::cout << res_string << std::endl;*/
    //hashing_test(32, md5);
    //real_md5_reversal();

    return 0;
}
