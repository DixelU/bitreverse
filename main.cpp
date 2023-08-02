#include <iostream>

#include "bitreverse.h"
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

void inplace_calculation_test()
{
    std::cout << std::endl << "inplace_calculation_test" << std::endl;
    dixelu::bitreverse::int_tracker<32> a = 2173, b = 234789, c = 1, u = dixelu::bitreverse::unknown;
    b = a ^ u;
    std::cout << b.__to_string() << std::endl;
    a |= b;
    b ^= c;
    c = a | b & c;
    std::cout << c.__to_string() << std::endl;
    std::cout << "Bit depth: " << c.__max_depth() << std::endl;
}

dixelu::bitreverse::itu32 crc32(std::vector<dixelu::bitreverse::itu8> message)
{
    dixelu::bitreverse::itu32 byte, mask;

    dixelu::bitreverse::itu32 crc = 0xFFFFFFFF;
    const dixelu::bitreverse::itu32 mask_const = 0xEDB88320;

    for (auto& ch : message)
    {
        /*byte = dixelu::bitreverse::itu32(ch);

        std::cout << byte.__to_string() << std::endl;
        std::cout << crc.__to_string() << std::endl;
        std::cout << "XOR: " << std::endl;
        crc = crc ^ byte;
        std::cout << crc.__to_string() << std::endl;

        for (int j = 7; j >= 0; j--)
        {
            std::cout << j << std::endl;

            std::cout << '\t' << crc.__to_string() << std::endl;
            auto crcn1 = crc & 1;
            std::cout << '\t' << "CRC AND 1: " << std::endl;
            std::cout << '\t' << crcn1.__to_string() << std::endl;

            std::cout << '\t' << crcn1.__to_string() << std::endl;
            std::cout << '\t' << "NEGATE CRCN1: " << std::endl;
            mask = -crcn1;
            std::cout << '\t' << mask.__to_string() << std::endl;

            std::cout << '\t' << crc.__to_string() << std::endl;
            auto crcs1 = crc >> 1;
            std::cout << '\t' << "SHIFT LEFT CRC: " << std::endl;
            std::cout << '\t' << crcs1.__to_string() << std::endl;

            std::cout << '\t' << mask_const.__to_string() << std::endl;
            std::cout << '\t' << mask.__to_string() << std::endl;
            auto masknmask = mask_const & mask;
            std::cout << '\t' << "MASKCONST AND MASK: " << std::endl;
            std::cout << '\t' << masknmask.__to_string() << std::endl;

            std::cout << '\t' << crcs1.__to_string() << std::endl;
            std::cout << '\t' << masknmask.__to_string() << std::endl;
            crc = crcs1 ^ masknmask;
            std::cout << '\t' << "CRCS1 XOR MASKNMASK: " << std::endl;
            std::cout << '\t' << crc.__to_string() << std::endl;
        }*/
        byte = dixelu::bitreverse::itu32(ch);
        crc = crc ^ byte;

        for (int j = 7; j >= 0; j--)
        {
            mask = -(crc & 1);
            crc = (crc >> 1) ^ (mask_const & mask);
        }
    }
    return ~crc;
}

void crc32_test(size_t count = 2)
{
    std::cout << std::endl << "crc32_test. string size: " << count << std::endl;

    std::vector<dixelu::bitreverse::itu8> unknown_string(count, dixelu::bitreverse::unknown);
    
    for (auto& ch : unknown_string)
        ch &= 0x7F;//ascii symbols

    dixelu::bitreverse::itu32 result = crc32(unknown_string);

    std::cout << result.__to_string() << " " << result.__max_depth() << std::endl;
}

void add_substract_test()
{
    std::cout << std::endl << "add_substract_test" << std::endl;
    dixelu::bitreverse::itu16 a, b;
    a = dixelu::bitreverse::unknown;
    b = 16;
    auto apb = a + b;
    auto amb = a - b;
    auto ma = -a;
    auto nota = ~a;

    std::cout << apb.__to_string() << std::endl;
    std::cout << amb.__to_string() << std::endl;
    std::cout << ma.__to_string() << std::endl;
    std::cout << nota.__to_string() << std::endl;
}

int main()
{
    counted_simple_test();
    counted_test_with_enable_counted_from_this();
    inplace_calculation_test();
    add_substract_test();
    crc32_test(16);

    return 0;
}
