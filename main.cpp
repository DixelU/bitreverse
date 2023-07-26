#include <iostream>

#include "bitreverse.h"
#include "counted_ptr.h"

void counted_simple_test()
{
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
    dixelu::bitreverse::int_tracker<32> a = 2173, b = 234789, c = 1, u = dixelu::bitreverse::unknown;
    b = a ^ u | c;
    a |= b;
    b ^= c;
    c = a | b & c;
    auto bit = (dixelu::bitreverse::bit_tracker)c;
    std::cout << "Bit depth: " << bit.bit_state->max_depth << std::endl;
}

int main()
{
    counted_simple_test();
    counted_test_with_enable_counted_from_this();
    inplace_calculation_test();

    return 0;
}
