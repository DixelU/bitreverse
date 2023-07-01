#include <iostream>

#include "bitreverse.h"

void counted_simple_test()
{
    auto t = dixelu::make_counted<int>(1);
    auto v = t;
    auto q = v;

    q = t;
    t = v;
    q = v;
    v = t;
    v = v;

    std::cout << *q << ":" << q.count() << std::endl;
}

int main()
{
    counted_simple_test();


    return 0;
}
