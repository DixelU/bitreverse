cmake_minimum_required(VERSION 3.25)
project(bitreverse)

set(CMAKE_CXX_STANDARD 23)

add_executable(bitreverse main.cpp)

target_compile_options(bitreverse PUBLIC -Wall -Wextra -Wuninitialized -pedantic -fstack-protector-all -O3 -ggdb)
#target_link_options(bitreverse PUBLIC -fsanitize=address)

add_executable(md5_reverse_demo md5_demo.cpp)
target_compile_options(md5_reverse_demo PRIVATE -Wall -Wextra -Wuninitialized -pedantic -fstack-protector-all -O3 -ggdb)

add_executable(mrev multiplication_reversal.cpp)
target_compile_options(mrev PRIVATE -Wall -Wextra -Wuninitialized -pedantic -fstack-protector-all -O3 -ggdb)

add_executable(int_tracker_benchmark int_tracker_benchmark.cpp)
target_compile_options(int_tracker_benchmark PRIVATE -Wall -Wextra -Wuninitialized -pedantic -fstack-protector-all -O3 -ggdb)

include(CTest)

if(BUILD_TESTING)
    add_executable(bitreverse_tests tests.cpp)
    target_compile_options(bitreverse_tests PRIVATE -Wall -Wextra -Wuninitialized -pedantic -fstack-protector-all -O3 -ggdb)
    add_test(NAME bitreverse_tests COMMAND bitreverse_tests)
    set_tests_properties(bitreverse_tests PROPERTIES TIMEOUT 10)

    add_test(NAME int_tracker_performance_regression COMMAND int_tracker_benchmark)
    set_tests_properties(
        int_tracker_performance_regression
        PROPERTIES
            LABELS performance
            TIMEOUT 15)

    if(MINGW)
        get_filename_component(
            BITREVERSE_MINGW_RUNTIME_DIR
            "${CMAKE_CXX_COMPILER}"
            DIRECTORY)
        set_tests_properties(
            bitreverse_tests
            int_tracker_performance_regression
            PROPERTIES
                ENVIRONMENT_MODIFICATION
                    "PATH=path_list_prepend:${BITREVERSE_MINGW_RUNTIME_DIR}")
    endif()
endif()
