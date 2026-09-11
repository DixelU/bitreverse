cmake_minimum_required(VERSION 3.25)
project(bitreverse)

set(CMAKE_CXX_STANDARD 23)

add_executable(bitreverse main.cpp)

target_compile_options(bitreverse PUBLIC -Wall -Wextra -Wuninitialized -pedantic -fstack-protector-all -O3 -ggdb)
target_link_options(bitreverse PRIVATE -static-libgcc -static-libstdc++ -static)
#target_link_options(bitreverse PUBLIC -fsanitize=address)

add_executable(md5_reverse_demo md5_demo.cpp)
target_compile_options(md5_reverse_demo PUBLIC -Wall -Wextra -Wuninitialized -pedantic -fstack-protector-all -O3 -ggdb)
target_link_options(md5_reverse_demo PRIVATE -static-libgcc -static-libstdc++ -static)

add_executable(inverse_demo inverse_demo.cpp)
target_compile_options(inverse_demo PUBLIC -Wall -Wextra -Wuninitialized -pedantic -fstack-protector-all -O3 -ggdb)
target_link_options(inverse_demo PRIVATE -static-libgcc -static-libstdc++ -static)

add_executable(synthesis_benchmark synthesis_benchmark.cpp)
target_compile_options(synthesis_benchmark PRIVATE -Wall -Wextra -pedantic -O3)
target_link_options(synthesis_benchmark PRIVATE -static-libgcc -static-libstdc++ -static)

add_executable(mrev multiplication_reversal.cpp)
target_compile_options(mrev PUBLIC -Wall -Wextra -Wuninitialized -pedantic -fstack-protector-all -O3 -ggdb)
target_link_options(mrev PRIVATE -static-libgcc -static-libstdc++ -static)

add_executable(int_tracker_benchmark int_tracker_benchmark.cpp)
target_compile_options(int_tracker_benchmark PUBLIC -Wall -Wextra -Wuninitialized -pedantic -fstack-protector-all -O3 -ggdb)
target_link_options(int_tracker_benchmark PRIVATE -static-libgcc -static-libstdc++ -static)

include(CTest)

if(BUILD_TESTING)
    add_executable(buffered_object_pool_tests buffered_object_pool_tests.cpp)
    target_compile_options(buffered_object_pool_tests PUBLIC -Wall -Wextra -Wuninitialized -pedantic)
    target_link_options(buffered_object_pool_tests PRIVATE -static-libgcc -static-libstdc++ -static)
    add_test(NAME buffered_object_pool_tests COMMAND buffered_object_pool_tests)
    set_tests_properties(buffered_object_pool_tests PROPERTIES TIMEOUT 10)

    add_executable(bitreverse_tests tests.cpp)
    target_compile_options(bitreverse_tests PUBLIC -Wall -Wextra -Wuninitialized -pedantic -fstack-protector-all -O3 -ggdb)
    target_link_options(bitreverse_tests PRIVATE -static-libgcc -static-libstdc++ -static)
    add_test(NAME bitreverse_tests COMMAND bitreverse_tests)
    set_tests_properties(bitreverse_tests PROPERTIES TIMEOUT 10)

    add_executable(inverse_tests inverse_tests.cpp)
    target_compile_options(inverse_tests PUBLIC -Wall -Wextra -Wuninitialized -pedantic -fstack-protector-all -O3 -ggdb)
    target_link_options(inverse_tests PRIVATE -static-libgcc -static-libstdc++ -static)
    add_test(NAME inverse_tests COMMAND inverse_tests)
    set_tests_properties(inverse_tests PROPERTIES TIMEOUT 30)

    add_executable(affine_inverse_tests affine_inverse_tests.cpp)
    target_compile_options(affine_inverse_tests PUBLIC -Wall -Wextra -Wuninitialized -pedantic -O3)
    target_link_options(affine_inverse_tests PRIVATE -static-libgcc -static-libstdc++ -static)
    add_test(NAME affine_inverse_tests COMMAND affine_inverse_tests)
    set_tests_properties(affine_inverse_tests PROPERTIES TIMEOUT 15)

    add_executable(synthesis_tests synthesis_tests.cpp)
    target_compile_options(synthesis_tests PRIVATE -Wall -Wextra -Wuninitialized -pedantic -O3)
    target_link_options(synthesis_tests PRIVATE -static-libgcc -static-libstdc++ -static)
    add_test(NAME synthesis_tests COMMAND synthesis_tests)
    set_tests_properties(synthesis_tests PROPERTIES TIMEOUT 30)

    add_test(NAME synthesis_cpp_export
        COMMAND ${CMAKE_COMMAND}
            "-DINVERSE_DEMO=$<TARGET_FILE:inverse_demo>"
            "-DCXX=${CMAKE_CXX_COMPILER}"
            "-DWORK=${CMAKE_CURRENT_BINARY_DIR}/synthesis-export-test"
            -P "${CMAKE_CURRENT_SOURCE_DIR}/synthesis_cli_test.cmake")
    set_tests_properties(synthesis_cpp_export PROPERTIES TIMEOUT 90)

    add_test(NAME inverse_cli_persistence
        COMMAND ${CMAKE_COMMAND}
            "-DINVERSE_DEMO=$<TARGET_FILE:inverse_demo>"
            "-DWORK=${CMAKE_CURRENT_BINARY_DIR}/inverse-cli-test"
            -P "${CMAKE_CURRENT_SOURCE_DIR}/inverse_cli_test.cmake")
    set_tests_properties(inverse_cli_persistence PROPERTIES TIMEOUT 30)

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
            buffered_object_pool_tests
            bitreverse_tests
            inverse_tests
            affine_inverse_tests
            synthesis_tests
            synthesis_cpp_export
            inverse_cli_persistence
            int_tracker_performance_regression
            PROPERTIES
                ENVIRONMENT_MODIFICATION
                    "PATH=path_list_prepend:${BITREVERSE_MINGW_RUNTIME_DIR}")
    endif()
endif()
