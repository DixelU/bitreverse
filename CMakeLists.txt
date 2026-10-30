cmake_minimum_required(VERSION 3.25)
project(bitreverse LANGUAGES CXX)

add_library(bitreverse_headers INTERFACE)
add_library(bitreverse::headers ALIAS bitreverse_headers)
target_include_directories(bitreverse_headers INTERFACE "${CMAKE_CURRENT_SOURCE_DIR}/include")
target_compile_features(bitreverse_headers INTERFACE cxx_std_23)

# Helpers shared by the tests, examples and benchmarks; not part of the library.
include_directories("${CMAKE_CURRENT_SOURCE_DIR}/support")

# -O3 even in Debug configurations: the test timeouts assume optimized code.
function(bitreverse_executable name source)
    add_executable(${name} ${source})
    target_link_libraries(${name} PRIVATE bitreverse::headers)
    target_compile_options(${name} PRIVATE -Wall -Wextra -pedantic -O3 -ggdb)
    target_link_options(${name} PRIVATE -static)
endfunction()

bitreverse_executable(bitreverse src/main.cpp)
bitreverse_executable(mrev src/multiplication_reversal.cpp)
foreach(example inverse_demo partial_assert_crc32 partial_assert_md5)
    bitreverse_executable(${example} examples/${example}.cpp)
endforeach()
foreach(benchmark arithmetic_circuit cegis hash_reversal int_tracker multiplication_shape selector synthesis)
    bitreverse_executable(${benchmark}_benchmark benchmarks/${benchmark}_benchmark.cpp)
endforeach()

include(CTest)
if(NOT BUILD_TESTING)
    return()
endif()

# bitreverse_test(<name> [COMMAND <command>...] [TIMEOUT <seconds>] [LABELS <label>...])
# runs the target <name> unless COMMAND is given; TIMEOUT defaults to 30.
function(bitreverse_test name)
    cmake_parse_arguments(PARSE_ARGV 1 test "" "TIMEOUT" "COMMAND;LABELS")
    if(NOT test_COMMAND)
        set(test_COMMAND ${name})
    endif()
    if(NOT test_TIMEOUT)
        set(test_TIMEOUT 30)
    endif()
    add_test(NAME ${name} COMMAND ${test_COMMAND})
    set_tests_properties(${name} PROPERTIES TIMEOUT ${test_TIMEOUT} LABELS "${test_LABELS}")
endfunction()

foreach(suite affine_inverse arithmetic cegis circuit_metrics hash inverse memory multiplication_reversal
        partial_assert selector solver_budget solver_search synthesis)
    bitreverse_executable(${suite}_tests tests/${suite}_tests.cpp)
    bitreverse_test(${suite}_tests)
endforeach()
set_tests_properties(multiplication_reversal_tests PROPERTIES TIMEOUT 60)

# The examples check their own results.
bitreverse_test(partial_assert_crc32)
bitreverse_test(partial_assert_md5)
bitreverse_test(int_tracker_performance_regression COMMAND int_tracker_benchmark TIMEOUT 15 LABELS performance)

# Runs tests/cmake/<script>.cmake against inverse_demo in a work directory of its own.
function(bitreverse_cli_test name script)
    bitreverse_test(${name} TIMEOUT 90 COMMAND ${CMAKE_COMMAND}
        "-DINVERSE_DEMO=$<TARGET_FILE:inverse_demo>"
        "-DCXX=${CMAKE_CXX_COMPILER}"
        "-DWORK=${CMAKE_CURRENT_BINARY_DIR}/${name}"
        ${ARGN}
        -P "${CMAKE_CURRENT_SOURCE_DIR}/tests/cmake/${script}.cmake")
endfunction()

bitreverse_cli_test(synthesis_cpp_export synthesis_cli_test)
bitreverse_cli_test(selector_cpp_export synthesis_cli_test -DMETHOD=synthesize-selector)
bitreverse_cli_test(cegis_cpp_export cegis_cli_test)
bitreverse_cli_test(inverse_cli_persistence inverse_cli_test)

# The CLI tests compile exported headers with g++, which needs the MinGW
# runtime DLLs on PATH.
if(MINGW)
    get_filename_component(mingw_bin "${CMAKE_CXX_COMPILER}" DIRECTORY)
    get_property(all_tests DIRECTORY PROPERTY TESTS)
    set_tests_properties(${all_tests} PROPERTIES
        ENVIRONMENT_MODIFICATION "PATH=path_list_prepend:${mingw_bin}")
endif()
