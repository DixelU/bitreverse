file(MAKE_DIRECTORY "${WORK}")
if(NOT DEFINED METHOD)
    set(METHOD synthesize)
endif()

function(run_cli expected)
    execute_process(COMMAND "${INVERSE_DEMO}" ${ARGN}
        RESULT_VARIABLE result OUTPUT_VARIABLE output ERROR_VARIABLE error TIMEOUT 15)
    if(NOT "${result}" STREQUAL "${expected}")
        message(FATAL_ERROR "inverse_demo ${ARGN}: exit ${result}\n${output}\n${error}")
    endif()
    set(cli_output "${output}" PARENT_SCOPE)
    set(cli_error "${error}" PARENT_SCOPE)
endfunction()

run_cli(0 build md5 "6d6435??" "${WORK}/source.bri")
file(SHA256 "${WORK}/source.bri" original)
file(WRITE "${WORK}/limited.bri" "existing artifact")
run_cli(1 ${METHOD} "${WORK}/source.bri" "${WORK}/limited.bri" --max-nodes=1)
if(METHOD STREQUAL "synthesize" AND NOT cli_error MATCHES "forward: failed")
    message(FATAL_ERROR "Limited synthesis did not identify its failing construction phase")
endif()
file(READ "${WORK}/limited.bri" retained)
if(NOT retained STREQUAL "existing artifact")
    message(FATAL_ERROR "Failed synthesis overwrote an existing artifact")
endif()
run_cli(1 ${METHOD} "${WORK}/source.bri" "${WORK}/source.bri")
if(METHOD STREQUAL "synthesize-selector")
    run_cli(1 ${METHOD} "${WORK}/source.bri" "${WORK}/absent.bri" --max-assignments=1)
    if(EXISTS "${WORK}/absent.bri")
        message(FATAL_ERROR "Domain-limited selector created an incomplete artifact")
    endif()
    run_cli(0 build md5 "6d6435?1" "${WORK}/nibble-source.bri")
    if(NOT cli_output MATCHES "unknown bits: 4")
        message(FATAL_ERROR "Known nibbles were not baked into the selector source")
    endif()
    run_cli(0 ${METHOD} "${WORK}/nibble-source.bri" "${WORK}/nibble.bri")
    run_cli(0 solve "${WORK}/nibble.bri" b682c4079aa8cae8524986bf8de47f5c)
    string(STRIP "${cli_output}" nibble_recovered)
    if(NOT nibble_recovered STREQUAL "6d643521")
        message(FATAL_ERROR "Nibble-specialized inverse did not recover md5!")
    endif()
    run_cli(2 solve "${WORK}/nibble.bri" 0b5aeacaa0f4f17d5fd7a8ea0a855e1f)
endif()
run_cli(0 ${METHOD} "${WORK}/source.bri" "${WORK}/md5.bri")
file(SHA256 "${WORK}/source.bri" after)
if(NOT original STREQUAL after)
    message(FATAL_ERROR "Synthesis modified the input artifact")
endif()
run_cli(0 solve "${WORK}/md5.bri" b682c4079aa8cae8524986bf8de47f5c)
string(STRIP "${cli_output}" recovered)
if(NOT recovered STREQUAL "6d643521")
    message(FATAL_ERROR "Synthesized MD5 inverse did not recover md5!")
endif()
run_cli(0 solve "${WORK}/md5.bri" 0b5aeacaa0f4f17d5fd7a8ea0a855e1f)
string(STRIP "${cli_output}" recovered)
if(NOT recovered STREQUAL "6d64353f")
    message(FATAL_ERROR "Synthesized inverse could not serve a second target")
endif()
run_cli(2 solve "${WORK}/md5.bri" 00000000000000000000000000000000)
run_cli(1 export-cpp "${WORK}/md5.bri" "${WORK}/md5.bri")
file(WRITE "${WORK}/unsynthesized.h" "existing header")
run_cli(1 export-cpp "${WORK}/source.bri" "${WORK}/unsynthesized.h")
file(READ "${WORK}/unsynthesized.h" retained)
if(NOT retained STREQUAL "existing header")
    message(FATAL_ERROR "Rejected export overwrote an existing header")
endif()
run_cli(0 export-cpp "${WORK}/md5.bri" "${WORK}/md5_inverse.h")

# The exported header is compiled in isolation, without the project include path.
# Digest vectors below were cross-checked using .NET's MD5 implementation.
file(WRITE "${WORK}/standalone.cpp" [=[
#include "md5_inverse.h"
#include "md5_inverse.h" // Generated headers must tolerate repeated inclusion.
#include <array>
#include <string_view>

bool check(std::string_view digest, unsigned last_byte)
{
    std::array<bool, 128> target{};
    for (size_t digit = 0; digit < digest.size(); ++digit) {
        char c = digest[digest.size() - digit - 1];
        unsigned nibble = c <= '9' ? c - '0' : c - 'a' + 10;
        for (size_t bit = 0; bit < 4; ++bit)
            target[digit * 4 + bit] = ((nibble >> bit) & 1) != 0;
    }
    const auto recovered = bitreverse_inverse(target);
    if (!recovered) return false;
    const std::array<unsigned, 4> expected{'m', 'd', '5', last_byte};
    for (size_t byte = 0; byte < expected.size(); ++byte)
        for (size_t bit = 0; bit < 8; ++bit)
            if ((*recovered)[byte * 8 + bit] != (((expected[byte] >> bit) & 1) != 0))
                return false;
    return true;
}

int main()
{
    return check("b682c4079aa8cae8524986bf8de47f5c", '!') &&
           check("0b5aeacaa0f4f17d5fd7a8ea0a855e1f", '?') &&
           !bitreverse_inverse(std::array<bool, 128>{}) ? 0 : 1;
}
]=])
execute_process(COMMAND "${CXX}" -std=c++23 -O1 -Wall -Wextra -pedantic
    -static -static-libgcc -static-libstdc++ standalone.cpp -o standalone.exe
    WORKING_DIRECTORY "${WORK}" RESULT_VARIABLE result
    OUTPUT_VARIABLE output ERROR_VARIABLE error TIMEOUT 60)
if(NOT "${result}" STREQUAL "0")
    message(FATAL_ERROR "Standalone inverse compilation failed: ${result}\n${output}\n${error}")
endif()
execute_process(COMMAND "${WORK}/standalone.exe" RESULT_VARIABLE result TIMEOUT 10)
if(NOT "${result}" STREQUAL "0")
    message(FATAL_ERROR "Standalone inverse returned incorrect results: ${result}")
endif()
