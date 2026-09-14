file(MAKE_DIRECTORY "${WORK}")
function(run_cli expected)
    execute_process(COMMAND "${INVERSE_DEMO}" ${ARGN}
        RESULT_VARIABLE result OUTPUT_VARIABLE output ERROR_VARIABLE error TIMEOUT 20)
    if(NOT "${result}" STREQUAL "${expected}")
        message(FATAL_ERROR "inverse_demo ${ARGN}: exit ${result}\n${output}\n${error}")
    endif()
    set(cli_output "${output}" PARENT_SCOPE)
    set(cli_error "${error}" PARENT_SCOPE)
endfunction()

run_cli(0 build crc32 "6?" "${WORK}/source.bri")
file(SHA256 "${WORK}/source.bri" original)
file(WRITE "${WORK}/limited.bri" "existing artifact")
run_cli(1 synthesize-cegis "${WORK}/source.bri" "${WORK}/limited.bri" --max-solver-steps=1)
if(NOT cli_error MATCHES "Certification: incomplete")
    message(FATAL_ERROR "Exhausted CEGIS query did not report incomplete certification")
endif()
file(READ "${WORK}/limited.bri" retained)
if(NOT retained STREQUAL "existing artifact")
    message(FATAL_ERROR "Failed CEGIS overwrote an existing artifact")
endif()
run_cli(1 synthesize-cegis "${WORK}/source.bri" "${WORK}/source.bri")
run_cli(1 synthesize-cegis "${WORK}/source.bri" "${WORK}/invalid.bri" --degree=3)
run_cli(0 synthesize-cegis "${WORK}/source.bri" "${WORK}/learned.bri" --degree=1)
if(NOT cli_output MATCHES "completed UNSAT miter")
    message(FATAL_ERROR "CEGIS CLI did not report completed certification")
endif()
file(SHA256 "${WORK}/source.bri" after)
if(NOT original STREQUAL after)
    message(FATAL_ERROR "CEGIS changed the source artifact")
endif()
run_cli(0 inspect "${WORK}/learned.bri")
if(NOT cli_output MATCHES "counterexample-learned polynomial")
    message(FATAL_ERROR "Learned backend was not persisted")
endif()
# Standard reflected CRC32 of the single byte 'a'.
run_cli(0 solve "${WORK}/learned.bri" e8b7be43)
string(STRIP "${cli_output}" recovered)
if(NOT recovered STREQUAL "61")
    message(FATAL_ERROR "Learned CRC32 did not recover the baked nibble input")
endif()
run_cli(2 solve "${WORK}/learned.bri" 00000000)
run_cli(1 solve "${WORK}/learned.bri" e8b7be43 --all)
run_cli(1 export-cpp "${WORK}/learned.bri" "${WORK}/learned.bri")
run_cli(0 export-cpp "${WORK}/learned.bri" "${WORK}/learned.h")
file(WRITE "${WORK}/standalone.cpp" [=[
#include "learned.h"
#include "learned.h"
#include <cstdint>
std::uint32_t crc32(unsigned char byte)
{
    std::uint32_t crc = 0xffffffffU ^ byte;
    for (unsigned bit = 0; bit < 8; ++bit)
        crc = (crc >> 1) ^ ((crc & 1U) ? 0xedb88320U : 0U);
    return ~crc;
}
int main()
{
    for (unsigned byte = 0; byte < 256; ++byte) {
        const auto crc = crc32(static_cast<unsigned char>(byte));
        std::array<bool, 32> target{};
        for (size_t bit = 0; bit < 32; ++bit) target[bit] = (crc >> bit) & 1U;
        const auto input = bitreverse_inverse(target);
        if ((byte >= 0x60 && byte <= 0x6f) != input.has_value()) return 1;
        if (input) for (size_t bit = 0; bit < 8; ++bit)
            if ((*input)[bit] != bool((byte >> bit) & 1U)) return 2;
    }
    return bitreverse_inverse(std::array<bool, 32>{}) ? 3 : 0;
}
]=])
execute_process(COMMAND "${CXX}" -std=c++23 -O1 -Wall -Wextra -pedantic
    -static -static-libgcc -static-libstdc++ standalone.cpp -o standalone.exe
    WORKING_DIRECTORY "${WORK}" RESULT_VARIABLE result
    OUTPUT_VARIABLE output ERROR_VARIABLE error TIMEOUT 60)
if(NOT "${result}" STREQUAL "0")
    message(FATAL_ERROR "Learned export compilation failed: ${result}\n${output}\n${error}")
endif()
execute_process(COMMAND "${WORK}/standalone.exe" RESULT_VARIABLE result TIMEOUT 10)
if(NOT "${result}" STREQUAL "0")
    message(FATAL_ERROR "Learned export failed independent CRC32 validation: ${result}")
endif()
