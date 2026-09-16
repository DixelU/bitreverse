# Each invocation is a new process: the algorithm construction must survive exit.
file(MAKE_DIRECTORY "${WORK}")

function(run_cli expected)
    execute_process(
        COMMAND "${INVERSE_DEMO}" ${ARGN}
        RESULT_VARIABLE result
        OUTPUT_VARIABLE output
        ERROR_VARIABLE error
        TIMEOUT 10)
    if(NOT "${result}" STREQUAL "${expected}")
        message(FATAL_ERROR "inverse_demo ${ARGN}: exit ${result}\n${output}\n${error}")
    endif()
    set(cli_output "${output}" PARENT_SCOPE)
endfunction()

run_cli(0 build crc32 "3132333435????????" "${WORK}/crc32.bri")
run_cli(0 inspect "${WORK}/crc32.bri")
if(NOT cli_output MATCHES "direct affine family; free bits: 0")
    message(FATAL_ERROR "CRC32 prefix inverse was not compiled as a direct inverse")
endif()
run_cli(0 solve "${WORK}/crc32.bri" cbf43926 "--output=${WORK}/matches.hex")
string(STRIP "${cli_output}" recovered)
file(STRINGS "${WORK}/matches.hex" saved)
if(NOT recovered STREQUAL "313233343536373839" OR NOT saved STREQUAL recovered)
    message(FATAL_ERROR "CRC32 inverse failed to recover and persist 123456789")
endif()

run_cli(0 build crc32 "??????????" "${WORK}/family.bri")
run_cli(0 inspect "${WORK}/family.bri")
if(NOT cli_output MATCHES "direct affine family; free bits: 8")
    message(FATAL_ERROR "Five-byte CRC32 inverse lost its free parameters")
endif()
run_cli(0 solve "${WORK}/family.bri" cbf43926 --limit=3)
string(REGEX MATCHALL "[0-9a-f]+" matches "${cli_output}")
list(REMOVE_DUPLICATES matches)
list(LENGTH matches count)
if(NOT count EQUAL 3)
    message(FATAL_ERROR "CRC32 inverse did not emit three different matches")
endif()

run_cli(0 build md5 "6d6435??" "${WORK}/md5.bri" --printable)
run_cli(0 solve "${WORK}/md5.bri" b682c4079aa8cae8524986bf8de47f5c)
string(STRIP "${cli_output}" recovered)
if(NOT recovered STREQUAL "6d643521")
    message(FATAL_ERROR "Persisted MD5 inverse failed to recover md5!")
endif()
run_cli(2 solve "${WORK}/md5.bri" 00000000000000000000000000000000)
run_cli(1 solve "${WORK}/crc32.bri" invalid)
run_cli(1 solve "${WORK}/crc32.bri" cbf43926 "--output=${WORK}/crc32.bri")
run_cli(0 inspect "${WORK}/crc32.bri")
