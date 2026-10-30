# Shared by the inverse_demo CLI tests, which are run with cmake -P and get
# INVERSE_DEMO, WORK and, to compile exported headers, CXX.
cmake_minimum_required(VERSION 3.25)

# Every run starts empty, so that no file left by an earlier run can satisfy a check.
file(REMOVE_RECURSE "${WORK}")
file(MAKE_DIRECTORY "${WORK}")

# run_cli(<exit code> <argument>...) runs inverse_demo, requires the exit
# code, and leaves its output in cli_output and cli_error.
function(run_cli expected)
    execute_process(COMMAND "${INVERSE_DEMO}" ${ARGN}
        RESULT_VARIABLE result OUTPUT_VARIABLE output ERROR_VARIABLE error TIMEOUT 20)
    if(NOT "${result}" STREQUAL "${expected}")
        message(FATAL_ERROR "inverse_demo ${ARGN}: exit ${result}\n${output}\n${error}")
    endif()
    set(cli_output "${output}" PARENT_SCOPE)
    set(cli_error "${error}" PARENT_SCOPE)
endfunction()

# run_cli_keeping(<path> <exit code> <argument>...) is run_cli, after which
# the file at <path> must be byte-for-byte unchanged.
function(run_cli_keeping path expected)
    file(SHA256 "${path}" before)
    run_cli(${expected} ${ARGN})
    file(SHA256 "${path}" after)
    if(NOT before STREQUAL after)
        message(FATAL_ERROR "inverse_demo ${ARGN} changed ${path}")
    endif()
    set(cli_output "${cli_output}" PARENT_SCOPE)
    set(cli_error "${cli_error}" PARENT_SCOPE)
endfunction()

# The standard output of the last run_cli, without surrounding whitespace,
# must be <expected>.
function(expect_output expected message)
    string(STRIP "${cli_output}" actual)
    if(NOT actual STREQUAL expected)
        message(FATAL_ERROR "${message}: got '${actual}'")
    endif()
endfunction()

function(expect_match text pattern message)
    if(NOT text MATCHES "${pattern}")
        message(FATAL_ERROR "${message}")
    endif()
endfunction()

function(expect_sha256 path expected message)
    file(SHA256 "${path}" actual)
    if(NOT actual STREQUAL expected)
        message(FATAL_ERROR "${message}")
    endif()
endfunction()

# compile_and_run(<source>) compiles tests/cmake/<source> with only WORK,
# which holds the exported headers, on its include path; it must exit with 0.
function(compile_and_run source)
    execute_process(COMMAND "${CXX}" -std=c++23 -O1 -Wall -Wextra -pedantic -static
            "-I${WORK}" "${CMAKE_CURRENT_FUNCTION_LIST_DIR}/${source}" -o standalone.exe
        WORKING_DIRECTORY "${WORK}" RESULT_VARIABLE result OUTPUT_VARIABLE output ERROR_VARIABLE error TIMEOUT 60)
    if(NOT "${result}" STREQUAL "0")
        message(FATAL_ERROR "Compiling ${source} failed: ${result}\n${output}\n${error}")
    endif()
    execute_process(COMMAND "${WORK}/standalone.exe" RESULT_VARIABLE result TIMEOUT 10)
    if(NOT "${result}" STREQUAL "0")
        message(FATAL_ERROR "${source} failed its checks of the exported inverse: exit ${result}")
    endif()
endfunction()
