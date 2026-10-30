# Learns a CRC-32 inverse with synthesize-cegis, solves with it, and checks
# its standalone C++ export.
include("${CMAKE_CURRENT_LIST_DIR}/cli_common.cmake")
set(source "${WORK}/source.bri")
set(learned "${WORK}/learned.bri")

run_cli(0 build crc32 "6?" "${source}")
file(SHA256 "${source}" source_hash)
file(WRITE "${WORK}/limited.bri" "existing artifact")
run_cli_keeping("${WORK}/limited.bri" 1 synthesize-cegis "${source}" "${WORK}/limited.bri" --max-solver-steps=1)
expect_match("${cli_error}" "Certification: incomplete" "Exhausted CEGIS query did not report incomplete certification")
run_cli_keeping("${source}" 1 synthesize-cegis "${source}" "${source}")
run_cli(1 synthesize-cegis "${source}" "${WORK}/invalid.bri" --degree=3)
run_cli(0 synthesize-cegis "${source}" "${learned}" --degree=1)
expect_match("${cli_output}" "completed UNSAT miter" "CEGIS CLI did not report completed certification")
expect_sha256("${source}" "${source_hash}" "CEGIS changed the source artifact")

run_cli(0 inspect "${learned}")
expect_match("${cli_output}" "counterexample-learned polynomial" "Learned backend was not persisted")
# Standard reflected CRC32 of the single byte 'a'.
run_cli(0 solve "${learned}" e8b7be43)
expect_output("61" "Learned CRC32 did not recover the baked nibble input")
run_cli(2 solve "${learned}" 00000000)
run_cli(1 solve "${learned}" e8b7be43 --all)

run_cli_keeping("${learned}" 1 export-cpp "${learned}" "${learned}")
run_cli(0 export-cpp "${learned}" "${WORK}/learned.h")
compile_and_run(crc32_export_check.cpp)
