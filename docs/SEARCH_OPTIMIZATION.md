# General solver optimization measurements

On 2026-10-13, the optimized solver used **22–35% less peak requested heap
memory** and ran **1.36–3.82 times faster** on the ten bounded cases below.
These changes optimize the general circuit solver; they do not substitute a
native hash brute-force implementation or a precomputed inverse.

## Changes

- Parent adjacency uses contiguous edges and offsets, removing thousands of
  small allocations while preserving parent order and duplicate argument slots.
- Gate opcodes are cached in a byte array, and constants are initialized once
  in the assignment array, avoiding source-expression pointer chasing in search.
- DPLL propagates directly from its undo trail. Its iterative decision stack
  avoids recursive search frames, and variable selection resumes after the
  assigned ancestor prefix instead of repeatedly scanning from the beginning.
- Affine propagation substitutes known atoms and retains only independent
  equations in reusable flat buffers. A backward reduction still extracts all
  singleton consequences. Every pass starts from current assignments, including
  after backtracking. Inactive affine reasoning allocates no atom maps.
- CDCL clauses reference a shared literal arena rather than allocating a vector
  for each gate reason, learned clause, and blocking clause.
- Both engines move collected models into the return value, avoiding a second
  full copy of a collected solution set.

Branch ordering, phase preference, result enumeration, callback cancellation,
affine activation limits, and the public APIs are retained. The saved inverse
format is unchanged. Work counters can decrease because redundant work has
been removed; identical numeric budgets need not interrupt at identical places
across versions.

## Measurement method

Windows x64, Intel Core i5-13600K, MinGW GCC 16.1.0, C++23, `-O3 -DNDEBUG`, static
linking. The baseline is revision `f983a5e`, rebuilt with the same compiler and
current benchmark source. Seven measured runs per case and version follow one
warm-up per binary, with alternating before/after order. Times are medians.
No builds or test suites ran concurrently with the final measurements.

Time covers `assert_equality`: equality construction, circuit compilation,
engine initialization, search, and cleanup. Forward hash construction and the
separate circuit-metrics passes are excluded. Allocation counts come from a
separate build with `BITREVERSE_TRACK_ALLOCATIONS`; instrumented timings are
not used in the table.

Memory is the peak additional **requested C++ heap bytes** above the live
allocation count just before solving. It includes temporary compilation and
search buffers, but excludes the already-built forward graph, allocator
overhead, stack, and other process memory. It is not RSS or total application
memory. The benchmark's allocation instrumentation is single-threaded.

The templates are `abcd` for 32-bit CRC32 and all cryptographic hashes,
`bitrev!` for 56-bit CRC32, and `abcdefghijkl` for 96-bit CRC32. Unknown bits
replace low bits of the leading bytes, as in the existing hash-reversal
benchmark. Every reported preimage is independently checked with Python
`hashlib` or `zlib`, including preservation of fixed template bits.

CRC32 has a 200-million-step budget; the other cases have 20 million steps.
`Limit` is an unfinished search, not UNSAT or a recovered preimage. Both
versions had the same SAT/limit outcomes and decision/conflict counts in this
matrix. MD5's affine option exceeds the atom cap and therefore uses DPLL.

## Results

| Algorithm / unknown bits / engine | Outcome | Before ms | After ms | Speedup | Before MiB | After MiB | Memory reduction |
|---|---|---:|---:|---:|---:|---:|---:|
| CRC32 / 32 / affine | SAT | 0.239 | 0.101 | 2.37x | 0.117 | 0.077 | 34.5% |
| CRC32 / 56 / affine | SAT | 3.944 | 1.143 | 3.45x | 0.171 | 0.112 | 34.3% |
| CRC32 / 96 / affine | SAT | 17.721 | 4.639 | 3.82x | 0.303 | 0.198 | 34.8% |
| MD5 / 8 / DPLL | SAT | 241.476 | 177.559 | 1.36x | 6.069 | 4.560 | 24.9% |
| MD5 / 8 / affine option | SAT | 248.317 | 179.231 | 1.39x | 6.774 | 4.560 | 32.7% |
| MD5 / 8 / CDCL | Limit | 218.840 | 150.887 | 1.45x | 18.581 | 13.729 | 26.1% |
| MD5 / 12 / DPLL | Limit | 472.873 | 315.385 | 1.50x | 6.095 | 4.586 | 24.8% |
| SHA-1 / 8 / DPLL | SAT | 327.844 | 220.006 | 1.49x | 7.302 | 5.727 | 21.6% |
| SHA-256 / 4 / DPLL | SAT | 142.125 | 71.165 | 2.00x | 21.636 | 15.084 | 30.3% |
| SHA-256 / 4 / CDCL | SAT | 211.418 | 99.262 | 2.13x | 61.360 | 44.319 | 27.8% |

For CRC32/96, allocation calls fell from 62,865 to 1,702; SHA-256/CDCL fell from
842,655 to 138,822. Raw timings, statuses, counters, recovered inputs, and
allocation measurements are in [the CSV](../experiments/search-optimization.csv).
Wall times remain sensitive to scheduling, CPU frequency, and background work;
the sub-millisecond CRC32 case is especially noisy.

## Validation

- Full Release build and all 21 CTest tests passed, including inverse
  persistence, exported C++, arithmetic, hashes, partial assertions, synthesis,
  and deterministic budget/callback behavior.
- `solver_search_tests` compares complete model sets against independent truth
  tables for 100 random six-input circuits, eight targets, and three engines
  (2,400 queries), checking duplicates and unreachable targets as well as SAT.
- Another 256 target queries test 130 affine inputs with six free bits spanning
  64-bit word boundaries; all expected model sets come from exhaustive tables.
- A 20,000-input first-model test exercises iterative DPLL under a 200,000-step
  budget. Parent-adjacency checks cover empty graphs and duplicate edges.
- The search tests also passed with GCC checked containers and assertions
  (`-D_GLIBCXX_DEBUG -D_GLIBCXX_ASSERTIONS`). This is not a sanitizer run.

## Reproducing

From the repository root in a shell with GCC, Python, and CMake available,
extract the old headers into a separate build directory. These commands do not
change the working checkout. `mkdir -p` below assumes a POSIX shell; on Windows,
create the directory with PowerShell `New-Item -ItemType Directory -Force`.

```sh
mkdir -p build-search-optimization/baseline
git archive --format=tar --output=build-search-optimization/baseline.tar f983a5e include
tar -xf build-search-optimization/baseline.tar -C build-search-optimization/baseline
g++ -std=c++23 -O3 -DNDEBUG -Ibuild-search-optimization/baseline/include benchmarks/hash_reversal_benchmark.cpp -static -o build-search-optimization/before.exe
g++ -std=c++23 -O3 -DNDEBUG -Iinclude benchmarks/hash_reversal_benchmark.cpp -static -o build-search-optimization/after.exe
g++ -std=c++23 -O3 -DNDEBUG -DBITREVERSE_TRACK_ALLOCATIONS -Ibuild-search-optimization/baseline/include benchmarks/hash_reversal_benchmark.cpp -static -o build-search-optimization/before-memory.exe
g++ -std=c++23 -O3 -DNDEBUG -DBITREVERSE_TRACK_ALLOCATIONS -Iinclude benchmarks/hash_reversal_benchmark.cpp -static -o build-search-optimization/after-memory.exe
python benchmarks/compare_search.py --before build-search-optimization/before.exe --after build-search-optimization/after.exe --before-memory build-search-optimization/before-memory.exe --after-memory build-search-optimization/after-memory.exe --repeats 7 --output build-search-optimization/comparison.csv
```

The memory arguments are optional. Each subprocess has a 60-second timeout.
The CSV is saved after every completed case, preserving earlier measurements
if a later case fails.

## Remaining costs

Hard nonlinear search still has exponential worst-case behavior. These changes
make the existing search cheaper; they do not establish general large-domain
hash inversion. Two benchmark cases still exhaust their budgets. CDCL's learned
and model-blocking clauses can still grow during long runs, and affine forms
still use a dense node-by-atom coefficient matrix. Potential next experiments
include bounded learned-clause retention, implicit gate reasons, compact node
IDs, incremental affine elimination, and different branching policies. Each
needs separate correctness checks and measurements across more than hashes.
