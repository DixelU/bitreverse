# Benchmarks

Each benchmark is a CMake target in `benchmarks/` and writes its results to
stdout. Timings depend on the machine; node counts and work counters are
deterministic for a given build.

| Executable | Measures | Arguments |
|---|---|---|
| `synthesis_benchmark` | BDD synthesis of small reversible, arithmetic, CRC32 and MD5 (4 to 12 unknown bits) relations: build work, retained sizes, artifact bytes, valid and invalid query time, native forward time (CSV) | None |
| `selector_benchmark` | Compiled selectors for MD5 with 8, 10, 12 and 16 unknown bits, compared with native MD5 and a sorted lookup table; every domain input is checked after save/load (CSV) | `[8\|10\|12\|16\|20]`: one width; 20 runs with raised budgets |
| `cegis_benchmark` | CEGIS on 3- to 24-bit circuits of independent Toffoli updates and on random permutations used as controls (CSV) | `[--include-48] [--max-solver-steps=N] [--output=DIR]`; `--include-48` adds a 48-bit case, `--output` saves the artifacts |
| `hash_reversal_benchmark` | One digest-only preimage search for CRC32, MD5, SHA-1 or SHA-256, and the forward DAG shape before equality (JSON lines) | `ALGORITHM INPUT_HEX UNKNOWN_BITS TARGET_HEX shape\|dpll\|affine\|cdcl MAX_STEPS` |
| `multiplication_shape_benchmark` | 256-bit multiplication by constants on either side: DAG shape and the cost of enumerating all models with `assert_equality` (CSV) | Optional case: `three`, `thirteen`, `dense_odd`, `all_ones`, `twelve`, `shift8`, `zero` |
| `arithmetic_circuit_benchmark` | Nodes, gates, depth and width of `+`, `-`, `*`, `/`, `%` at 8, 32, 64 and 256 bits (CSV) | None |
| `int_tracker_benchmark` | Time per tracker operation, with upper bounds on DAG size and depth; CTest runs it as a regression check | None |

`benchmarks/run_hash_reversal.py` drives `hash_reversal_benchmark` over a
matrix of algorithms, unknown widths and engines, computes targets and checks
recovered inputs with Python's `hashlib` and `zlib`, and writes a CSV:

```sh
python benchmarks/run_hash_reversal.py --exe build/hash_reversal_benchmark --output hash.csv --quick
```

On Windows, pass the path with `.exe`.

Its options are `--max-steps` (default 2000000), `--timeout` (seconds per
process, default 30), `--quick`, `--algorithms`, `--bits`, `--engines` and
`--input-hex` (default `61626364`, "abcd").

Compiling `hash_reversal_benchmark.cpp` with `-DBITREVERSE_TRACK_ALLOCATIONS`
adds peak requested heap bytes and allocation calls of each solve
(`benchmarks/search_allocations.h`). The counter is single-threaded and
excludes allocator overhead; use an uninstrumented build for timings.
