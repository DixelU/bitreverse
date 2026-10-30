# bitreverse

bitreverse is a header-only C++23 library that runs integer algorithms over
symbolic bits and inverts them: given an output, it finds inputs that produce
it. `int_tracker<N>` records fixed-width unsigned arithmetic and bit operations
as a shared Boolean DAG. Concrete values fold to constants; `unknown` creates
input bits. An inverse is compiled once for a fixed algorithm, input length and
set of known bits, saved to a file, and then queried with new outputs.

Scope:

- A match (preimage) is any input with the requested output. A target can have
  many matches or none; the original input is not singled out.
- Affine relations, such as CRC32 over a fixed message length, invert directly.
  Nonlinear relations (MD5, SHA-1, SHA-256) need a search per query, or a
  precomputed inverse whose construction can grow exponentially with the number
  of unknown bits. Small unknown domains are practical; general hash inversion
  is not.
- Widths, loop counts and control flow are fixed when the algorithm runs: C++
  code cannot branch on an unknown bit.
- Single-threaded: reference counts and node pools are not synchronized.
- GCC (including MinGW-w64) only.

## Build

```sh
cmake -S . -B build -G Ninja -DCMAKE_BUILD_TYPE=Release
cmake --build build
ctest --test-dir build --output-on-failure
```

This needs GCC with C++23 support and CMake 3.25 or newer. To use the headers
from another CMake project, link the `bitreverse::headers` INTERFACE target:

```cmake
add_subdirectory(path/to/bitreverse)
target_link_libraries(your_target PRIVATE bitreverse::headers)
```

Without CMake, add `include/` to the include path: `g++ -std=c++23 -Iinclude app.cpp`.

## Command-line quickstart

`build/inverse_demo` builds and queries inverses of CRC32 and MD5 over messages
given as hex patterns, where each `?` is one unknown nibble.

```sh
# CRC32 of "12345" followed by four unknown bytes: a direct affine inverse.
build/inverse_demo build crc32 "3132333435????????" crc.bri
build/inverse_demo solve crc.bri cbf43926                           # 313233343536373839

# MD5 of "md5" followed by one printable byte: one search per query.
build/inverse_demo build md5 "6d6435??" md5.bri --printable
build/inverse_demo solve md5.bri b682c4079aa8cae8524986bf8de47f5c   # 6d643521

# Do the work at build time instead, then export a standalone C++ function.
build/inverse_demo synthesize md5.bri md5-synth.bri
build/inverse_demo solve md5-synth.bri b682c4079aa8cae8524986bf8de47f5c
build/inverse_demo export-cpp md5-synth.bri md5_inverse.h
```

Matches are printed to stdout as hex messages, one per line (`123456789` and
`md5!` above). Status goes to stderr; exit code 2 means no input matches. All
commands and options are in [docs/CLI.md](docs/CLI.md).

## C++ example

```cpp
#include <fstream>
#include <iostream>
#include "inverse.h"

namespace br = dixelu::bitreverse;
namespace inv = br::inversion;

int main()
{
    const br::itu8 x = br::unknown;                  // eight unknown bits
    const br::itu8 y = x * x + br::itu8(7);          // any algorithm on tracked values

    // Build once and save. bits_of lists bits least significant first.
    const auto program = inv::program::compile(inv::bits_of(x), inv::bits_of(y));
    {
        std::ofstream file("square.bri", std::ios::binary);
        program.save(file);
    }

    // Later, possibly in another process: every x with x * x + 7 == 8.
    std::ifstream file("square.bri", std::ios::binary);
    const auto loaded = inv::program::load(file);
    inv::values target(8);
    for (size_t bit = 0; bit < target.size(); ++bit)
        target[bit] = (8u >> bit) & 1u;
    loaded.solve(target, [](const inv::values& input)
    {
        unsigned value = 0;
        for (size_t bit = 0; bit < input.size(); ++bit)
            value |= unsigned(input[bit]) << bit;
        std::cout << value << '\n';                  // 1, 129, 127, 255
        return true;                                 // false stops early
    }, 0);                                           // limit: 0 = all, default 1
}
```

`loaded.synthesized()` would return a program that answers the same queries
without search. See [docs/INVERSE.md](docs/INVERSE.md).

## Inverse backends

A program has exactly one backend. `compile` chooses affine or search; the other
three are built from a compiled program and return a new program.

| Backend | Created by | Build cost | Query cost | `solve` lists all matches |
|---|---|---|---|---|
| [Affine](docs/INVERSE.md#affine) | `compile`, when the relation is affine over GF(2) | Gaussian elimination | One GF(2) matrix-vector product | Yes |
| [Search](docs/INVERSE.md#search) | `compile`, otherwise | None | DPLL or CDCL search; exponential worst case | Yes |
| [BDD synthesis](docs/INVERSE.md#bdd-synthesis) | `synthesized()`, CLI `synthesize` | BDDs of the circuit and relation; can grow exponentially | One BDD path for validity, one per unknown bit | Yes, from the saved relation |
| [Compiled selector](docs/INVERSE.md#compiled-selector) | `selected()`, CLI `synthesize-selector` | Enumerates all 2^u inputs, u at most 20 | Tree walk plus one forward evaluation | Yes |
| [CEGIS-learned](docs/INVERSE.md#cegis-learned) | `learned()`, CLI `synthesize-cegis` | SAT counterexample loop and an UNSAT proof; only degree-1 or degree-2 polynomials | Polynomial plus one forward evaluation | No, one match per target |

The last three can be exported as standalone C++ (`export_cpp`, CLI `export-cpp`).

## Repository layout

| Path | Contents |
|---|---|
| `include/tracker.h` | `bit_tracker`, `int_tracker<N>` and their arithmetic |
| `include/bitreverse.h` | Umbrella header: trackers, the solver and `assert_equality` |
| `include/inverse.h`, `include/inverse/` | Compiled inverses, their backends and the saved format |
| `include/solver/` | Search engines over tracked circuits |
| `include/partial_assert.h` | Constraints applied while an algorithm is built |
| `include/crc32.h`, `md5.h`, `sha1.h`, `sha256.h`, `hash_common.h` | Hash functions over tracked bytes |
| `include/circuit_metrics.h` | Size, depth and width of a tracked DAG |
| `include/multiplication_reversal.h` | secp256k1 point arithmetic over tracked integers |
| `include/counted_ptr.h`, `buffered_object_pool.h` | Reference counting and node allocation |
| `support/` | Helpers shared by tests, examples and benchmarks; not part of the library |
| `src/` | Exploratory executables `bitreverse` (`main.cpp`) and `mrev` (`multiplication_reversal.cpp`) |
| `examples/` | The `inverse_demo` CLI and the partial-assertion examples |
| `benchmarks/` | Benchmark executables and the `run_hash_reversal.py` driver |
| `tests/` | Unit tests; `tests/cmake/` holds CLI round-trip scripts |
| `docs/` | Reference documentation |

## Documentation

- [docs/CLI.md](docs/CLI.md): `inverse_demo` commands, options, formats and exit codes.
- [docs/INVERSE.md](docs/INVERSE.md): the C++ inverse API, backends, budgets, statistics, export and persistence.
- [docs/SOLVER.md](docs/SOLVER.md): search engines, options, budgets and statistics.
- [docs/PARTIAL_ASSERT.md](docs/PARTIAL_ASSERT.md): constraints during construction.
- [docs/ARITHMETIC_CIRCUITS.md](docs/ARITHMETIC_CIRCUITS.md): how `int_tracker` builds arithmetic.
- [docs/BENCHMARKS.md](docs/BENCHMARKS.md): the benchmark executables.
