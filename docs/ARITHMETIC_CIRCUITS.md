# Shallower arithmetic circuits

The arithmetic changes implement the main circuit-topology proposals from the
[shared discussion](https://chatgpt.com/share/6aab92c7-5d10-83eb-8851-1393186adeaa).
They change how `int_tracker<N>` constructs Boolean expressions, preserving its
unsigned, modulo-`2^N` arithmetic and the existing inverse/solver interfaces.

## Implementation

- Addition uses a Brent-Kung parallel-prefix carry scan. It supports arbitrary
  widths and a symbolic input carry. Fully concrete inputs use a direct Boolean
  loop; no prefix graph is needed for them.
- Subtraction uses the same scan for `lhs + ~rhs + 1`.
  `self_sub_ret_carry` still ignores its incoming carry and returns **true for
  no unsigned underflow**. `self_add_ret_carry` still consumes and returns carry.
- Multiplication builds AND partial-product rows, compresses each group of three
  rows into two carry-save rows, and performs one final prefix addition. Carries
  above the result width are discarded. The operand with fewer potentially
  nonzero bits supplies the rows, making a sparse constant equally useful on
  either side. This is a row-based carry-save tree, without a Dadda column scheduler.
- Division retains non-restoring division and uses the prefix adder at each
  iteration. Its outer remainder dependency is still sequential.
- A known divisor of one or a power of two becomes bit wiring and zero constants.
  Other constant divisors use `bit_width(divisor) + 1` remainder bits, including
  the sign bit. Constants wider than a native integer are inspected bit by bit.
- Known leading zero dividend bits are skipped. A small constant numerator thus
  needs fewer iterations, while the full variable divisor remains available.
  No high divisor bits are truncated.

Division remains a total bit-vector operation: dividing by zero returns an
all-ones quotient and the original dividend as remainder. Skipped quotient bits
retain a divisor-zero condition when the divisor is symbolic. In particular,
`0 / x` cannot become an unconditional zero, nor can `x / x` become an
unconditional one. These cases are included in the exhaustive tests.

Inputs keep their original symbolic identities. The suggestion to replace
`!unknown` with `unknown` is unsound when other expressions refer to the same
input, and is not applied. In-place operations and a remainder output that
aliases either division operand are supported. Compile-time evaluation remains
available, including symbolic construction using the new scratch containers.

## Measured circuit shape

The benchmark constructs forward expressions without asserting a target. Nodes
are counted once in the shared DAG; depth is the longest leaf-to-output path,
with leaves at depth zero. This differs by one from `__max_depth()` for graphs
rooted in unknown inputs, because the existing tracker gives those inputs depth
one. The older `int_tracker_benchmark` retains that convention.

64-bit results on 2026-09-17, before (`0c9bd86`) and after this change:

| Expression | Gates before | Gates after | Depth before | Depth after |
|---|---:|---:|---:|---:|
| `x + y` | 314 | 470 | 126 | 22 |
| `x - y` | 439 | 536 | 189 | 24 |
| `x + 1` | 126 | 178 | 63 | 11 |
| `x * y` | 25,896 | 12,312 | 374 | 46 |
| `x * 13` | 603 | 756 | 124 | 24 |
| `13 * x` | 9,757 | 756 | 367 | 24 |
| `x / y` | 24,573 | 35,289 | 8,382 | 1,143 |
| `x % y` | 25,080 | 36,066 | 8,384 | 1,167 |
| `x / 8` | 10,047 | 0 | 4,080 | 0 |
| `x % 8` | 10,412 | 0 | 4,085 | 0 |
| `x / 10` | 11,038 | 1,629 | 4,456 | 548 |
| `x % 10` | 11,412 | 1,585 | 4,457 | 552 |
| `37 / x` | 24,386 | 2,914 | 8,260 | 95 |
| `37 % x` | 24,893 | 3,685 | 8,262 | 119 |

At 256 bits, multiplication drops from 423,048 gates / depth 1,526 to 197,367
gates / depth 64. Division drops from depth 131,838 to 6,130, with gates
increasing from 393,213 to 581,921. Remainder by ten drops from 168,756 gates
to 6,577.

Prefix addition deliberately trades more gates for shorter dependencies.
Construction of generic addition and division can also take longer. Shallower
circuits do not by themselves establish faster SAT search, smaller synthesized
inverses, or improved hash reversal. The existing integration tests pass, but
these measurements are circuit-shape measurements, not new reversal benchmarks.

The complete 8-, 32-, 64-, and 256-bit measurements are in
[`arithmetic-before.csv`](../experiments/arithmetic-before.csv) and
[`arithmetic-after.csv`](../experiments/arithmetic-after.csv). Both were built
with MinGW GCC 16.1.0, `-std=c++23 -O3`, on Windows. `build_ms` records one
construction per expression and excludes graph measurement and final result
destruction; it is informational, not a statistical timing comparison. Gate
counts and depths are deterministic.

Reproduce the current measurements with:

```sh
cmake -S . -B build -G Ninja -DCMAKE_BUILD_TYPE=Release
cmake --build build --target arithmetic_circuit_benchmark
build/arithmetic_circuit_benchmark > arithmetic.csv
```

The same benchmark source compiled against `include/` from `0c9bd86` reproduces
the baseline shape. Existing experiment reports describe their historical
circuits and have not been rewritten with these new counts.

## Validation and follow-up work

`arithmetic_tests` checks the generated Boolean DAG independently of the solver:

- All pairs of 1-, 3-, 5-, and 8-bit symbolic inputs, with both input carry
  values, against native arithmetic. This covers overflow, subtraction carry,
  multiplication, division, zero divisors, and aliased/repeated operands.
- Every 8-bit constant on either side of division, for all 256 symbolic input
  assignments.
- 64-, 65-, and 129-bit sampled values and boundary cases against separate
  Boolean schoolbook multiplication and restoring-division references. Cases
  include divisors exceeding 64 bits and high-bit powers of two.
- Compile-time concrete and symbolic construction, aliased remainder outputs,
  and structural budgets for the intended depth and size improvements.

The full CTest suite also covers CRC32, MD5, SHA-1, SHA-256, elliptic-curve
arithmetic, reverse solving, saved inverses, and standalone C++ exports.

The broader proposals remain separate experiments: Boolean hash-consing and
associative canonicalization, reciprocal/magic-number division with a widened
high-half product, and redundant-remainder/SRT division. None is required by
the arithmetic paths introduced here.
