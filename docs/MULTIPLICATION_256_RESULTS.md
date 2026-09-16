# Shape and solving cost of 256-bit constant multiplication

Measured on 2026-09-16 against repository checkpoint `163fdc4`, using GCC 15.2,
C++23 and `-O3` on Windows. No multiplication or solver implementation changed.
The experiment adds only a benchmark target and this report.

`int_tracker<256>` multiplication retains the low 256 product bits: the equation
is `k*x == target (mod 2^256)`. For an odd `k`, it has exactly one solution for
every target. The measured `x * k` expressions recover that solution through
gate propagation, with zero Boolean branch attempts, including a dense 256-bit
constant. This toy is substantially easier than general Boolean inversion.

## The stored graph

The implementation stores a shared directed acyclic graph, not an expanded
expression tree. An occurrence of an unknown or a shared carry reuses the same
node. Depth below means the longest path in edges from an input/constant to an
output, with leaves at level zero. Width is the largest number of distinct nodes
assigned to one such gate-depth level, including leaves. It is not maximum
fan-in, number of candidates, a minimum-width circuit layout, or search-tree width.

| Expression | Retained product nodes | Product depth | Product width | Equality nodes | Equality depth | Equality width |
|---|---:|---:|---:|---:|---:|---:|
| `x * 3` | 1,525 | 508 | 509 | 1,896 | 763 | 510 |
| `x * 13` | 2,779 | 508 | 507 | 3,162 | 763 | 509 |
| `x * dense_odd` | 90,249 | 789 | 505 | 90,634 | 1,044 | 508 |
| `x * (2^256-1)` | 161,928 | 1,015 | 509 | 162,312 | 1,271 | 510 |
| `x * 12` | 1,514 | 504 | 505 | 1,882 | 757 | 506 |
| `x * 256` | 249 | 0 | 249 | 622 | 247 | 248 |
| `13 * x` | 137,885 | 1,519 | 33,658 | 138,268 | 1,774 | 33,658 |

The comparison computes an OR of bit mismatches, left-associated in MSB-first
order. For `x * 13` it adds 255 levels above the deepest product output.
For the shift case, eight output bits are constant zero and only 248 unknown
leaves remain. Its product contains no Boolean gates.

For a known right-hand operand with `p` set bits, the retained shift/add network
has `O(N*p)` gates, at most quadratic in the bit width. Its critical path is
`O(N+p)`: a path advances across bit columns and addition rows; a high carry
does not feed back into the next row's low bit. Eager evaluation still constructs
temporary additions for zero multiplier bits, then discards those branches.
Retained node counts therefore do not count all temporary allocations.

If shared expressions were instead copied at every use, the `x * 13` equality
would contain approximately `10^7.639155`, or 43.6 million, node occurrences.
The dense-constant equality would contain approximately `10^139.035247`
occurrences. These are theoretical expanded expression sizes computed over the
DAG in logarithmic form. Neither expansion is built, and neither is a count of
integer candidates.

## Assertion timing and search

The benchmark invokes the actual `assert_equality<256>` overload with
`first_only=false`, asking it to finish enumeration of all retained-variable
models. Each assertion has a 20-million-step guard; successful rows finish
before it. Times are medians of five calls and include comparison construction,
solver graph/engine initialization, propagation, search and collecting models.
They exclude the previously built multiplication expression and independent
native verification. Short timings vary with host load.

| Expression | Default assertion | Affine reasoning disabled | Default branch attempts | Default conflicts | Generated models | Status |
|---|---:|---:|---:|---:|---:|---|
| `x * 3` | 6.55 ms | 0.64 ms | 0 | 0 | 1 | complete |
| `x * 13` | 15.14 ms | 0.66 ms | 0 | 0 | 1 | complete |
| `x * dense_odd` | 59.81 ms | 68.28 ms | 0 | 0 | 1 | complete |
| `x * (2^256-1)` | 144.94 ms | 147.17 ms | 0 | 0 | 1 | complete |
| `x * 12` | 5.45 ms | 0.31 ms | 0 | 0 | 1 partial | complete |
| `x * 256` | 0.88 ms | 0.14 ms | 0 | 0 | 1 partial | complete |
| `13 * x` | 473.24 ms | 486.52 ms | 4,030 | 2,015 | 1 | complete |
| `dense_odd * x` | 721.59 ms | 770.40 ms | 3,704 | 1,728 | 0 | step limit |

For `x * 13`, product construction itself takes 5.42 ms in the default row,
separate from the 15.14 ms assertion. Inside that assertion, solver `run()`
takes 13.90 ms. Its gate propagation already fixes every retained node; one
subsequent affine pass adds Gaussian elimination work. With affine reasoning
disabled, the complete assertion takes 0.66 ms and `run()` takes 0.115 ms,
again with no branching. This is unnecessary solver work, not candidate search.
Dense circuits exceed the default affine-atom cap, so affine propagation is
automatically inactive; their on/off timing differences are measurement noise.

For each successful odd `x * k` case, the search tree is only its root:
decision depth zero, width one, one final complete assignment. No full guessed
256-bit candidate is tested and rejected. The solver instead propagates known
bit constraints through the circuit.

For `13 * x`, 4,030 Boolean branch attempts in a completed exhaustive binary
search imply 2,015 internal search states and 2,016 terminal leaves: 2,015
contradictions and one valid model. Most contradictions reject partial
assignments. They are not 2,015 rejected complete integers. Those counts do not
determine the maximum search depth or width; the decision depth is at most 256.
The solver traverses depth-first using a mutable assignment state and undo
trail, rather than storing the complete search tree.

## Operand order exposes a compiler problem

Multiplication shifts its left operand and selects each addition using a bit
from its right operand. In `x * k`, every selector is constant and folds away.
In `k * x`, those selectors are unknown.

The current ternary construction always emits:

```text
select(s, a, b) = (!s AND b) OR (s AND a)
```

It lacks the identity `select(s,a,a) = a`. Consequently, even low product bits
that an upper multiplier bit cannot affect retain redundant conditional logic.
This obscures the low-to-high dependencies that made `x * k` easy to propagate.
The raw multiplication graph and raw assertion path do not call the separate
`program::compile` specialization pass.

The result is about 50 times as many stored product nodes and roughly 31 times
the default assertion time for `13 * x` versus `x * 13`, despite mathematical
commutativity. A dense reversed-order run reaches its work limit without a
model; that is an incomplete measurement, not an unsatisfiability result.

## What a returned model means

For nonzero `k`, write `k = 2^t*u` with `u` odd. A target is reachable exactly
when its low `t` bits are zero; then there are exactly `2^t` complete inputs:

```text
x0 = inverse_mod(u, 2^(256-t)) * (target / 2^t) mod 2^(256-t)
x  = x0 + j * 2^(256-t),  0 <= j < 2^t
```

Thus `k=13` has one complete solution, `k=12` has four when reachable, and
`k=256` has 256. `k=0,target=0` admits all `2^256` inputs; other targets admit none.

Raw `assert_equality` includes only unknowns reachable from its comparison.
`x * 12` drops the top two unknown bits and returns one 254-bit assignment
representing all four complete inputs. `x * 256` similarly returns one 248-bit
assignment. A zero product returns one empty assignment. These are intentional
don't-care omissions, not a count of one full integer.

The redundant swapped-order graph can retain even those irrelevant unknowns:
`12 * x` retains 256 variables and actually emits four full assignments.
`256 * x` generates 58 models before hitting the measured step budget, so that
run is not exhaustive. On interrupted collecting calls, the benchmark reports
the generated-model count from solver statistics; no solution container is
returned by the exception, and `assigned_bits=0` is not a claim that its generated
models were empty. Explicit input ports through `program::compile` are the
existing interface for preserving all free bits regardless of graph reachability.

If both operands are widened before multiplying and all 512 result bits are
compared, nonzero constant multiplication has at most one input. That exact
integer-product question differs from the 256-bit modular operation measured here.

## Reproduction

All reachable targets are produced from this fixed 256-bit input using an
independent Boolean schoolbook multiplier:

```text
x = 0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef
k_dense = d6e8feb86659fd93a5a3564e27f8862b9e3779b97f4a7c15bf58476d1ce4e5b9
```

Every normally returned model is checked with that independent implementation,
filling omitted don't-care bits with zero. Additional impossible-target cases
flip a low output bit for `k=12` and `k=0`; both reject without branching.

```sh
cmake --build BUILD_DIR --target multiplication_shape_benchmark
multiplication_shape_benchmark > multiplication-256.csv
# Optional single case: three, thirteen, dense_odd, all_ones, twelve, shift8, zero
multiplication_shape_benchmark thirteen
```

Equivalent direct compilation from the repository root, using the current source
layout:

```sh
g++ -std=c++23 -O3 -Wall -Wextra -pedantic -Iinclude benchmarks/multiplication_shape_benchmark.cpp -o benchmark.exe
```

Source: [multiplication_shape_benchmark.cpp](../benchmarks/multiplication_shape_benchmark.cpp).
Raw measurements: [multiplication-256.csv](../experiments/multiplication-256.csv).
The stored data includes constants, target values, timing ranges, operation
counts, active solver modes, graph sizes, and incomplete-run statuses.
