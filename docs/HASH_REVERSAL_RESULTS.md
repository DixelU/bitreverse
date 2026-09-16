# SHA-1 and SHA-256: forward shape and bounded preimage search

Full-round SHA-1 and SHA-256 now run over tracked integers, alongside MD5 and
CRC32. SHA-256 is a member of SHA-2; there is no separate algorithm named
"SHA-2" to add as a third row. The SHA implementations follow
[FIPS 180-4](https://csrc.nist.gov/pubs/fips/180-4/upd1/final).

The first comparison finds that **SHA-1 is deeper, but SHA-256 is larger and
costlier for this solver**. With eight unknown input bits, MD5 and SHA-1 recover
the chosen preimage within 20 million search steps. SHA-256 reaches that limit.
All three hashes recover the four-bit case. These are small, specialized
preimage problems, not general hash inversion or collision attacks.

## Experiment definition

Measurements were collected on 2026-09-16, Windows x64, GCC 15.2, C++23, `-O3`,
with the benchmark linked statically. Each measurement is one run, not a median.
Antivirus activity occurred during this work, so timings are exploratory and
should not be treated as stable performance estimates. Graph counts and search
work counts describe the recorded runs independently of elapsed wall time.

Every algorithm receives the same four-byte template based on `abcd`
(`61626364`). Unknown bits replace the low bits of the first byte, followed by
the low bits of the second byte, and so on. Thus eight unknown bits means
`??626364`; sixteen means `????6364`. The unknown portion of the witness is
discarded when constructing the symbolic inputs. Its only role outside the
solver is generating a reachable target with Python `hashlib` or `zlib`.

There are no supplied intermediate states, printable-character requirements,
reduced rounds, truncated digests, precomputed selectors, or BDD synthesis in
these runs. Search asks for the first matching input. Each recovered input is
checked independently against the digest and the fixed input bits. A separate
native-library enumeration checks the entire domain through sixteen unknown
bits and finds exactly one preimage for each tested target. That enumeration
does not supply assignments to the solver.

[The main CSV](../experiments/hash-reversal.csv) contains 28 shape-only runs and
48 search runs: 25 verified successes and 23 search-limit results, with no
UNSAT, process timeout, or error rows. The
[initial two-million-step pilot](../experiments/hash-reversal-quick.csv) is
retained separately. The main run uses 20,000,000 search steps per query and
a 30-second external timeout per process. A limit is **unknown**, not UNSAT.
Compilation of solver adjacency and engine initialization are outside the
search-step budget; the process timeout also covers those phases.

## What depth and width mean

`measure_circuit` visits only nodes reachable from the supplied outputs. The
benchmark measures and flushes the forward record **before constructing any
digest-equality expression**. Inputs fixed by the template are already constants,
so normal construction-time simplification has happened.

- Nodes are distinct stored DAG nodes: gates, unknown leaves, and constants.
  A shared subexpression is counted once, even if several outputs use it.
- Depth is the longest dependency path in edges, with inputs/constants at zero.
- Width is the largest number of distinct nodes at the same leaf-based depth.
  It is not a tree breadth, search frontier size, antichain maximum, or memory
  bound. Gate-only widths and all per-depth widths are also available in the API.
- Fanout and edge counts count argument slots. Repeated output roots affect
  output-depth statistics, but do not duplicate stored nodes.

These are forward Boolean expression DAGs, not solver search trees or BDDs.
Addition decomposition, operator ordering, constant values, and sharing affect
their shape. The measurements are properties of this implementation, not
minimum circuits or cryptographic security estimates.

For eight unknown bits, before equality:

| Algorithm | Output bits | DAG nodes | Gates | Depth | Maximum width | AND + OR gates |
|---|---:|---:|---:|---:|---:|---:|
| CRC32 | 32 | 593 | 585 | 17 | 62 | 0 |
| MD5 | 128 | 31,751 | 31,743 | 2,880 | 25 | 17,605 |
| SHA-1 | 160 | 47,357 | 47,349 | 4,737 | 88 | 25,955 |
| SHA-256 | 256 | 105,231 | 105,223 | 3,795 | 91 | 53,687 |

SHA-1 has 1.49 times MD5's nodes and 1.64 times its depth. SHA-256 has 3.31 times
MD5's nodes, but only 1.32 times its depth. CRC32's greater width than MD5 does
not make it harder here: its forward graph contains only XOR/NOT operations
and input leaves. Gate type and the constraints matter alongside depth/width.

Increasing the unknown domain changes graph shape much less than domain size:

| Algorithm | 4 unknown bits: nodes / depth / width | 16 bits | 32 bits |
|---|---:|---:|---:|
| CRC32 | 573 / 16 / 62 | 582 / 17 / 63 | 542 / 18 / 63 |
| MD5 | 31,615 / 2,880 / 25 | 32,292 / 2,924 / 25 | 32,691 / 2,977 / 32 |
| SHA-1 | 45,837 / 4,681 / 61 | 49,596 / 4,768 / 96 | 52,846 / 4,901 / 100 |
| SHA-256 | 105,121 / 3,795 / 85 | 106,245 / 3,828 / 81 | 107,435 / 3,861 / 84 |

The domain grows from 16 to 4,294,967,296 assignments in that table; the 24/32-bit
cases were shape-only. CRC32's node count is not monotone because changing
constants to variables changes local simplification and retained NOT nodes.
All four fully fixed inputs produce only the two shared Boolean constants.

For context, building the usual left-fold OR of mismatching digest bits adds
the following shape. This is still a structural measurement before solving:

| Algorithm, 8 unknown bits | Forward nodes / depth | Equality nodes / depth |
|---|---:|---:|
| CRC32 | 593 / 17 | 609 / 45 |
| MD5 | 31,751 / 2,880 | 31,946 / 2,953 |
| SHA-1 | 47,357 / 4,737 | 47,597 / 4,897 |
| SHA-256 | 105,231 / 3,795 | 105,599 / 4,051 |

Equality adds a modest number of nodes here; that small increase does not
predict the difficulty of finding a satisfying assignment.

## Recovery results

Plain DPLL, first preimage, 20-million-step budget. Times include equality
construction inside `assert_equality`, solver compilation, initialization, and
search; forward construction and separate graph profiling are excluded.

| Algorithm | 4 unknown bits | 8 unknown bits | 12 unknown bits | 16 unknown bits |
|---|---:|---:|---:|---:|
| CRC32 | SAT, 0.107 ms | SAT, 0.112 ms | SAT, 0.111 ms | SAT, 0.119 ms |
| MD5 | SAT, 15.091 ms | SAT, 142.848 ms | Limit, 364.608 ms | Limit, 349.018 ms |
| SHA-1 | SAT, 15.999 ms | SAT, 186.840 ms | Limit, 400.523 ms | Limit, 385.698 ms |
| SHA-256 | SAT, 133.552 ms | Limit, 834.057 ms | Limit, 782.805 ms | Limit, 541.064 ms |

CRC32 takes zero search decisions throughout these four widths. The eight-bit
MD5 solve takes 273 decisions and 8,030,785 steps; SHA-1 takes 157 decisions and
9,555,305 steps. Decisions across explored branches can exceed the number of
unknown input bits; they are not distinct input variables. SHA-256 takes 153
decisions before reaching 20,000,000 steps in the eight-bit run.

Requesting affine propagation activates it for CRC32. It falls back to ordinary
DPLL for every MD5/SHA row: the number of input and nonlinear gate atoms exceeds
the existing 4,096-atom default. Those rows do not test a stronger XOR-aware
hash solver. The direct affine inverse from `program::compile` is a separate
backend and is not benchmarked here.

CDCL recovers all CRC32 cases and each hash's four-bit case, but reaches the
budget at 8/12/16 bits for all three hashes. This does not establish that CDCL
is generally worse: the counters account for different engine work, the cap
is a work limit rather than equal wall time, and only one target/template is
tested at each width.

The small-domain enumeration baseline is much faster for these hashes. At
eight unknown bits, it reaches `abcd` on trial 98 in 0.129 ms for MD5, 0.128 ms
for SHA-1, and 0.228 ms for SHA-256. At sixteen bits it reaches the target on
trial 25,186 in about 34–35 ms for each hash. These timings include Python loop,
candidate allocation, and native-library API overhead; they are not pure native
hash instruction timings or an average over targets. Exhaustive enumeration
continues after the first match to check uniqueness, recorded separately as
`native_total_ms`.

## Comparison with earlier experiments

[The partial-assert experiments](PARTIAL_ASSERT.md) supplied eight observed
MD5 round-state bits or sixteen observed CRC32 state bits in addition to the
final digest. Those observations completely determined their input domains,
allowing eager simplification to reduce each digest to two constant nodes and
final solving to take zero decisions. They demonstrate the value of early
information; they are not equivalent to digest-only preimage recovery.

The deferred MD5 example uses the same `?bcd` byte template as the new eight-bit
case and retains exactly the same 31,751-node digest DAG. Its cheap final solve
comes from the extra checkpoint constraint. The old CRC32 example uses the
longer `AB/checkpoints` message, so its 2,135-node deferred digest is not a
same-template comparison with the new four-byte CRC32 graph.

[Earlier BDD synthesis](SYNTHESIS_RESULTS.md) built a reusable inverse for an
eight-bit MD5 domain in about 1.049 seconds; its 750,000-created-node budget
blocked larger examples. [The selector backend](SELECTOR_RESULTS.md) reached
twenty unknown MD5 bits by exhaustively enumerating 1,048,576 inputs and storing
a selector plus forward verifier. Those experiments pay construction costs to
answer future queries. The new SAT benchmark does fresh search for one target.
Their BDD/selector node counts and query timings are not the same quantities as
forward DAG nodes or this benchmark's solve time.

The useful next experiment is to test digest outputs as separate bindings,
then compare graph locality, propagation, and input ordering across several
targets at identical unknown widths. Global depth and maximum width alone do
not explain the observed search cost. Local XOR reasoning and modular-addition
structure are plausible directions; these results do not yet measure them.

## Reproduction and validation

Sources: [benchmark](../benchmarks/hash_reversal_benchmark.cpp),
[independent driver](../benchmarks/run_hash_reversal.py),
[metrics API](../include/circuit_metrics.h),
[SHA-1](../include/sha1.h), and [SHA-256](../include/sha256.h).

The CMake targets are `hash_reversal_benchmark`, `sha_tests`, and
`circuit_metrics_tests`. The main recorded matrix was invoked with:

```sh
python benchmarks/run_hash_reversal.py \
  --exe build-hash-experiments/hash_reversal_benchmark.exe \
  --output experiments/hash-reversal.csv \
  --max-steps 20000000 --timeout 30 --engines dpll,affine,cdcl
```

The driver accepts `--quick`, `--algorithms`, `--bits`, `--engines`, and
`--input-hex`. Every chosen width has a shape-only run; `--engines=` requests
only those runs. Explicit `--bits` also selects those widths for solving.
The benchmark accepts messages through 128 bytes, but this measured matrix
contains only four-byte messages. Native enumeration is capped at sixteen
unknown bits regardless of selected solver widths.

The hash functions can be used with the existing inverse API, although this
change does not add SHA names to the `inverse_demo` CLI. To inspect before
assertion directly:

```cpp
#include "circuit_metrics.h"
#include "sha256.h"
namespace br = dixelu::bitreverse;
std::vector<br::itu8> input{br::unknown, 'b', 'c', 'd'};
auto digest = br::hash::sha256(input);
auto shape = br::measure_circuit(digest); // No target or equality imposed.
// shape.widths holds the complete count at every dependency depth.
```

Before execution was paused, standalone SHA tests passed known-answer vectors
for empty/abc, padding boundaries 55/56/63/64/65 bytes, multi-block and binary
messages, and symbolic assignments. Metrics tests passed DAG-sharing,
non-topological discovery, duplicate operands/roots, depth/width, operation
counts, and input-exclusion checks. All 25 successful main-matrix preimages
passed independent `hashlib`/`zlib` verification. This is not an exhaustive
correctness test of the hashes or the solver for all inputs.

Full CMake/CTest validation did not complete: configuration stalled, and
Kaspersky subsequently flagged the statically linked standalone SHA test
`cmake-build-release/sha_tests_standalone.exe` as
`VHO:Trojan-Ransom.Win32.Crypmod.gen`. The file disappeared; its classification
has not been resolved. No flagged executable was recreated after that report,
and no additional executable runs were used to finish this analysis. The
successful SHA test results above came from the earlier dynamically linked
test build. No antivirus exclusion is required or recommended by this report.
