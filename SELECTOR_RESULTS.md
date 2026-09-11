# Step 2: bounded selectors with forward validation

The new backend avoids converting intermediate forward gates into BDDs. It
evaluates 64 concrete assignments per machine-word pass, groups equal outputs,
and builds a decision tree that chooses a canonical input. Each query traverses
that tree and runs the saved forward circuit once to verify the answer.

The 20-bit MD5 case now builds in **3.86 seconds**. All **1,048,576 inputs** were
independently verified after serialization and reload. The earlier BDD builder
stopped during forward conversion at 10 and 12 unknown bits.

This is an exhaustive, table-derived baseline. During construction it stores a
temporary table of reachable outputs. It avoids the intermediate forward BDDs,
the relation BDD, and the separate validity BDD; it does not avoid enumeration
or establish compact structural inversion of MD5.

## Measurements

GCC 15.2, Release, same machine as step 1. These are fixed four-byte MD5 messages
with selected low bits of the trailing bytes unknown and the remaining bits
baked. At 16 bits, the prefix is `md`; at 20 bits, the domain is the hex pattern
`6d6?????`.

| Unknown bits | Inputs | Build | Saved artifact | Peak tracked allocation | Checked query |
|---|---:|---:|---:|---:|---:|
| 8 | 256 | 1.04 ms | 0.524 MB | 0.879 MB | 29.31 us |
| 10 | 1,024 | 2.32 ms | 0.551 MB | 0.882 MB | 30.01 us |
| 12 | 4,096 | 6.29 ms | 0.644 MB | 0.904 MB | 27.88 us |
| 16 | 65,536 | 118.97 ms | 2.668 MB | 4.714 MB | 30.74 us |
| 20 | 1,048,576 | 3.863 s | 38.265 MB | 65.664 MB | 28.32 us |

MB denotes decimal megabytes. Saved artifacts include the original specialized
forward circuit. Tracked allocation includes selector storage, its derived
forward schedule, and temporary tables/work vectors. It excludes the original
program, fixed/control objects, recursion frames, allocator overhead, benchmark
reference data, serialized text buffers, and query workspace. It is not process
RSS.

The default limits are 65,536 assignments, 131,071 total tree nodes, 250 million
counted operations, and 256 MiB of tracked allocation. The 20-bit experiment
explicitly raises the first three to 1,048,576 assignments, 2,097,151 nodes, and
2 billion operations. It consumes 1,870,142,268 operations. Memory retains the
default limit. The implementation hard-caps unknown width at 20 bits and total
unknown-plus-output width at 512 bits.

At 20 bits, enumeration takes 1.042 seconds, tree construction 2.607 seconds,
and checking the tree against every distinct enumerated target 0.212 seconds.
The independent native-MD5 verification after reload takes another 32.309
seconds and is excluded from build timing. Its result is exact agreement for
every domain input, not sampled success. The small query timing sample is
separate from that exhaustive validation.

## The tradeoff

Construction improves substantially, but the generic forward verifier evaluates
roughly 32,000 individual Boolean gates on each query. This dominates query
time. The 20-bit selector itself averages 20 target-bit decisions in the timed
sample. Random invalid digests and one-bit near misses also take roughly
28.6 microseconds because they still require the forward check.

For the eight-bit domain, step 1's lazy BDD backend remains faster to query:
0.71 microseconds in its recorded run versus approximately 29 here. It required
roughly a second to build, versus 1 millisecond for this selector. The backends
serve different construction/query tradeoffs.

A native scalar forward MD5 pass measures about 0.087 microseconds. A sorted
table using packed digests and inputs measures approximately 0.021 microseconds
per valid query at 20 bits, with 20.972 MB of table-entry payload. Those
interfaces omit the inverse API's bitvector conversion/allocation costs. Saved
text size and native table payload are different storage measures. Nevertheless,
the table baseline gives no evidence that this selector discovered a more
compact MD5 inverse: it retains one leaf per reachable digest, all allowed input
ranks, and a full binary tree with `2 * leaves - 1` nodes. Storage still grows
with the enumerated domain.

## Correctness and persistence

Ascending packed input ranks encode false-first order in the declared input-bit
layout, including aliases and known bits. Equal outputs share one leaf with a
sorted group of all matching input ranks. The default evaluator returns the
first member. `solve --all` enumerates the group without SAT, after checking its
representative with the forward circuit. The standalone export returns only
the canonical member.

Construction enumerates the entire permitted domain and verifies every distinct
reachable output against the finished tree before returning an artifact.
Unreachable targets can select an arbitrary candidate; the forward check rejects
it. Permanently baked requirements are also verified. This is exhaustive finite
certification, not a SAT proof or a learned generalization beyond the domain.

The versioned artifact stores tree nodes and input groups alongside the existing
forward circuit. Loading reconstructs the forward evaluator and checks bounds,
tree structure, group ranges/order, and the outer checksum. It trusts the
builder's semantic certification; a checksum is not an independently verified
proof against deliberate metadata forgery. The format is compatible with older
backends through the existing outer version.

All ten CTest checks passed, including exhaustive small nonlinear functions,
multiple preimages, aliases, unused inputs, empty/impossible domains, budget
failures, corrupt artifacts, native-reference MD5, and standalone export.
The 16-bit exported C++ header also compiled and ran independently, checking
`md5!`, `md5?`, and an unreachable digest. The exporter uses heap workspace for
forward evaluation so large circuits do not require a circuit-sized stack.

## Reproduce

```sh
inverse_demo build md5 "6d64????" md5-source.bri
inverse_demo synthesize-selector md5-source.bri md5-selector.bri
inverse_demo export-cpp md5-selector.bri md5_inverse.h
inverse_demo solve md5-selector.bri b682c4079aa8cae8524986bf8de47f5c
```

Each `?` is an unknown hex nibble; concrete nibbles are baked. The 20-bit case:

```sh
inverse_demo build md5 "6d6?????" md5-20-source.bri
inverse_demo synthesize-selector md5-20-source.bri md5-20.bri --max-assignments=1048576 --max-nodes=2097151 --max-operations=2000000000
```

`selector_benchmark` runs the 8/10/12/16-bit cases. `selector_benchmark 20` runs
only the ceiling experiment with its documented raised budgets. Raw results:
[8–16 bits](experiments/synthesis-step2.csv),
[20 bits](experiments/synthesis-step2-20bit.csv).

The next experiment is counterexample-guided synthesis: search for small
candidate functions and prove their correctness without first enumerating every
input. The table and selector results now provide concrete baselines for both
construction cost and saved size.
