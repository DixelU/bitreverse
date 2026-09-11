# Nonlinear inverse synthesis: first experiments

The project now builds an exact inverse for bounded nonlinear circuits, saves
it, and evaluates it without SAT or input search. It can also export a standalone
C++ header containing Boolean decision graphs evaluated along the supplied
target's paths. The initial measurements below used the full schedule evaluator;
the implemented first-step results appear after the traversal experiment.

The first useful result is that **input recovery and target validity can have
very different representation sizes**. For MD5 with three bytes fixed and one
byte unknown, the input-selection functions use 786 decision nodes. The saved
validity predicate uses 28,971. These are storage counts, not lower bounds on
query work; see the follow-up traversal measurement below.

## Measured sizes

These measurements use the default bounds of 750,000 created decision nodes and
10,000,000 construction operations. The MD5 examples are fixed four-byte messages
with 4, 8, or 12 selected bits left unknown; the remaining bits are baked in.

| Specialized algorithm | Unknown bits | Forward nodes | Input selector nodes | Validity nodes | Synthesis time |
|---|---:|---:|---:|---:|---:|
| Toffoli reversible gate | 3 | 5 | 6 | 0 | 0.06 ms |
| Multiply by 3 modulo 256 | 8 | 37 | 73 | 0 | 0.62 ms |
| Square modulo 256 | 8 | 289 | 38 | 13 | 0.50 ms |
| One-byte CRC32 | 8 | 102 | 44 | 4,861 | 8.0 ms |
| Specialized MD5 | 4 | 31,598 | 41 | 1,934 | 34 ms |
| Specialized MD5 | 8 | 31,650 | 786 | 28,971 | 1.00 s |
| Specialized MD5 | 12 | 32,057 | — | — | Stopped at the node limit after 1.57 s |

Forward counts include the specialized circuit's gates, leaves, and constants.
Decision nodes represent Boolean multiplexers. These are different cost units;
the table does not establish a primitive-gate or execution-time advantage.
Selector and validity sets can overlap, so their counts need not add exactly.

The eight-unknown-bit MD5 build creates 574,537 temporary and retained decision
nodes. Its saved relation uses 31,267 nodes; the union of that relation, the
selectors, and the validity function uses 61,024. The complete `.bri` artifact is
1,252,779 bytes, including the forward circuit retained for resynthesis. The
exported C++ contains only the canonical inverse and its validity check.

Reaching the limit at 12 unknown bits demonstrates the current builder's cost;
it does **not** prove that the final inverse must be large. Variable ordering,
temporary-node collection, and stronger minimization may improve these results.

## Query behavior and validation

For the eight-bit MD5 specialization, checked `program::evaluate` took roughly
30 microseconds per query in this run. That includes validity evaluation,
allocation, and consuming every returned bit. It measures the in-process decision
schedule evaluator, not the exported executable or native forward MD5. No claim
of matching native MD5's speed follows from this measurement.

CRC32 remains better served by the existing affine backend: the one-byte affine
inverse measured about 0.52 microseconds per checked query, versus 6.2 microseconds
for the synthesized decision diagram. Exact nonlinear synthesis supplements the
affine path.

Tests exhaustively compare small nonlinear functions and all their preimages
against independent truth tables. For the baked `md5` prefix, all 256 possible
last-byte digests are checked against an independent native-integer MD5
implementation, including persistence and malformed-target rejection. The
standalone export test compiles without any project include path and checks
`md5!`, `md5?`, and an unreachable digest. Building with insufficient resources
fails explicitly and leaves existing artifacts intact.

The exhaustive synthesis test executable passed when compiled directly with
GCC 15.2 at `-O2`. The final CMake/CTest run passed seven of eight checks,
including the standalone export test. Kaspersky quarantined the CMake Release
synthesis test executable before CTest could launch it; that check remains
unrun in the final CTest build. Its detection log records the executable's path.

## Follow-up: stored size versus work per query

A read-only PowerShell traversal of the existing saved artifact followed only
the branch selected by each target bit, separately for validity and each input
selector. All 256 digests produced by .NET MD5 for `md5` plus one byte recovered
that exact byte. This did not change the C++ implementation.

| Targets | Validity path visits | Total visits for validity and eight selectors |
|---|---:|---:|
| All 256 reachable digests | 128 each | Mean 190.19; range 175–220 |
| 32 deterministic invalid digests | Mean 9.03; range 6–12 | Mean 65.41 if selectors are also evaluated |
| All-zero digest | 7 | 55 if selectors are also evaluated |

The original evaluator executes all 29,757 retained function nodes per query.
For reachable targets, independent path traversal visits an average of 186.41
distinct nodes; its 190.19 total includes repeated visits across roots. Checking
validity first permits immediate rejection of invalid targets. These are node
visit counts, **not** measured C++ speedups, and traversal does not shrink the
saved graph. The proposed construction and evaluation experiments are in
[EXPLORATION_PLAN.md](EXPLORATION_PLAN.md).

## Step 1 implemented: evaluation and construction profiling

The default API and exported C++ now evaluate validity first and follow only
target-selected branches. The full schedule remains available as a benchmark
baseline. No input search is involved in either evaluator. All eight CTest
checks passed in the new build, including exhaustive synthesis tests and
compilation/execution of the standalone exported header. The earlier quarantine
issue did not recur in this run.

Measurements from GCC 15.2, Release, in the same benchmark executable:

| Specialized inverse | Valid query, lazy | Valid query, schedule | Invalid query, lazy | Invalid query, schedule |
|---|---:|---:|---:|---:|
| MD5, 4 unknown bits | 0.522 us | 2.342 us | 0.096 us | 1.713 us |
| MD5, 8 unknown bits | 0.710 us | 30.838 us | 0.101 us | 21.772 us |
| CRC32, 8 unknown bits | 0.230 us | 5.194 us | 0.033 us | 3.629 us |

The eight-bit MD5 case improves by approximately 43 times for valid queries in
this run. Its mean lazy visit count remains 190.191. The invalid pool now mixes
zero, random digests, and near misses produced by single-bit changes; its mean
is 36.578 visits, so it is a different workload from the earlier random-only
traversal sample. Every invalid target is checked against the complete native
forward image before timing. All 256 byte-domain inputs recover correctly after
save/load, and lazy, scheduled, and profiled results agree.

Native scalar four-byte MD5 measured 0.090 us per forward pass. That baseline
uses packed integers and excludes bitvector conversions, while the inverse
timings include its allocating public API and consuming all returned bits.
It provides practical context, not an equal-representation circuit comparison.
The existing affine CRC32 backend measured 0.218 us for valid queries.

For the eight-bit MD5 build:

| Construction phase | Phase time | Nodes created in phase | Nodes reachable from phase outputs | Peak tracked allocation |
|---|---:|---:|---:|---:|
| Forward conversion | 790.9 ms | 511,116 | 4,186 | 88.37 MB |
| Relation construction | 90.9 ms | 33,664 | 31,267 | 101.95 MB |
| Witness extraction | 19.7 ms | 29,757 | 61,024 including saved relation | 106.33 MB |
| Compaction | 2.8 ms | 0 | 61,024 | 110.93 MB |

End-to-end synthesis measured 1.049 seconds, including cleanup and construction
of the returned program. Phase times exclude boundary reachability scans;
the overall statistics include those scans. MB denotes decimal megabytes.
Tracked allocation counts bytes requested by the instrumented builder
containers; it excludes process/allocator overhead, the input and returned
circuits, control objects, and reachability/indexing scratch. The recorded peaks
are not process RSS. Reachable counts describe phase outputs, even while caches
and intermediate graphs remain allocated.

**Both the 10-bit and 12-bit MD5 cases fail during forward conversion at 750,000
created nodes. Neither reaches relation construction.** Their end-to-end times
were 1.320 and 1.257 seconds. This localizes the immediate construction limit to
the representation of intermediate forward computations. It does not establish
a lower bound on the final inverse.

The eight-bit saved artifact remains byte-for-byte identical to the earlier
artifact (SHA-256 checked), so the new evaluator also benefits existing saved
programs. The next experiment is to construct selectors without materializing
every forward-gate decision diagram, then validate candidates with the forward
algorithm. [Raw benchmark output](experiments/synthesis-step1.csv).

## Reproduce

After building the project, run `synthesis_benchmark` for CSV measurements. It
reports construction work, retained relation/selector/validity sizes, artifact
bytes, query timing, and explicit limit failures. Timings vary by machine and
load. Original expression construction and concrete target generation are
excluded from synthesis/query timing.

```sh
inverse_demo build md5 "6d6435??" md5-source.bri
inverse_demo synthesize md5-source.bri md5-synth.bri
inverse_demo export-cpp md5-synth.bri md5_inverse.h
inverse_demo solve md5-synth.bri b682c4079aa8cae8524986bf8de47f5c
```

The exported `bitreverse_inverse` selects one canonical input and returns
`std::nullopt` for unreachable targets. Use `solve --all` on the saved relation
to enumerate distinct matching inputs. The next research question is how well
the selector functions compress as the unknown domain grows, independently of
the cost of recognizing valid targets.
