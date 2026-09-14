# Step 3: learning and proving compact inverse functions

The new counterexample-guided backend constructs an inverse without enumerating
the input domain. On a 48-bit structured nonlinear circuit, it learns from **65
counterexamples**, proves completeness, and saves a **1,894-byte artifact**.
Construction and proof take **25.35 ms** with an explicitly raised solver budget.
This circuit has 2^48 possible inputs. The saved candidate has 64 coefficient
references, representing 16 ANDs and 16 XORs, matching the forward operation count.

This establishes structural inversion for a limited function family. It is not
an improvement to the bounded MD5 result, nor a general inverse-circuit compiler.

## What the learner does

For each unknown input bit, the candidate is an XOR of selected basis features:
the constant 1, individual target bits, and (at degree two) pairwise ANDs of
distinct target bits. The learner begins with all-zero candidates. For each
candidate `G`, the existing DPLL solver with affine propagation checks:

```text
D(x) and (not D(G(F(x))) or F(G(F(x))) != F(x))
```

A SAT model supplies an allowed input that the candidate fails to recover from
its output. The builder independently replays the model, adds its output/input
pair as a linear equation on the unknown polynomial coefficients, and solves
the accumulated equations over GF(2). Unconstrained coefficients stay zero.
Only a normally completed UNSAT result permits returning a learned plan.

The proof checks the relational property: an alternative valid preimage is
acceptable. It does not require `G(F(x)) == x`. Learning nevertheless fixes the
particular witnesses supplied by SAT, which can exclude a simpler polynomial
selector for a many-to-one relation. Failure to fit these witnesses does not
prove that no selector exists in the polynomial family.

Repeated operands retain their identities. Both forward copies and candidate
terms share an interning table. The proof builder normalizes associative XOR,
NOT parity, and repeated-operand cancellation before invoking the solver. It
does not distribute ANDs or insert a hand-written inverse for a benchmark.
Stable creation IDs order operands, avoiding dependence on allocation addresses.

At runtime the stored polynomial computes one candidate, then the saved forward
circuit checks its output and permanent requirements. Queries perform no SAT
search. Construction also stores no full input/output table. The row system and
proof graph are discarded after certification.

## Measurements

GCC 15.2, C++23, Release, Windows, on the same machine as the previous steps.
The structured family consists of independent three-bit updates:

```text
(a, b, c) -> (a, b, c XOR (a AND b))
```

All rows below use degree two and the default limits except the marked 48-bit
row, which raises `max_solver_steps` from 2,000,000 to 20,000,000. Saved bytes
include the specialized forward circuit, input layout, polynomial and checksum.

| Unknown bits | Counterexamples | Solver calls | Counted solver steps | Build | Coefficient references | Saved bytes | Checked query |
|---|---:|---:|---:|---:|---:|---:|---:|
| 3 | 5 | 6 | 544 | 0.09 ms | 4 | 190 | 0.200 us |
| 6 | 9 | 10 | 4,700 | 0.26 ms | 8 | 291 | 0.214 us |
| 12 | 17 | 18 | 56,764 | 0.71 ms | 16 | 514 | 0.269 us |
| 24 | 33 | 34 | 786,423 | 3.75 ms | 32 | 974 | 0.504 us |
| 48, raised budget | 65 | 66 | 11,925,357 | 25.35 ms | 64 | 1,894 | 0.813 us |

The default 48-bit run stops at two million steps, after eight incorporated
counterexamples, during the ninth solver call. Raising only that limit tests
the cost of proof rather than changing the candidate representation. At 48 bits,
verification/miter construction takes 20.39 ms of the 25.35 ms total.

The first experiment, before XOR normalization, required 1,373,163 solver steps
for the 12-bit case and hit the default limit at 24 bits. The final 12-bit run
requires 56,764 steps, about 24 times fewer. These counted operations are not
CPU instructions. The before/after measurements isolate a useful representation
change; they do not establish an asymptotic solver bound.

Native forward execution of the 48-bit example takes roughly 0.011 us, while the
generic checked inverse API takes 0.813 us. Native code uses a packed integer;
the inverse timing includes bitvector allocation and forward interpretation.
Equal Boolean operation counts therefore do not imply equal measured runtime.
Timing samples are short and sensitive to host load; sizes and work counts are
the stronger reproducible results here. No process memory peak was measured.

Raw data: [default budgets](experiments/synthesis-step3.csv),
[raised solver budget](experiments/synthesis-step3-20m.csv),
[before XOR normalization](experiments/synthesis-step3-before-xor.csv).

## Controls and limits

Fixed-seed random permutations provide same-width controls. The three-bit
permutation succeeds, with 12 coefficient references. The four-bit control
produces inconsistent quadratic training equations. Five- and six-bit controls
hit the default proof limit; with 20 million steps they also reach inconsistent
training equations. For these bijections the witness is unique, so inconsistency
does rule out this degree-two basis for the sampled constraints, and hence for
the complete inverse. It does not rule out compact higher-degree or shared
circuit representations. The controls are supplied as truth-table-defined
forward circuits; their encoding and gate counts are not matched to the small
structured forward circuits.

The polynomial basis is intentionally narrow. A cubic controlled-XOR update
fails with degree two, and the ordinary three-bit update fails with degree one.
There is no automatic degree escalation, sparse coefficient optimization,
learned gate composition, or repair of previously chosen preimages.

For MD5, the default quadratic basis contains 8,257 features, above the default
4,096-feature budget. A separate degree-one trial on pattern `6d6435?1` (four
unknown bits, 31,240 forward nodes) passes the feature limit but reaches the
512-gate proof-depth cap before the first solver call. It creates no learned
artifact. This is an implementation limit, not evidence against a compact MD5
inverse. The prior bounded-selector backend remains available for MD5.

## Certification, resources, and persistence

Default CEGIS options are degree 2, 4,096 basis features, 256 counterexamples,
2,000,000 total solver steps, and 100,000 source/miter nodes. All limits must be
positive. Hard implementation caps are 256 unknown bits and 256 output bits,
16,384 features, 4,096 counterexamples, and 1,000,000 nodes. The symbolic proof
graph is limited to depth 512 because affine preprocessing is currently
recursive. XOR normalization also limits each traversal to `max_nodes` visits.

Solver work is counted across all proof calls, including search, propagation,
and affine elimination work. Both DPLL and CDCL now throw `solver_limit` on
exhaustion, even without a statistics pointer. The CEGIS builder translates this
to `cegis_limit` and never treats interruption as UNSAT. General solver callers
retain unlimited search by default (`max_search_steps == 0`).

These are deterministic work and graph bounds, not wall-time or process-memory
limits. Solver initialization and circuit preparation are outside the search
step counter; dimensions, node count and depth bound those phases separately.
`cegis_statistics` records basis size, counterexamples, solver calls/steps,
candidate terms/references, elapsed and verification time, certification status,
and the phase that failed. Verification time includes building the miter.

The versioned `LEARNED` backend saves only the candidate alongside the existing
forward program. Save/load preserve constant input bits and aliases. Loading
checks structure, indices, duplicate/unused terms and the outer checksum. As
with the older backends, loading trusts the builder's semantic certification;
it does not rerun SAT or verify an independently checkable proof certificate.
Deliberately forged metadata with a recomputed checksum is outside that trust
contract. The generated C++ has no project or solver dependency.

`evaluate(target, {})` and default `solve(target, callback)` return one valid
preimage. `solve` rejects limits other than one, including `--all`; this backend
does not silently omit additional preimages or fall back to query-time search.
The chosen preimage is not promised to be false-first canonical.

## Validation

The new tests cover exhaustive small reversible and many-to-one functions,
requirements on original and recovered inputs, unreachable outputs, aliases,
constant and unused inputs, empty/impossible domains, inadequate polynomial
families, resource exhaustion, depth limits, backend replacement, stream
formatting, corrupted/truncated archives, and source preservation on failure.
Separate budget tests exercise SAT, UNSAT and interruption in both engines,
with and without statistics and during streaming enumeration.

Native checks exhaust all inputs through 12 bits after save/load. At 24 and 48
bits, 4,096 deterministic probes are additional validation; the completeness
claim comes from the UNSAT miter, not those samples. Standalone exports compile
in isolation and pass the same native checks. The CLI test builds and learns a
nibble-specialized CRC32 program across processes, checks destination retention
on failed synthesis, and independently validates the exported header on all
256 one-byte CRC32 targets, including targets outside the baked domain.

The Release CTest run passes 11 of 13 checks. Two existing executables,
`synthesis_tests.exe` and `selector_tests.exe`, were locked at launch and then
disappeared after linking. Both suites pass when compiled separately with
`-O1 -g` and dynamic runtime linking. Thus all 13 test groups have passing
executions, but the default Release CTest run remains affected by that host issue.
No antivirus settings were changed.

## Reproduce and continue

```sh
cmake --build BUILD_DIR --target inverse_demo cegis_benchmark cegis_tests solver_budget_tests
ctest --test-dir BUILD_DIR --output-on-failure
cegis_benchmark --include-48 --output=out/cegis-examples
cegis_benchmark --include-48 --max-solver-steps=20000000 --output=out/cegis-examples-20m

inverse_demo build crc32 "6?" crc-source.bri
inverse_demo synthesize-cegis crc-source.bri crc-learned.bri --degree=1
inverse_demo solve crc-learned.bri e8b7be43
inverse_demo export-cpp crc-learned.bri crc_inverse.h
```

The next experiment should compose reversible updates so their inverses require
shared intermediate expressions beyond a fixed quadratic basis. Pair that with
removing the proof-depth limitation and measuring reduced-round MD5, preserving
the random controls. An external Manthan comparison remains unexecuted; the
[primary-source comparison note](CEGIS_COMPARISON.md) describes its interface,
platform requirements and independent verification requirements.
