# Assertions during graph construction

Include `partial_assert.h` (with `include/` on the compiler's include search path)
to use an explicit constraint context without `program::compile`:

```cpp
#include "partial_assert.h"
namespace br = dixelu::bitreverse;

br::partial_assert_context constraints;
br::int_tracker<32> input = br::unknown;
auto state = input ^ br::int_tracker<32>(0x12345678);
const br::int_tracker<32> mask = 0x0100010f;
const br::int_tracker<32> observed = 0x00000105;

// Two-argument shorthand for tracked integer equality. Unselected bits on
// both sides are excluded from the comparison.
constraints.partial_assert(state & mask, observed & mask);
state = constraints.simplify(state);

// Build the rest of the algorithm from the simplified live values.
auto output = state + br::int_tracker<32>(7);
constraints.partial_assert(output, br::int_tracker<32>(0x1234550c));

// Keep the original input leaves, even if a simplified expression no longer
// mentions some of them. A zero result means UNSAT; one is the default limit.
std::vector<br::bit_tracker> inputs(input.bits.begin(), input.bits.end());
constraints.solve(inputs, [&](const br::collision_resolution::crs_state& model)
{
    // model.assignments maps original unknown leaves to their Boolean values.
    return true;
});
```

`partial_assert(condition)` also accepts any tracked Boolean expression.
For example, `partial_assert(x | y)` keeps both alternatives; adding
`partial_assert(!x)` subsequently forces `y`. It never chooses a satisfying
assignment and pretends it was a necessary consequence.

Every assertion retains its original relation, propagates AND/OR/XOR/NOT
implications, and optionally combines XOR equations over GF(2). Equality works
between different expression DAGs; shared subtrees are not required. Unresolved
relations remain constraints for the final solve. This version substitutes
proven constants; it does not merge arbitrary equal symbolic expressions.

Call `simplify` on the live values that later operations consume. It supports a
tracked bit, tracked integer, or vector of tracked integers. It returns a new
expression with inferred constants folded, preserves unresolved input identity,
and does not mutate the source DAG. A simplified expression is valid under its
context's constraints: use `context.solve` to retain them, or explicitly carry
`context.requirements()` into another solver. Simplifying a requirement to
`true` does not remove the original requirement from the context.

The API is incremental for callers. Each assertion currently rebuilds solver
adjacency from the accumulated roots and seeds previously inferred facts into a
fresh propagation state. It does not yet maintain persistent adjacency or
learned clauses between calls, so excessive checkpoints can cost more than they
save. Final solving also reuses the inferred facts alongside the original
constraints.

## Results and limits

`statistics()` describes the last successful assertion. Its `decisions` and
`solutions` are always zero: assertion propagation generates no complete
candidates. `known_variable_count()` counts forced original unknown leaves.
Successful propagation is not a satisfiability proof; nonlinear contradictions
can remain until `solve`. That method can enumerate models, with zero meaning
all models for its `max_solutions` argument. Returning false from its callback
stops enumeration; an empty callback just counts. Counts include every retained
unknown, including auxiliary unknowns in requirements, not only listed inputs.

A detected contradiction throws `partial_assert_conflict`. Exceeding the node
or propagation-step limit throws `partial_assert_limit`. Failed assertions leave
requirements, facts, and the last successful statistics unchanged. A final solve
has its own `solver_options::max_search_steps`; exhausting it throws
`solver_limit`, which must not be interpreted as UNSAT.

Defaults are 200,000 graph nodes, 2,000,000 propagation steps per assertion,
4,096 affine atoms, and 64 MiB for dense affine coefficients plus elimination
row coefficients. The byte cap excludes container metadata and ordinary graph
storage. Affine propagation falls back to gate propagation above its atom or
byte cap, or when an XOR/NOT chain exceeds 512 gates. Node and step caps are
positive; `max_affine_atoms = 0` disables affine reasoning. Compilation and
affine initialization are outside the propagation-step budget, bounded instead
by the graph, coefficient, and depth checks. `simplify` and `solve` also enforce
the context's graph node cap.

## CRC32 and MD5 experiments

Separate sources instrument the algorithms without changing `crc32.h` or
`md5.h`. Run these commands from the repository root after configuring a build:

```sh
cmake --build build --target partial_assert_tests partial_assert_crc32 partial_assert_md5
ctest --test-dir build -R partial_assert --output-on-failure
```

- [`examples/partial_assert_crc32.cpp`](../examples/partial_assert_crc32.cpp): two unknown prefix bytes, a known suffix, and
  16 observed bits of the internal CRC after the prefix.
- [`examples/partial_assert_md5.cpp`](../examples/partial_assert_md5.cpp): one unknown byte, three known bytes, and eight
  observed bits of the working state after the first MD5 round.

Each compares applying the checkpoint inside the algorithm against building the
whole algorithm before applying the **same checkpoint and final digest**. The
observed intermediate bits are additional supplied information. They are not
obtained from the final digest, and these cases do not establish a method for
general MD5 inversion. The chosen checkpoints completely determine the small
unknown domains, demonstrating the favorable case for early simplification.

Both examples independently evaluate recovered inputs using native arithmetic
and verify checkpoint uniqueness over their small domains. That reference
enumeration is outside the timed experiment and supplies no assignments to the
constraint context. Printed candidate counts refer to final solver models,
not that independent verification enumeration. Reported graph sizes count
reachable distinct DAG nodes, not tree width or peak allocations. Timings are
single-run measurements including assertion propagation and simplification,
and will vary by machine.

Release measurements on 2026-09-16, Windows x64, GCC 15.2 with `-O3`:

| Example | Checkpoint timing | Digest DAG nodes | Retained DAG nodes | Build + assertions | Final solve |
|---|---|---:|---:|---:|---:|
| CRC32 | Inside algorithm | 2 | 176 | 0.559 ms | 0.084 ms |
| CRC32 | After algorithm | 2,135 | 2,206 | 1.180 ms | 1.120 ms |
| MD5 | Inside algorithm | 2 | 65 | 0.584 ms | 0.040 ms |
| MD5 | After algorithm | 31,751 | 31,944 | 13.492 ms | 10.719 ms |

CRC32's checkpoint inferred all 16 unknown input bits; MD5's inferred all eight.
Both modes produced exactly one candidate with zero final search decisions.
The difference here is construction and propagation cost, not the number of
surviving solutions. Two digest nodes means the outputs became references to
the shared `true` and `false` constants; the retained graph still includes the
checkpoint constraints needed to justify those constants.

Validation: the new unit tests, both example self-checks, and the existing bit
tracker, solver-budget, inverse, and affine-inverse suites passed (7 CTest
targets). Unit tests compare small randomly generated Boolean formulas against
exhaustive native truth tables, check rollback and retained identities, and
exercise resource fallback, malformed DAG rejection, and both solver engines.
