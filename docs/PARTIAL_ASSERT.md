# Assertions during construction

`partial_assert.h` provides `partial_assert_context`, which applies constraints
while an algorithm is being built. Each assertion propagates its consequences,
and `simplify` folds the values it proves into later expressions, so the rest
of the algorithm is built on a smaller graph. The context never chooses values;
only its `solve` searches. It is independent of `inversion::program`.

```cpp
#include "partial_assert.h"
namespace br = dixelu::bitreverse;

br::partial_assert_context constraints;
const br::itu32 input = br::unknown;
auto state = input ^ br::itu32(0x12345678);

// Observe the bits selected by mask; the masked-out bits are zero on both sides.
const br::itu32 mask = 0x0100010f;
constraints.partial_assert(state & mask, br::itu32(0x00000105) & mask);
state = constraints.simplify(state);        // fold the inferred constants

// Continue the algorithm from the simplified value, then assert its output.
const auto output = state + br::itu32(7);
constraints.partial_assert(output, br::itu32(0x1234550c));

// List the original input leaves: simplification may have removed some.
const std::vector<br::bit_tracker> inputs(input.bits.begin(), input.bits.end());
constraints.solve(inputs, [&](const br::collision_resolution::solution& model)
{
    auto value = input;
    br::assign_assert_result(value, model.assignments);   // 0x0000037d
    return true;                            // false stops the enumeration
});                                         // max_solutions defaults to 1; 0 = all
```

## API

| Member | Effect |
|---|---|
| `partial_assert(condition)` | Requires a tracked bit to be true. |
| `partial_assert(lhs, rhs)` | Requires two `int_tracker<N>` values to be equal. |
| `simplify(value)` | Returns a copy of a bit, an `int_tracker<N>` or a `std::vector<int_tracker<N>>` with every proven node replaced by its constant. |
| `solve(inputs, callback = {}, max_solutions = 1, options = {}, statistics = nullptr)` | Searches for models of all requirements; returns their count. |
| `requirements()`, `assertion_count()` | The asserted conditions, in order. |
| `known_variable_count()` | Unknown leaves with a proven value. |
| `statistics()` | `solver_statistics` of the last successful assertion. |

Assertions:

- Each assertion keeps the original condition, then propagates AND/OR/XOR/NOT
  implications and, within the affine limits, GF(2) reasoning over all
  requirements so far. The two sides of an equality need not share
  subexpressions.
- Only forced values are recorded. `partial_assert(x | y)` keeps both
  alternatives; a later `partial_assert(!x)` then forces `y`.
- Proven constants are substituted; equal symbolic expressions are not merged.
- An assertion is transactional: on any error the requirements, proven values
  and statistics stay as they were.
- Success is not a satisfiability proof. Contradictions that need search appear
  only in `solve`.
- Each assertion recompiles all requirements and reseeds the values proven so
  far. Nothing else carries over between calls, so many small checkpoints cost
  repeated work.

`simplify` does not change the source DAG, and unresolved unknowns keep their
identity. A simplified expression is valid only together with the context's
requirements: solve it with `context.solve`, or pass `context.requirements()`
to another solver. A requirement that simplifies to true stays in the context.

`solve` binds every requirement to true, adds the proven values and runs the
[solver](SOLVER.md). `inputs` must be unknown leaves or constants; listing them
keeps inputs that simplification removed from the requirements. A model
assigns every unknown in the requirements and the inputs, so the count is over
complete models, not only over the listed inputs. `max_solutions = 0`
enumerates all models; a callback returning false stops; an empty callback
only counts. `statistics()` reports `decisions == 0` and `solutions == 0`,
because assertions never branch.

## Options and errors

| `partial_assert_options` | Default | Meaning |
|---|---:|---|
| `max_nodes` | 200000 | Graph nodes per assertion, `simplify` and `solve` |
| `max_steps` | 2000000 | Propagation steps per assertion |
| `max_affine_atoms` | 4096 | 0 disables affine reasoning |
| `max_affine_bytes` | 67108864 | Dense affine coefficients plus elimination rows |
| `affine_reasoning` | true | |

`max_nodes`, `max_steps` and `max_affine_bytes` must be positive, or the
constructor throws `std::invalid_argument`. Affine reasoning is skipped, and
gate propagation still runs, when the atoms or the byte estimate exceed their
caps or a NOT/XOR chain is longer than 512 gates.

| Exception | Raised when |
|---|---|
| `partial_assert_conflict` | Propagation finds a contradiction |
| `partial_assert_limit` | The node or step budget is exceeded |
| `std::invalid_argument` | A null node, a cycle, an unsupported gate, or a `solve` input that is not a leaf |
| `solver_limit` | `solve` exhausts `options.max_search_steps`; this is not UNSAT |

## Examples

`examples/partial_assert_crc32.cpp` and `examples/partial_assert_md5.cpp`
hash a message whose leading bytes are unknown and observe one masked internal
value (a checkpoint). Each runs twice with the same two constraints: once
asserting the checkpoint inside the algorithm, once after the digest. They
print DAG sizes and timings for both and check the recovered input with a
native hash. The checkpoint is extra information, not derived from the digest.

```sh
cmake --build build --target partial_assert_crc32 partial_assert_md5
build/partial_assert_crc32
build/partial_assert_md5
```
