# Inverse API

`inverse.h` compiles a tracked computation into an `inversion::program`: a
fixed relation `outputs = f(inputs)` with permanent requirements, plus one
backend that answers queries for new outputs. A program can be saved, loaded in
another process, converted to another backend and, for three backends,
exported as standalone C++. The [CLI](CLI.md) is a thin layer over this API.

```cpp
#include "inverse.h"
namespace br = dixelu::bitreverse;
namespace inv = br::inversion;
```

## Compiling

```cpp
// static member of inv::program
static program compile(const bits& inputs, const bits& outputs,
                       const bits& requirements = {});
```

- `inv::bits` is `std::vector<br::bit_tracker>`; `inv::values` is `std::vector<bool>`.
- `inputs` lists the input bits in the order results are returned. Each must be
  an unknown leaf or a constant; constants (known bits) are returned unchanged.
  Every unknown that an output or requirement depends on must be listed. An
  unknown that nothing depends on may be listed; it is then free, so each
  match also exists with that bit flipped.
- Copying a tracker copies its bits, so the copy is the same unknown. Listing
  one unknown twice makes both positions one variable. Create separate
  `br::unknown` values for independent bits.
- `requirements` are bits that must all be true for a match, for example a
  restricted input domain. They are saved with the program.
- `compile` copies the reachable DAG, merges structurally equal gates and
  reapplies the tracker's constant and identity rules. It then tries the affine
  backend and otherwise uses search.
- Make known values constants before running the algorithm, so tracking folds
  them early. Widths, loop counts and branches are fixed by the C++ run; to
  change them, compile another program.

`input_count()`, `unknown_count()` (distinct unknowns), `output_count()` and
`node_count()` (compiled circuit nodes) describe the forward relation.

```cpp
br::itu8 x = br::unknown;
const auto program = inv::program::compile(
    inv::bits_of(x), inv::bits_of(x ^ br::itu8(0x5a)),
    inv::bits{!x.bits[0]});          // requirement: the top bit of x is 0
// Affine, no free bits. Target 0x25 gives x = 0x7f; target 0xa5 has no match.
```

## Bit order

- `bits_of(int_tracker<N>)` lists bits least significant first.
  `bits_of(std::vector<int_tracker<N>>)` concatenates the integers in order,
  each least significant first. `int_tracker::bits` itself is most significant
  first.
- A target has the order of `outputs`; a result has the order of `inputs`.
- The unknown columns are the distinct unknowns in order of first appearance in
  `inputs`. Affine free bits follow this order.
- The canonical match of a target is its lexicographically smallest match, with
  false before true, comparing results in input order.

## Queries

| Call | Backends | Result |
|---|---|---|
| `evaluate(target, free_bits)` | All except search | One match or `std::nullopt`. Affine: `free_bits.size()` must equal `free_bit_count()`. Others: `free_bits` must be empty. |
| `solve(target, callback, max_solutions = 1, options = {}, statistics = nullptr)` | All | Calls `callback(input)` for each match until it returns false or `max_solutions` matches were emitted (0 = no limit). Returns the number emitted; 0 when nothing matches. |

| Backend | `evaluate` returns | `solve` order |
|---|---|---|
| Affine | The match for the given free bits | Free bits counted up as a little-endian binary number from all false |
| Search | (`std::logic_error`) | Solver order |
| BDD synthesis | Canonical match | Canonical order |
| Compiled selector | Canonical match | Canonical order |
| CEGIS-learned | One match, not necessarily canonical | One match; `max_solutions` must be 1 |

A target of the wrong width throws `std::invalid_argument`. `options` and
`statistics` are the solver's ([SOLVER.md](SOLVER.md)) and configure the search
backend; with `max_search_steps` set, an exhausted search throws
`solver_limit` rather than returning a short count. For the other backends,
`statistics` reports only `nodes`, `variables`, `solutions`, `elapsed` and
`affine_enabled` (true for the affine backend).

## Backends

A program has exactly one backend. `is_affine()`, `is_synthesized()`,
`is_selected()` and `is_learned()` identify it; all false means search.
`synthesized`, `selected` and `learned` build from the saved circuit and
return a new program with only that backend. On failure they throw and leave
the source program unchanged; none falls back to search. Backend-specific
accessors throw `std::logic_error` on other backends.

| Exception | Raised when |
|---|---|
| `synthesis_limit`, `selector_limit`, `cegis_limit` (all `std::runtime_error`) | A budget or hard limit stops a build |
| `solver_limit` | A search query exhausts `max_search_steps` |
| `std::logic_error` | A query or accessor needs another backend |
| `std::invalid_argument` | Invalid ports, wrong widths or free bits, invalid CEGIS options |

Errors while loading are described under [Saving and loading](#saving-and-loading).

### Affine

`compile` selects this backend when every output and requirement is an affine
function of the unknowns over GF(2). XOR and NOT are affine; AND and OR are
affine only when one operand is constant or both operands have the same
coefficients.
Programs with more than 2^20 nodes, unknowns or outputs, or needing more than
128 MiB of elimination storage, use search instead.

Compilation stores the reduced row-echelon form and the row operations, so a
query performs no elimination. Requirements act as extra outputs that must be 1.
With `k = free_bit_count()` non-pivot unknown columns, every reachable target
has 2^k matches: `evaluate(target, free_bits)` selects one and `solve`
enumerates them. A target outside the image has none.

### Search

The program stores the circuit only. Each query binds the outputs to the target
and the requirements to true and runs the solver: DPLL with affine propagation
by default, or CDCL with `conflict_learning` and `affine_reasoning = false`.
Every declared unknown is
assigned in every match. The worst case is exponential in the number of
unknowns.

### BDD synthesis

```cpp
program synthesized(const synthesis_options& options = {},
                    synthesis_statistics* statistics = nullptr) const;
```

The builder constructs reduced ordered BDDs, with target variables ordered
before unknown inputs, in four phases: `forward` (a BDD for every gate the
outputs and requirements reach), `relation` (targets and inputs, restricted to
the requirements), `witness` (eliminates the inputs, giving a validity function
of the target and one selector function per unknown bit that picks the
canonical match) and `compaction`. The program keeps the relation, the
selectors and the validity function.

`evaluate` follows the target-selected path through the validity function,
then through each selector; an unreachable target returns before any selector.
`solve` walks the relation for the target and lists every match in canonical
order. Neither searches.

| `synthesis_options` | Default | Meaning |
|---|---:|---|
| `max_nodes` | 750000 | Decision nodes created, including temporary ones; nothing is collected during a build |
| `max_operations` | 10000000 | BDD operations and traversal steps |

Both must be positive. Hard limits: output bits plus unknown bits at most 512,
and at most 1,000,000 decision nodes.

`synthesis_statistics` has `created_nodes`, `operations`, `elapsed`,
`peak_tracked_bytes`, `failed_phase` (`synthesis_phase::count` after success)
and `phases[4]`. Each phase records `started`, `completed`, `created_nodes`,
`operations`, `resident_nodes` (all nodes at the end of the phase),
`live_nodes` (reachable from the phase outputs; completed phases only),
`tracked_bytes`, `peak_tracked_bytes` and `elapsed`. Phases after a failed one
are not started; `synthesis_phase_name(phase)` names a phase. Tracked bytes
are allocation requests of the builder's diagrams, caches and work containers.
They exclude allocator overhead, the source circuit, the returned plan,
recursion frames and reachability scratch, so they are not process memory.

Sizes: `synthesized_node_count()` (all saved decision nodes),
`synthesized_relation_node_count()`, `synthesized_function_node_count()`
(selectors plus validity, the part `export_cpp` emits),
`synthesized_selector_node_count()` and `synthesized_validity_node_count()`.
Selector and validity nodes can overlap. A decision node is a Boolean
multiplexer, not comparable with forward gate counts.

`evaluate_profiled(target, &stats)` returns the same result as `evaluate` and
fills an `evaluation_statistics`: `decision_visits = validity_visits +
selector_visits`, where a node reached from several roots counts once per
visit. `evaluate_schedule(target, &stats)` evaluates every function node once,
in order, independent of the target, as a baseline.

### Compiled selector

```cpp
program selected(const selector_options& options = {},
                 selector_statistics* statistics = nullptr) const;
```

The builder enumerates all 2^u assignments of the u unknowns, 64 per forward
pass, drops those that fail a requirement, groups the rest by output and builds
a binary decision tree over target bits with one leaf per distinct output. It
then certifies the tree: every distinct output must reach its own leaf. The
program stores the tree and every allowed input, grouped by output in canonical
order, so storage grows with the domain.

`evaluate` walks the tree (at most `output_count()` decisions) and runs the
forward circuit once on the leaf's first input. If an output or requirement
disagrees, the result is `std::nullopt`; an unreachable target reaches an
arbitrary leaf and fails this check. `solve` lists the leaf's whole group after
the same check.

| `selector_options` | Default | Meaning |
|---|---:|---|
| `max_assignments` | 65536 | 2^u must not exceed it |
| `max_nodes` | 131071 | Tree decisions plus leaves; a tree with L leaves has 2L - 1 nodes |
| `max_operations` | 250000000 | Counted enumeration, sorting and tree work |
| `max_bytes` | 268435456 | Tracked allocation of the stored plan and the build |

All must be positive. Hard limits: u at most 20, u plus output bits at most
512, at most 2^21 - 1 tree nodes.

`selector_statistics` has `domain_assignments` (2^u), `allowed_assignments`
(meeting the requirements), `distinct_outputs` (leaves), `operations`,
`peak_bytes`, `enumeration_elapsed`, `tree_elapsed`, `validation_elapsed`
(certification), `elapsed` and `failed_phase` (`"none"`, `"enumeration"`,
`"tree"` or `"validation"`).

Sizes: `selector_node_count()`, `selector_leaf_count()`,
`selector_assignment_count()` (stored inputs) and
`selector_forward_node_count()` (gates of the forward check).
`evaluate_selected(target, &stats)` returns the same result as `evaluate` and
fills a `selector_evaluation_statistics` with `decision_visits` and
`forward_gate_evaluations`.

### CEGIS-learned

```cpp
program learned(const cegis_options& options = {},
                cegis_statistics* statistics = nullptr) const;
```

The candidate inverse sets each unknown bit to an XOR of basis features of the
target: the constant 1, each target bit and, at degree 2, each product of two
distinct target bits (1 + m + m(m-1)/2 features for m output bits). Starting
from all-zero coefficients, the builder repeats:

1. The solver (DPLL with affine propagation) looks for an allowed input `x`
   whose candidate `G(F(x))` breaks a requirement or has `F(G(F(x))) != F(x)`.
2. If it finds one, the target and input become a GF(2) equation on the
   coefficients; the accumulated system is solved, with free coefficients 0.
3. If it proves that no such `x` exists (UNSAT), the candidate is returned.

Only a completed UNSAT proof returns a program. It shows that the candidate
yields a match for every reachable target; the domain is never enumerated. If
the collected equations have no solution, the build fails. That does not show
that no polynomial inverse exists, because the learner fits the particular
inputs the solver returned.

`evaluate` computes the polynomial and checks it with the forward circuit. The
match it returns is valid but not necessarily canonical. `solve` with
`max_solutions` other than 1 throws `std::logic_error`.

| `cegis_options` | Default | Meaning |
|---|---:|---|
| `max_degree` | 2 | 1 or 2 |
| `max_features` | 4096 | Basis size; hard limit 16384 |
| `max_counterexamples` | 256 | Hard limit 4096 |
| `max_solver_steps` | 2000000 | Solver work summed over all proof queries |
| `max_nodes` | 100000 | Source circuit nodes, and miter nodes of each proof; hard limit 1,000,000 |

A degree other than 1 or 2, or a zero budget, throws `std::invalid_argument`.
Other hard limits: at most 256 unknown bits and 256 output bits, and a miter
depth of at most 512 gates (the solver's affine preprocessing recurses). An
interrupted proof throws `cegis_limit` and is never treated as UNSAT.

`cegis_statistics` has `features`, `counterexamples`, `solver_calls`,
`solver_steps`, `candidate_terms`, `candidate_coefficients` (of the last
candidate checked), `certified`, `elapsed`, `verification_elapsed` (building
and solving the proof queries) and `failed_phase` (`"none"`, `"preparation"`,
`"verification"` or `"learning"`). Sizes: `learned_term_count()` (distinct
terms used) and `learned_coefficient_count()` (term references over all
unknown bits).

## C++ export

`export_cpp(std::ostream&)` works for synthesized, selector and learned
programs and throws `std::logic_error` otherwise. It writes a `#pragma once`
header that needs only the standard library (C++17 or later) and defines:

```cpp
inline std::optional<std::array<bool, INPUTS>>
bitreverse_inverse(const std::array<bool, OUTPUTS>& target);
```

Bits use the program's order; known input bits are returned as constants. A
synthesized export holds the validity and selector functions, not the
relation. A selector export holds the tree, one input per leaf and the forward
check. A learned export holds the polynomial and the forward check. The output
does not depend on the stream's flags or locale.

## Saving and loading

```cpp
void save(std::ostream&) const;
static program load(std::istream&);
```

- The file is text: a `BITREVERSE_INVERSE 1` header, the circuit nodes and
  ports, a backend marker (`SEARCH`, `AFFINE`, `SYNTHESIZED`, `SELECTOR` or
  `LEARNED`) with its section, `CHECKSUM` and `END_INVERSE`. Each section has
  its own version number. Numbers are written in decimal regardless of stream
  flags, so a program always saves to the same bytes. The CLI uses binary
  streams.
- Every saved program keeps its forward circuit, so any file can be rebuilt
  with another backend. It holds no search state, learned clauses or partial
  enumeration.
- `load` checks the structure: field bounds (at most 1,000,000 nodes and ports
  per section), acyclicity, no unreachable nodes, port kinds, backend
  dimensions, the backend's own structure (reduced ordered BDD, tree shape and
  sorted input groups, valid polynomial terms), the checksum, and no trailing
  data. Malformed input throws `std::invalid_argument` or `std::runtime_error`.
- The checksum is a 64-bit FNV-1a hash of the canonical serialization. It
  detects accidental damage, not deliberate edits.
- `load` trusts the builder: it does not repeat synthesis, enumeration or the
  CEGIS proof, so a deliberately altered file can give wrong answers.
- Circuit node IDs are assigned breadth-first from the ports. `load`
  reserializes the rebuilt circuit to verify the checksum, so this numbering is
  part of the format.
