# Solver

The solver finds assignments of unknown leaves that give chosen nodes of a
tracked circuit chosen values. `bitreverse.h` includes it; include that header
rather than the files in `include/solver/`. The [inverse API](INVERSE.md) and
[partial assertions](PARTIAL_ASSERT.md) use it for their search queries.

## Entry points

| Function | Purpose |
|---|---|
| `assert_equality(lhs, rhs, ...)` | Solves `lhs == rhs` for two `bit_tracker`s or two `int_tracker<N>`s. Returns a `solutions_t` (all models, or one with `first_only`), or streams models to a callback and returns their count. Throws `unsatisfiable` (a `std::runtime_error`) when there is no model. |
| `collision_resolution::solve(bit, target, first_only, options, statistics)` | All models that drive `bit` to `target`, as a `solutions_t`; empty when none. |
| `collision_resolution::solve_stream(bit, target, callback, options, statistics)` | Streams models without keeping them; returns their count. |
| `collision_resolution::solve_compiled_stream(circuit, bindings, callback, options, statistics)` | Reuses a `compiled_circuit`; `bindings` lists `{node_id, value}` pairs. |
| `assign_assert_result(value, model.assignments)` | Replaces the unknown bits of a tracker by their values in a model. |

`options` defaults to `solver_options{}` and `statistics` to `nullptr`.
`collision_resolution::solution` holds `assignments`, a `std::map` from unknown
leaf (`bit.bit_state`) to its value; `solutions_t` is `std::set<solution>`.
A model assigns every unknown reachable from the constrained roots, and
unknowns that do not affect the result are enumerated both ways. Unknowns that
are not reachable do not appear. A callback returns `bool`, where false stops
after the current model; `assert_equality` also accepts callbacks returning
`void`.

```cpp
namespace br = dixelu::bitreverse;

const br::itu8 x = br::unknown;
for (const auto& model : br::assert_equality(x * br::itu8(3), br::itu8(0x2d)))
{
    auto value = x;
    br::assign_assert_result(value, model.assignments);
    // value.to_string() == "00001111": 15 * 3 == 45
}
```

## Options

| `solver_options` | Default | Meaning |
|---|---:|---|
| `affine_reasoning` | true | GF(2) propagation over XOR/NOT regions (DPLL only) |
| `max_affine_atoms` | 4096 | Affine propagation is off above this many atoms |
| `conflict_learning` | false | CDCL instead of DPLL |
| `max_conflict_analysis_nodes` | 512 | Work bound for analysing one conflict; 0 = unlimited |
| `max_search_steps` | 0 | Work budget per query; 0 = unlimited |

## Engines

**DPLL** (default) backtracks chronologically with an explicit decision stack.
Unknowns are tried in decreasing order of fan-out. The first value tried
follows the AND/OR parents that are already assigned. Gate propagation applies
local AND/OR/XOR/NOT implications from an undo trail.

**Affine propagation** writes every node as an affine form over atoms (unknown
leaves and AND/OR gates). Assigned nodes give linear equations, and Gaussian
elimination finds forced atoms. It is active only when the circuit has at least
one XOR or NOT gate and between 1 and `max_affine_atoms` atoms. It stores a
dense matrix of nodes × ceil(atoms / 64) words.

**CDCL** learns first-UIP clauses and backjumps non-chronologically. Circuit
gates propagate natively and keep their Tseitin clauses only as conflict
reasons; learned and model-blocking clauses use two watched literals. Every
model found is blocked, so all models are enumerated. When analysing one
conflict exceeds `max_conflict_analysis_nodes`, that conflict is handled
chronologically. CDCL throws `std::logic_error` if affine propagation would be
active: set `affine_reasoning = false` with `conflict_learning = true`.

## Budgets

- `max_search_steps` counts deterministic work units: decisions, propagation,
  affine elimination and conflict analysis. Compiling the circuit and setting
  up the engine are not counted.
- Exhausting it throws `solver_limit` (a `std::runtime_error`), with or without
  a statistics pointer; `statistics->search_steps` then holds the units used.
  A budgeted query never returns a count that looks like UNSAT: zero without an
  exception means there is no model.
- Models passed to a callback before the exception are valid.
- Step counts are reproducible for one build and query, but change when the
  implementation changes.

## Statistics

`solver_statistics` is reset at the start of each query.

| Field | Meaning |
|---|---|
| `nodes`, `variables` | Circuit nodes and unknown leaves |
| `decisions` | Branch assignments tried; both values of a variable count |
| `propagations` | Assigned nodes processed by propagation |
| `affine_passes`, `affine_atoms`, `affine_enabled` | Affine elimination passes, atoms counted, whether affine propagation ran |
| `conflicts` | Contradictions found |
| `solutions` | Models emitted |
| `learned_clauses`, `backjumps`, `conflict_analysis_cutoffs` | CDCL only |
| `peak_trail` | Most nodes assigned at once |
| `search_steps` | Budget units used |
| `elapsed` | Time in the search, also for interrupted runs |

## Source layout

| File | Contents |
|---|---|
| `options.h`, `statistics.h` | `solver_options`, `solver_limit`, `solver_statistics` |
| `solution.h` | `solution`, `solutions_t`, `solution_callback` |
| `compiled_circuit.h` | Dense node IDs, opcodes, children and parent lists |
| `solver_state.h` | Assignments, the undo trail and the step budget |
| `propagation/gates.h` | Gate implications |
| `propagation/affine.h` | GF(2) propagation |
| `propagation/clauses.h` | Clause storage with a shared literal arena and watched literals |
| `propagation/reasoned_gates.h` | Gate implications with Tseitin reasons, for CDCL |
| `search/session.h` | Request and result bookkeeping shared by both engines |
| `search/dpll.h`, `search/cdcl.h` | The engines |
| `solve.h` | Engine dispatch and the entry points |
