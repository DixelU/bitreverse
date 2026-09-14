# Solver architecture

`bitreverse.h` remains the public umbrella header. The headers in this
directory split the internal solver implementation by responsibility:

- `options.h` and `statistics.h` define the public configuration and metrics.
- `compiled_circuit.h` assigns dense IDs and builds input/parent adjacency.
- `solver_state.h` owns assignments, the propagation queue, and the undo trail.
- `propagation/gates.h` implements local AND/OR/XOR/NOT implications.
- `propagation/affine.h` implements optional GF(2) reasoning.
- `propagation/clauses.h` stores clauses and two-watched-literal lists.
- `propagation/reasoned_gates.h` performs native CDCL gate implications while
  attaching a Tseitin reason to each one.
- `search/dpll.h` implements chronological DPLL search.
- `search/cdcl.h` implements conflict learning and non-chronological
  backtracking.
- `solve.h` is the only search-engine dispatch point.

The implementation headers are included by `bitreverse.h` in their intended
internal namespaces; consumers should continue to include only
`bitreverse.h`.

Set `solver_options::max_search_steps` to a positive count to bound a query's
search work; zero preserves unlimited search. Both engines count deterministic
search and propagation units, including affine elimination and conflict
analysis. `solver_statistics::search_steps` reports consumed units after a
completed or interrupted run. Circuit compilation and engine initialization
are outside this search budget.

Budget exhaustion throws `solver_limit`, even without a statistics pointer.
It never returns a partial count that could be mistaken for UNSAT. Enumeration
callbacks may already have received models before a later budget exception;
returning false from the callback still stops normally after that model. A
first-model query proves UNSAT only when it returns zero without an exception.

## Conflict learning and affine reasoning

Set `solver_options::conflict_learning` to select CDCL. Circuit implications
carry Tseitin reasons, and conflicts are analyzed with first-UIP learning.

Original circuit gates use native propagation and retain their Tseitin clauses
only as conflict-analysis reasons. Learned and model-blocking clauses use
two-watched-literal propagation. `max_conflict_analysis_nodes` bounds the work
spent resolving one conflict; exceeding it falls back to chronological branch
handling. Set it to zero for unlimited pure-CDCL comparisons.

Learned clauses that produce a non-chronological backjump stay active in the
watch lists. A clause that only reproduces the chronological phase change is
kept as the current implication reason without adding permanent watch-list
overhead.

The affine propagator does not yet emit explanation clauses. If it would be
active for a circuit, CDCL raises an explicit error; disable affine reasoning
for that comparison. Circuits over the configured affine atom cap already run
without affine propagation and can use CDCL directly.
