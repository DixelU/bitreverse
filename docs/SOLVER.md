# Solver architecture

[`bitreverse.h`](../include/bitreverse.h) remains the public umbrella header.
The headers in [`include/solver/`](../include/solver/) split the internal solver
implementation by responsibility:

- `options.h` and `statistics.h` define the public configuration and metrics.
- `compiled_circuit.h` assigns dense IDs, caches gate opcodes, and builds
  input adjacency plus contiguous parent lists with per-node offsets.
- `solver_state.h` owns assignments and the undo trail; a cursor into the
  trail serves as the propagation queue without duplicating assigned IDs.
- `propagation/gates.h` implements local AND/OR/XOR/NOT implications.
- `propagation/affine.h` implements optional GF(2) reasoning.
- `propagation/clauses.h` stores clause metadata, a shared literal arena,
  and two-watched-literal lists.
- `propagation/reasoned_gates.h` performs native CDCL gate implications while
  attaching a Tseitin reason to each one.
- `search/dpll.h` implements chronological DPLL search with an explicit
  decision stack and an ordered variable scan that resumes past ancestors.
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
Counts are deterministic for a given implementation and query, but can change
when an optimization eliminates work; they are not a versioned unit of time.

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

## Storage and propagation costs

Constants are initialized in the assignment array once, so querying a value
does not dereference its source expression. Parent lists retain the previous
parent order and repeated argument slots. Clause references use IDs and offsets
that survive arena growth; temporary literal spans must not survive an append.
Collected solutions are moved out of the engine instead of copied.

The affine propagator substitutes assigned atoms before elimination and builds
a row-echelon basis from the remaining equations. Dependent equations use a
single scratch row; a descending reduction of the independent rows finds the
forced atoms. Flat work buffers are reused between passes and rebuilt from the
current trail, so branch rollback cannot leave stale equations. Disabled affine
reasoning retains no per-node maps or atom vectors. The compiled coefficient
matrix still costs O(nodes * ceil(atoms / 64)) words when affine reasoning is active.

[Search optimization measurements](SEARCH_OPTIMIZATION.md) include reproducible
before/after timing and allocation data, validation, and remaining limitations.
