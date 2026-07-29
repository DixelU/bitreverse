# Solver architecture

`bitreverse.h` remains the public umbrella header. The headers in this
directory split the internal solver implementation by responsibility:

- `options.h` and `statistics.h` define the public configuration and metrics.
- `compiled_circuit.h` assigns dense IDs and builds input/parent adjacency.
- `solver_state.h` owns assignments, the propagation queue, and the undo trail.
- `propagation/gates.h` implements local AND/OR/XOR/NOT implications.
- `propagation/affine.h` implements optional GF(2) reasoning.
- `search/dpll.h` implements chronological DPLL search.
- `solve.h` is the only search-engine dispatch point.

The implementation headers are included by `bitreverse.h` in their intended
internal namespaces; consumers should continue to include only
`bitreverse.h`.

## Adding conflict learning

The CDCL implementation should live in `search/cdcl.h` and reuse
`compiled_circuit` plus the common solver state where possible. Clause
storage/watch lists and implication reasons should be separate propagation
modules. Once implemented, `solve.h` should select CDCL when
`solver_options::conflict_learning` is true.

Until then, requesting conflict learning raises an error so benchmark runs
cannot silently execute baseline DPLL.
