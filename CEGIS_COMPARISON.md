# Manthan comparison backend

Primary-source review: 2026-09-14. This is an integration proposal, not a
completed benchmark. Manthan was not installed or executed in this workspace.

Manthan is a suitable external comparison for the third experiment in
[EXPLORATION_PLAN.md](EXPLORATION_PLAN.md): it synthesizes Boolean functions
that satisfy a relation wherever that relation has a witness. Its documented
interface consumes QDIMACS and produces Skolem functions in Verilog. The
documented setup uses Python 3.13, Python packages, and a shell script that
builds dependencies; the supported setup examples cover Ubuntu and macOS.
A separate Linux/WSL experiment is therefore the practical first integration
route for this Windows C++ project. That platform choice is an inference from
the documented workflow, not a claim that native Windows operation is
impossible. [Implementation and setup](https://github.com/meelgroup/manthan)

The Manthan2 paper describes dependency-aware multiclass learning,
interpolation for uniquely determined functions, variable retention, and
lexicographic MaxSAT repair. These are concrete comparison targets beyond a
small native counterexample-guided learner. The paper's benchmark results do
not establish performance for our specialized MD5 circuits.
[Manthan2 paper](https://arxiv.org/abs/2108.05717)

## Proposed relation export

Use the compiled forward circuit, declared unknown inputs, output IDs, and
permanent requirement IDs already present in `inverse.h`. Export a linear-size
Tseitin CNF for:

```text
R(target, input, auxiliary) = D(input) and gates(input, auxiliary)
                             and output(input, auxiliary) == target
```

Assign each distinct compiled node one ID. Repeated operands must reuse that
ID; unknown inputs optimized out of all outputs still need an existential
variable. Encode every gate as an equivalence, constants as unit clauses,
permanent requirements as true unit clauses, and each target/output equality
with both implications. This is an encoding of the existing DAG, not
enumeration of its satisfying assignments.

Emit a universal block for the target bits and an existential block for input
bits plus gate auxiliaries. Manthan uses the QDIMACS blocks to distinguish
function arguments and synthesized variables. Keep the IDs contiguous and
write a sidecar mapping for bit order and original node identity.
[Parser source](https://github.com/meelgroup/manthan/blob/master/src/preprocess.py)

Interpret this as a functional-synthesis specification: the desired condition
is `exists input, auxiliary R(target, input, auxiliary)` equivalent to
`R(target, G(target), H(target))`. It does **not** require the quantified formula
to be true for every target. Unreachable targets remain unconstrained. This
follows from Manthan's stated synthesis contract.
[Synthesis contract](https://github.com/meelgroup/manthan)

Auxiliaries are a real comparison cost: this straightforward encoding asks for
their Skolem functions too. After synthesis, retain the input-selector outputs
and their transitive dependencies for runtime, and count the complete generated
module separately. Do not count every auxiliary output as a required inverse
output or assume it can be deleted before dependency analysis. This is an
integration consequence of the proposed encoding.

## Execution and acceptance

An eventual bounded run can use the following documented CLI switches. This
command has not been run here; paths are examples relative to a separate
Manthan checkout.

```sh
python manthan.py --seed=10 --maxsamples=64 --maxrepairitr=64 \
    --preprocess=1 --unique=1 --multiclass=1 --lexmaxsat=1 \
    -o inverse_skolem.v inverse.qdimacs
python checkSkolem.py --qdimacs inverse.qdimacs --skolem inverse_skolem.v
```

The entry point exposes these controls and stops refinement when its repair
limit is exceeded. The sample and repair counts do not constitute an overall
wall-time or memory bound; the runner needs an external resource limit and
distinct `success`, `limit`, and `error` outcomes. Use an empty output directory
per run so a failed run cannot reuse an earlier successful artifact. Record the
exact repository and submodule revisions, flags, seed, platform, build tools,
and logs. Do not call a modern checkout with options disabled a reproduction
of the original 2020 implementation without pinning and checking that version.
[Entry point](https://github.com/meelgroup/manthan/blob/master/manthan.py)

The supplied independent checker builds a Boolean error formula and invokes
an ABC helper. In the source retrieved for this review, `check_skolem` returns
`unsat` for missing or empty counterexample files, and also when `_parse_cex`
cannot recover enough values. Its CLI prints a SAT counterexample without
raising a nonzero exit code. Therefore neither exit code zero nor the checker's
success message alone should certify an imported artifact. These are source
observations, not results from an executed failure test; recheck the pinned
revision before integration.
[Checker source](https://github.com/meelgroup/manthan/blob/master/checkSkolem.py)

Validate the retained candidate with our forward circuit and permanent domain
check, proving the following miter UNSAT:

```text
D(x) and (not D(G(F(x))) or F(G(F(x))) != F(x))
```

Treat timeout, malformed solver output, and missing results as inconclusive.
Only a proved candidate may expose exact no-match results. Runtime then
evaluates `G(target)` followed by `D(candidate)` and `F(candidate) == target`.
For small domains, also exhaustively check against native forward execution
to exercise export, bit ordering, and reload independently.

## Fair measurements

Report export size/time, synthesis time and peak memory, sample count, repair
count, verification time/status, generated module size, retained candidate
size, and checked query time. Compare against the existing BDD and enumerated
selector artifacts on identical domains. Circuit gate counts become comparable
only after applying the same optimization flow to every representation;
serialized bytes remain useful as a separate practical measure. Auxiliary
encoding overhead and process startup belong in construction measurements.

An external run remains outstanding. This review establishes a concrete
interchange boundary and acceptance criteria without adding Manthan's runtime
dependencies to the native library.
