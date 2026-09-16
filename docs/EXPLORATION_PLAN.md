# Exploring compact compiled inverses

The next milestone should be a certified inverse that selects an input and
checks it with one forward evaluation, built without constructing the complete
input/output relation. Compare its size and construction cost against both
decision diagrams and simple tables as the unknown domain grows.

This is a proposal for the next experiments. The existing prototype and its
measurements are described in [SYNTHESIS_RESULTS.md](SYNTHESIS_RESULTS.md).

**Progress:** the first two implementation steps are complete, and step three now
has a bounded counterexample-guided polynomial learner with UNSAT certification.
It discovers a 48-bit structured nonlinear inverse from 65 counterexamples,
stores 1,894 bytes, and takes 25.35 ms with a raised solver budget. Random
permutation controls expose the fixed quadratic family's limits. This is
structural inversion beyond table construction, but does not improve the MD5
result. See [step 3 results](CEGIS_RESULTS.md) and the
[external-backend comparison proposal](CEGIS_COMPARISON.md).

Earlier steps: lazy evaluation and
construction profiling are implemented. Checked
one-byte MD5 queries measured 0.71 microseconds versus 30.84 for the retained
schedule baseline. Both 10- and 12-bit MD5 builds stop during forward conversion,
before relation construction. The candidate-plus-forward-check backend now
builds the 20-bit MD5 domain in 3.86 seconds and passes independent verification
of all 1,048,576 inputs after serialization/reload. It still enumerates the domain
and stores a large tree. At that checkpoint the suite had ten passing checks.
[Step 2 details](SELECTOR_RESULTS.md). Next is learning shared intermediate
functions beyond the fixed polynomial basis, composing reversible updates, and
removing the current proof-depth limitation before further MD5 experiments.

## What we are trying to establish

Keep three questions separate:

1. Can we construct the inverse within the available time and memory?
2. How large is the saved executable inverse?
3. How much work does each new query require?

Exponential storage can buy fast lookup. The stronger objective is to discover
algorithmic structure that produces a compact inverse program. A successful
256-input MD5 example alone cannot distinguish those explanations. Include
table-derived selectors and random mappings with matching input/output widths
as controls.

Known parameters, message length, and permanent input restrictions remain baked
into every artifact. Repeated operands must retain their shared identity.

## First experiment: measure what the prototype actually does

Instrument forward-gate conversion, relation construction, witness extraction,
and final compaction separately. Record elapsed time, created nodes, live nodes,
and peak memory. The builder currently converts every reachable forward gate to
a BDD and retains its result before constructing the relation. Its node cap
counts all created nodes, and it has no garbage collection. The 12-bit failure
could occur before the full relation is even built.

Compare three evaluators on exactly the same artifact:

- The original full topological schedule, retained as a baseline.
- Per-root traversal following the supplied target bits, with optional sharing
  of already evaluated nodes.
- Input selectors followed by a forward check.

An ordered BDD over a 128-bit target needs at most 128 decisions along one path.
Consequently, the current 28,971 stored validity nodes do not imply 28,971
necessary operations on every query. Traversal improves evaluation without
reducing the saved graph's size. Measure both valid and invalid targets, and
compare actual C++ runtime with native forward evaluation under the same
compiler settings.

A read-only follow-up already confirms the opportunity: all 256 reachable MD5
targets recover correctly with a mean of 190.19 path visits across the validity
root and eight selectors, versus 29,757 evaluations in the original schedule.
The validity path alone takes 128 visits for reachable targets and a mean of
9.03 for 32 deterministic invalid targets. This is a structural operation
count, not a C++ timing result.

## Second experiment: replace separate validity synthesis

Let `F(x)` be the specialized forward algorithm and `D(x)` its permanent input
restrictions. Seek a total candidate function `G(y)` that returns a valid
preimage whenever one exists. Then the exact runtime interface is:

```text
x = G(y)
if D(x) and F(x) == y: return x
otherwise: return no match
```

For unreachable outputs, `G` may return anything. These unconstrained cases
give the optimizer freedom to simplify its Boolean functions. The current
builder already uses a limited form of this freedom when a whole branch is
invalid; the experiment should go further and optimize the selectors jointly.

The forward check is sound for every candidate, but rejection is complete only
after proving that `G` succeeds for every reachable target. With that proof,
the checked inverse has size bounded by the candidate, the forward circuit,
the domain check, and output comparison. It does not need an independently
synthesized image-membership circuit. This is a direct construction, not a
prediction that it will outperform every existing representation.

For a bounded baseline, enumerate inputs using native forward execution and
construct a decision tree on informative output bits. Stop splitting when the
remaining targets share a chosen input. Compare multi-terminal trees, separate
Boolean output functions, sorted tables, and hash tables. This can avoid the
large collection of intermediate forward-gate BDDs, but still pays for
enumeration; it establishes a baseline rather than demonstrating generalization.

## Third experiment: synthesize using counterexamples

Start with a small candidate `G`. Ask a SAT solver whether there is an `x`
satisfying:

```text
D(x) and (not D(G(F(x))) or F(G(F(x))) != F(x))
```

A satisfying assignment identifies a reachable output the candidate gets
wrong. Refine the candidate and repeat. UNSAT certifies that it works for every
reachable output. A timeout proves nothing about completeness; retain such a
candidate as an experiment, not an exact inverse artifact.

This check avoids explicitly constructing the image of `F`. Its own cost can
still be substantial. Record candidate size, verification time, number of
counterexamples, and the cost of each refinement. Verify final candidates with
an independent checker where practical.

This direction connects directly to Boolean functional synthesis. Manthan
uses learned candidates and reasoning-based refinement; Manthan2 adds
dependency-aware learning, interpolation, and improved candidate repair.
Use it as a comparison backend before reproducing its full machinery here.
Sources: [Manthan implementation](https://github.com/meelgroup/manthan),
[Manthan2 paper](https://arxiv.org/abs/2108.05717).

## Fourth experiment: preserve and exploit reversible structure

Build a ladder of test families whose difficulty can be adjusted independently:

| Family | What it tests |
|---|---|
| CRC32 and affine maps | Whether we preserve known algebraic inverses |
| Odd modular multiplication and XOR/shift combinations | Word-level identities and baked constants |
| Reversible conditional updates and Feistel-style networks | Composition of compact nonlinear inverses |
| Squaring, truncation, and restricted domains | Choice among multiple preimages |
| Reduced-round MD5 with varying unknown-bit placement | Where mixing defeats each representation |
| Full-round MD5 with bounded unknown domains | The current stress test |
| Random injections and random permutations | Compression attributable to the domain size alone |

For example, `(a, b) -> (a, b XOR h(a))` is inverted by the same update, even
when `h` is highly nonlinear. Keeping the controlling state makes inversion
local. An automatic compiler should recognize such structure before expanding
everything into individual bits.

Vary unknown width, placement of known bits, number of rounds, and input
restrictions separately. Include same-width random permutations as well as
sparse random mappings into wider outputs: their opportunities for exploiting
unreachable targets differ.

The success criterion is a growth curve showing smaller saved programs than
the table baseline, with exact verification. A single successful input width
or shorter query time does not establish that.

## Fifth experiment: change representation and witness choices

Use Boolean circuits that preserve shared AND/XOR logic alongside BDDs. Export
forward and inverse circuits through the same optimization and gate-counting
flow so comparisons use the same cost unit. Berkeley ABC supplies circuit
optimization and equivalence checking that can serve as a baseline.
[ABC project](https://github.com/berkeley-abc/abc).

For many-to-one algorithms, compare the current false-first witness against
choosing whichever valid preimage minimizes the joint output circuit. Build
time may be used to optimize that choice. A compact function selecting one
preimage does not automatically provide compact enumeration of every preimage;
keep those objectives separately measured.

For BDDs, compare input and output orders, complemented edges, liveness-based
collection, and decomposition. The current target-before-input ordering makes
extraction simple. Interleaving variables requires a different extraction
algorithm, not just renaming indices.

Also investigate synthesis-oriented representations beyond BDDs. The SAUNF
work shows that suitable representations can be exponentially more succinct
than BDDs and characterizes when compact functional solutions exist. This
makes a BDD construction failure insufficient evidence of a large minimum
inverse. [SAUNF paper](https://arxiv.org/abs/2104.14098).

## How far to take it

Run a sequence of bounded, reproducible experiments. Persist successful
artifacts, failing cases, seeds, compiler settings, budgets, and phase metrics.
Increase budgets only when the measurements indicate which hypothesis that
increase will test. Sampled success and formal completeness must remain
distinct in the results.

The broad conjecture that every small forward circuit has an equally small
inverse is stronger than the available evidence. General Boolean functional
synthesis has conditional worst-case size barriers. Those results do not
establish a lower bound for this MD5 specialization or eliminate useful
subclasses. [Hardness and practical synthesis](https://arxiv.org/abs/1804.05507).

The most valuable outcome would be an empirical map of which structures admit
compact inverses, which representations discover them, and where construction
or saved size starts to grow exponentially.
