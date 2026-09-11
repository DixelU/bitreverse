# bitreverse

`bitreverse` runs integer algorithms over symbolic bits and finds inputs that
produce a requested output. `int_tracker<N>` follows fixed-width bit operations;
ordinary values become constants, while `unknown` creates symbolic input bits.

The reusable inverse API in `inverse.h` separates **building an inverse** from
**using it on new outputs**. Known input bits and algorithm parameters are baked
into the tracked computation before compilation, so constant folding and Boolean
simplification can remove their unnecessary work. The saved program retains the
input mapping, output relation, and permanent constraints across application
restarts. It is not tied to the output used by one particular solve.

An inverse can return several matching inputs, or none. A matching input is a
preimage; it need not be the original input. This matters for checksums and hashes,
where different inputs can have the same output.

## Three paths to an inverse

For an affine bit relation, compilation performs elimination over GF(2) and
constructs a direct inverse family. At runtime the target output and a choice of
free bits determine an input without search. With `k` free bits, every compatible
target has `2^k` matching assignments. Changing the free bits selects different
members of the family; incompatible targets have no match. CRC32 at a fixed
message length is the initial example of this path.

For nonlinear relations, **exact synthesis** can move the reasoning into the
build step. It constructs a reduced ordered binary decision diagram (BDD) of the
relation, eliminates unknown input variables, and builds Boolean functions that
select one canonical matching input from a new target. A validity function
rejects targets that have no match. The saved artifact retains these functions
and the relation: canonical evaluation needs no SAT solver, while `solve` can
enumerate distinct matching inputs by traversing the relation. Repeated operands
share variables and are constrained consistently throughout this construction.

The synthesized canonical function can also be exported as standalone C++ with
no project or solver dependency. Multiple preimages remain available through the
saved relation; the exported function selects one of them.

See [the first synthesis experiments](SYNTHESIS_RESULTS.md) for measured nonlinear
and MD5 results, including separate sizes for input recovery and target validation.

Without explicit synthesis, nonlinear relations are saved as reusable Boolean
circuits and use the existing solver for each new target. This is the default
MD5 path. Printable-character restrictions can also make a CRC32 problem
nonlinear. Either saved circuit can be synthesized later without rebuilding the
tracked algorithm.

The compiler simplifies the relation and specializes known values; it does not
guarantee a globally minimal circuit or a compact direct formula for every
algorithm. BDD construction can grow exponentially and depends on variable
ordering. Synthesis has explicit node and operation budgets and fails when one
is exceeded; it does not silently switch back to runtime search. Even a small
remaining input domain can require substantial build work for MD5. The saved
artifact contains the compiled inverse, not a suspended search, learned clauses,
or an enumeration checkpoint.

## Build and run

The current CMake configuration targets a GCC-compatible C++23 toolchain and
requires CMake 3.25 or newer. For example, in a shell where GCC and Ninja are on
`PATH`:

```sh
cmake -S . -B build -G Ninja -DCMAKE_BUILD_TYPE=Release
cmake --build build
ctest --test-dir build --output-on-failure
```

The executable is `build/inverse_demo` (`build/inverse_demo.exe` on Windows).

Build an inverse for nine-byte CRC32 messages whose first five bytes are `12345`:

```sh
build/inverse_demo build crc32 "3132333435????????" crc32-prefix.bri
build/inverse_demo inspect crc32-prefix.bri
build/inverse_demo solve crc32-prefix.bri cbf43926 --output=matches.hex
```

The matching input is printed as `313233343536373839` (the bytes of `123456789`)
and written to `matches.hex`. The four unknown bytes give this example no free
bits. The inverse file can be loaded by another process, or reused with another
CRC32 target, without tracking the algorithm again.

To build a family with multiple matching inputs:

```sh
build/inverse_demo build crc32 "??????????" crc32-five-bytes.bri
build/inverse_demo solve crc32-five-bytes.bri cbf43926 --limit=3
```

This unconstrained five-byte CRC32 relation has eight free bits, yielding 256
different five-byte inputs for every target. `--all` enumerates the complete
family. Enumeration can be enormous for larger inputs; the default is one result.

MD5 can likewise be compiled with a known prefix and one unknown byte:

```sh
build/inverse_demo build md5 "6d6435??" md5-prefix.bri --printable
build/inverse_demo solve md5-prefix.bri b682c4079aa8cae8524986bf8de47f5c --output=md5-matches.hex
```

This example searches printable messages beginning with `md5` and recovers
`6d643521`, the bytes of `md5!`. Supply another ordinary 32-digit MD5 digest to
reuse the same inverse for a different target.

To do that reasoning once and save executable inverse functions:

```sh
build/inverse_demo synthesize md5-prefix.bri md5-prefix-synth.bri
build/inverse_demo inspect md5-prefix-synth.bri
build/inverse_demo solve md5-prefix-synth.bri b682c4079aa8cae8524986bf8de47f5c
build/inverse_demo export-cpp md5-prefix-synth.bri md5_inverse.h
```

Alternatively, add `--synthesize` to `build`. Synthesis defaults to at most
750,000 created decision nodes and 10,000,000 operations. Both commands accept
positive `--max-nodes=N` and `--max-operations=N` limits. Raising them permits
more build work but does not ensure success. This implementation also has hard
ceilings of 1,000,000 created decision nodes and 512 combined unknown input and
target output bits. A failed synthesis leaves its source
and any existing destination unchanged and does not create a new destination.
`synthesize` and `export-cpp` require a destination different from their source.

The generated header defines `bitreverse_inverse`, accepting an
`std::array<bool, output_bit_count>` and returning an
`std::optional<std::array<bool, input_bit_count>>`. It evaluates the saved Boolean
functions directly and returns `std::nullopt` for an unreachable target. Its bit
ordering is the same as the API below; digest strings must first be decoded into
that ordering. The header needs only the C++ standard library and can be used
without this repository. `solve --all` on the `.bri` file enumerates the full set
of matches; the standalone function returns the canonical match only.

Build time, artifact size, and query cost measure different things. Synthesis
reports time, total created nodes, and operations, including temporary work.
`inspect` reports the retained decision nodes and the nodes reachable from the
relation and from the canonical functions plus validity. The latter sets may
overlap. It also separates the input selectors from the reachable-target check:
rejecting invalid outputs can cost much more than selecting an input when the
output is known to be reachable. A BDD node is a Boolean conditional, not one primitive AND/XOR gate, so
these counts are not directly comparable to the forward circuit's gate count.
Canonical evaluation executes the retained function graph; enumerating every
preimage additionally costs at least the number of results emitted.

`HEX_PATTERN` consists of known byte pairs such as `6d` and unknown pairs `??`.
Quote it to avoid shell wildcard expansion. Message length, known bytes, hash
constants, and `--printable` restrictions are baked at build time. `--printable`
means bytes `0x20` through `0x7e`, inclusive. To change a baked value or the input
length, build another inverse. The C++ API also supports individual unknown bits.

`solve` accepts the usual eight-digit CRC32 or 32-digit MD5 representation without
`0x`. It prints complete input messages as hexadecimal, one per line on stdout;
status goes to stderr. `--output=FILE` also writes each result to that file and
flushes it immediately, retaining emitted results when the application exits.
The output file is overwritten at the start of a solve. Exit codes are 0 for
success, 1 for an argument/file/runtime error, and 2 for no matching input.

Use `--limit=N` to bound results, or `--all` / `--limit=0` for unlimited enumeration.
Search options include `--no-affine` to disable the solver's affine propagation
and `--conflict-learning` to enable CDCL with affine propagation disabled. These
options control the search path; direct affine and synthesized inverses need no
solver at query time.

## C++ API

Include `inverse.h` alongside the algorithm you want to specialize. The algorithm
is ordinary C++ that operates on tracked values; it does not need an inverse
implementation of its own. Here is a complete example with a baked XOR parameter:

```cpp
#include <fstream>
#include "inverse.h"

namespace br = dixelu::bitreverse;
namespace inv = br::inversion;

int main()
{
    br::itu8 x = br::unknown;
    const br::itu8 mask = 0x5a;             // A baked algorithm parameter.
    const auto y = x ^ mask;
    const auto inverse = inv::program::compile(
        inv::bits_of(x), inv::bits_of(y));

    {
        std::ofstream file("xor.bri", std::ios::binary);
        inverse.save(file);
    }

    // This part can run in another process, without rebuilding x or y.
    std::ifstream file("xor.bri", std::ios::binary);
    const auto loaded = inv::program::load(file);
    inv::values target(8);
    for (size_t bit = 0; bit < target.size(); ++bit)
        target[bit] = ((0xa5u >> bit) & 1u) != 0;

    const inv::values free_bits(loaded.free_bit_count(), false);
    const auto input = loaded.evaluate(target, free_bits);
    if (!input)
        return 2;                         // Target incompatible with the relation.
    // *input contains the bits of 0xff, since 0xff ^ 0x5a == 0xa5.
}
```

`bits_of(integer)` returns bits least significant first. `bits_of(vector)`
concatenates that ordering for successive integers. `values` is
`std::vector<bool>`. Both `evaluate` and `solve` return values in the input order
passed to `compile`, including any known input bits. Keep unknown inputs as
distinct tracked values; copying one unknown tracker aliases its bits.

The optional third `compile` argument is a list of tracked Boolean requirements
that must all be true. For example, `inv::bits{!x.bits[0]}` requires the most
significant bit of an eight-bit `x` to be zero. Such requirements are saved with
the inverse and enforced for every target. Construct known values as constants
before running the algorithm so ordinary tracking can simplify them early.

For either affine or nonlinear programs, use `solve` to stream matches:

```cpp
const size_t count = loaded.solve(target,
    [](const inv::values& input) {
        // Consume or persist this complete matching input.
        (void)input;
        return true;              // false stops after this match.
    }, 10);                       // Maximum 10; zero means unlimited.
```

No matches return zero. For direct affine programs, `evaluate(target, free_bits)`
selects a family member and `free_bit_count()` gives the required number of free
bits. For synthesized programs, `evaluate(target, {})` returns the canonical
matching input; there is no affine free-bit parameterization of the nonlinear
family. Check `is_synthesized()` before `is_affine()` to identify the selected
backend, since synthesis can also be requested for an affine circuit.

To synthesize a compiled program and export its canonical inverse:

```cpp
inv::synthesis_options options;
options.max_nodes = 750000;
options.max_operations = 10000000;
inv::synthesis_statistics statistics;
const auto direct = loaded.synthesized(options, &statistics);
const auto input = direct.evaluate(target, {});
std::ofstream saved("direct.bri", std::ios::binary);
direct.save(saved);
std::ofstream header("direct_inverse.h");
direct.export_cpp(header);
```

`synthesized` returns a new program and keeps the original available if a resource
limit is exceeded. `synthesis_statistics` records `created_nodes`, `operations`,
`relation_nodes`, `function_nodes`, and `elapsed` (nanoseconds). Retained graph
sizes are available through `synthesized_node_count()`,
`synthesized_relation_node_count()`, and `synthesized_function_node_count()`.
Use `synthesized_selector_node_count()` and `synthesized_validity_node_count()`
to separate input recovery from target validation.
`export_cpp` requires a synthesized program.

`solve` can also take `solver_options` and a `solver_statistics*` after the result
limit; solver settings apply only to the search backend. Forward program size
is available through `input_count()`, `unknown_count()`, `output_count()`, and
`node_count()`.

Compilation fixes the tracked topology, widths, and loop counts. C++ control flow
cannot branch on a value that remains unknown: represent that choice using
tracked bit operations, or build a separate program for each fixed topology.
The serialization format is versioned; loading validates its structure and a
checksum covering the circuit and saved inverse to catch accidental corruption. Use
`program::load` to read it rather than relying on its internal representation.
