# inverse_demo command line

`inverse_demo` builds, converts, inspects and queries saved inverses (`.bri`
files) of CRC32 and MD5 over byte messages. CMake builds it as
`build/inverse_demo` (`inverse_demo.exe` on Windows). The C++ API behind it is
described in [INVERSE.md](INVERSE.md).

## Conventions

```text
inverse_demo COMMAND POSITIONAL... [OPTIONS]
inverse_demo --help
```

- Options follow the positional arguments. A flag is `--name`; a value option
  is `--name=VALUE`. Numeric values are nonnegative decimal integers.
- A repeated value option keeps its last value. Exceptions in `solve`:
  `--limit` and `--all` may appear only once in total, and `--output` only once.
- Build reports, file descriptions and matches go to stdout; `solve` prints
  its status lines to stderr. Errors go to stderr as `Error: MESSAGE`,
  followed by the usage text.
- Exit codes: `0` success, `1` error, `2` no matching input (`solve` only).
- `--help` alone prints the usage text to stdout.

| Text | Format |
|---|---|
| `HEX_PATTERN` | Two hex digits per message byte, in message order. Each `?` is one unknown nibble: `6d64????` fixes two bytes, `6d6?????` fixes one byte and one nibble. Quote it so the shell does not expand `?`. |
| `TARGET_HEX` | The digest as usually printed, without `0x`: exactly 8 digits for CRC32 and 32 for MD5. Either case. |
| Printed matches | Two lowercase hex digits per byte, in message order, one message per line. |

The message length, the known nibbles and `--printable` are baked into the
file. To change them, run `build` again.

## build

```text
inverse_demo build crc32|md5 HEX_PATTERN FILE [--printable] [--synthesize [--max-nodes=N] [--max-operations=N]]
```

Runs the hash over the pattern with tracked bits, compiles the relation and
saves it to FILE. The file has the affine backend when the relation is affine
(CRC32 without `--printable`) and the search backend otherwise.

| Option | Effect |
|---|---|
| `--printable` | Requires every byte, known or unknown, to be in 0x20..0x7e. This makes CRC32 nonlinear. |
| `--synthesize` | Runs BDD synthesis, as `synthesize` does, before saving. |
| `--max-nodes=N`, `--max-operations=N` | Synthesis budgets. Only valid with `--synthesize`; defaults as for `synthesize`. |

It prints the saved path, the pattern, the restriction and a description of the
file (see `inspect`) to stdout. With `--synthesize`, the synthesis statistics
come first, and a budget failure behaves as in `synthesize`: FILE is not
written.

## synthesize, synthesize-selector, synthesize-cegis

```text
inverse_demo synthesize INPUT.bri OUTPUT.bri [--max-nodes=N] [--max-operations=N]
inverse_demo synthesize-selector INPUT.bri OUTPUT.bri [--max-assignments=N] [--max-nodes=N] [--max-operations=N] [--max-bytes=N]
inverse_demo synthesize-cegis INPUT.bri OUTPUT.bri [--degree=1|2] [--max-features=N] [--max-counterexamples=N] [--max-solver-steps=N] [--max-nodes=N]
```

Each command loads INPUT, builds one backend from its saved circuit and writes
a file with only that backend to OUTPUT. INPUT can have any backend.

| Command | Option | Default | Limits |
|---|---|---:|---|
| `synthesize` | `--max-nodes` | 750000 | Decision nodes created, including temporary ones; hard cap 1,000,000 |
| | `--max-operations` | 10000000 | BDD operations |
| `synthesize-selector` | `--max-assignments` | 65536 | Enumerated domain size, 2^(unknown bits); hard cap 2^20 |
| | `--max-nodes` | 131071 | Tree decisions plus leaves; hard cap 2,097,151 |
| | `--max-operations` | 250000000 | Counted enumeration and tree work |
| | `--max-bytes` | 268435456 | Tracked allocation, including the stored selector |
| `synthesize-cegis` | `--degree` | 2 | Polynomial degree, 1 or 2 |
| | `--max-features` | 4096 | Basis features; hard cap 16384 |
| | `--max-counterexamples` | 256 | Hard cap 4096 |
| | `--max-solver-steps` | 2000000 | Solver work summed over all proof queries |
| | `--max-nodes` | 100000 | Source circuit nodes and miter nodes; hard cap 1,000,000 |

[INVERSE.md](INVERSE.md) gives the other hard limits of each backend and the
meaning of its statistics.

- Budgets must be positive.
- On success the build statistics and a description of OUTPUT go to stdout.
- When a budget or hard limit stops the build, the statistics so far go to
  stderr and the command exits with 1.
- OUTPUT is written only after the build succeeds; a failed build neither
  creates nor changes it. For `synthesize-cegis`, success means the UNSAT
  certification completed.
- INPUT must be a different file from OUTPUT and is never modified.
- A learned (CEGIS) file gives one match per target: `solve` with `--all` or a
  limit other than 1 fails.

## export-cpp

```text
inverse_demo export-cpp INPUT.bri OUTPUT.h
```

Writes a standalone header for a synthesized, selector or learned file; other
files are an error. INPUT must differ from OUTPUT. The header starts with
`#pragma once`, includes only standard headers, compiles as C++17 or later,
and defines:

```cpp
inline std::optional<std::array<bool, INPUT_BITS>>
bitreverse_inverse(const std::array<bool, OUTPUT_BITS>& target);
```

`target[i]` is bit `i` of the digest read as a number; bit 0 is the low bit of
the last hex digit. Result index `8 * k + j` is bit `j` (least significant
first) of message byte `k`, known bits included. The function returns
`std::nullopt` when no input matches. Synthesized and selector headers return
the canonical match ([INVERSE.md](INVERSE.md#bit-order)); `solve --all` on the
`.bri` file lists every match.

## inspect

```text
inverse_demo inspect FILE
```

Prints the input, unknown and output bit counts, the circuit node count, the
backend, and the backend's sizes: free bits (affine), saved, relation,
selector and validity decision nodes (synthesized), tree nodes, leaves, stored
inputs and verifier nodes (selector), or terms and coefficient references
(learned).

## solve

```text
inverse_demo solve FILE TARGET_HEX [--limit=N|--all] [--output=PATH] [--no-affine] [--conflict-learning]
```

| Option | Effect |
|---|---|
| `--limit=N` | Stop after N matches. Default 1; 0 means no limit. |
| `--all` | Same as `--limit=0`. |
| `--output=PATH` | Also writes each match to PATH and flushes it after each line. PATH is truncated when `solve` starts and must differ from FILE. |
| `--no-affine` | Disables affine (GF(2)) propagation in the search. |
| `--conflict-learning` | Uses CDCL; affine propagation is off. |

`--no-affine` and `--conflict-learning` only affect files with the search
backend; the other backends do not search. `solve` prints the file description
to stderr, each match to stdout as it is found, and then
`Emitted N matching input(s).` to stderr. With no match it prints
`No matching input for this target and the baked constraints.` and exits
with 2. The order of matches depends on the backend; enumerating a large
family can take very long.

## Examples

```sh
# Five unknown bytes: the CRC32 relation has 8 free bits, so 256 matches per target.
inverse_demo build crc32 "??????????" crc5.bri
inverse_demo solve crc5.bri cbf43926 --limit=3
inverse_demo solve crc5.bri cbf43926 --all --output=all.hex

# A compiled selector over 16 unknown MD5 bits.
inverse_demo build md5 "6d64????" md5-16.bri
inverse_demo synthesize-selector md5-16.bri md5-selector.bri
inverse_demo solve md5-selector.bri b682c4079aa8cae8524986bf8de47f5c   # 6d643521

# A learned inverse for one unknown CRC32 nibble.
inverse_demo build crc32 "6?" crc-nibble.bri
inverse_demo synthesize-cegis crc-nibble.bri crc-learned.bri --degree=1
inverse_demo solve crc-learned.bri e8b7be43                            # 61
inverse_demo export-cpp crc-learned.bri crc_inverse.h
```
