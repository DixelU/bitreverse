# Arithmetic circuits

`int_tracker<N>` (in `tracker.h`) is an unsigned N-bit integer of tracked bits.
Arithmetic wraps modulo 2^N. `bits` is stored most significant bit first.
`itu8`, `itu16`, `itu32` and `itu64` are aliases for common widths. This page
describes the Boolean circuits the operators build.

## Gates

Every gate is built by `make_bitstate_operation`, which folds constant operands
and applies local identities: idempotence, complements and absorption for
AND/OR, cancellation and complements for XOR, and double negation. The result
can therefore be an existing node. Unknown leaves keep their identity: a rule
such as replacing `!unknown` by `unknown` would be unsound when other
expressions use the same leaf, and is not applied.

Fully concrete operands fold to constants, so `int_tracker` with only known
values computes ordinary results. The operators are `constexpr`; during
constant evaluation each constant is a fresh node.

## Addition and subtraction

- Addition uses a Brent-Kung parallel-prefix carry scan: linear gate count and
  logarithmic carry depth, for any width, with a symbolic carry-in allowed.
  Fully concrete operands use a direct loop.
- `self_add_ret_carry(rhs, carry)` uses `carry` as carry-in and returns the
  carry-out in it.
- Subtraction computes `lhs + ~rhs + 1` with the same scan.
  `self_sub_ret_carry(rhs, carry)` ignores the incoming `carry` and sets it to
  true when the unsigned subtraction does not underflow.
- Unary minus is `~x + 1`.

## Multiplication

Multiplication builds one partial-product row (the multiplicand shifted and
ANDed with one multiplier bit) for every multiplier bit that is not constant
zero. The operand with fewer such bits is the multiplier, so a sparse constant
helps on either side. Groups of three rows are compressed to two by carry-save
adders, without horizontal carries, until two rows remain; one prefix addition
then produces the result. Carries above bit N-1 are discarded.

## Division and remainder

`/`, `%` and `divmod(divisor, remainder)` use non-restoring division with the
prefix adder in each step. The steps stay sequential. The divisor is classified
first:

| Divisor | Circuit |
|---|---|
| Constant zero | No gates: quotient all ones, remainder equal to the dividend |
| Constant power of two | Wiring: a shift and the low bits |
| Other constant | Remainder arithmetic on `bit_width(divisor) + 1` bits |
| Symbolic | Remainder arithmetic on N + 1 bits |

Leading dividend bits that are constant zero are skipped, so a small constant
numerator needs fewer steps; no divisor bits are dropped.

Division is total. Dividing by zero gives an all-ones quotient and the dividend
as remainder, for symbolic divisors as well: quotient bits skipped because of
known leading zeros stay tied to "divisor == 0". Hence `0 / x` is not
simplified to 0 and `x / x` is not simplified to 1. A remainder output may
alias either operand.

`prepare_divisor` classifies a constant divisor once for repeated use:

```cpp
namespace br = dixelu::bitreverse;

const auto by_ten = br::prepare_divisor<32>(10);
const br::itu32 value = br::unknown;
const auto quotient = value / by_ten;
const auto remainder = value % by_ten;
```

The prepared divisor also has `divide`, `modulo` and `divmod` members. It
accepts an `int_tracker<N>` or an integer, and throws `std::invalid_argument`
for a divisor with unknown bits. Each division still builds its own circuit.

## Shifts and other operations

- Shifting by a `size_t` is logical; an amount of N or more gives zero.
- Shifting by an `int_tracker<N>` builds a barrel shifter with one stage per
  amount bit i where 2^i < N. Higher amount bits are ignored.
- `if_then_else(condition, a, b)` selects between two bits or two
  `int_tracker<N>`s.
- `differs(a, b)` is the OR of the bitwise XOR; `int_tracker<N>::are_equal(a, b)`
  is the AND of the bitwise XNOR.
- `static_cast<bit_tracker>(x)` is true when any bit is set; `!x` when none is.
- Converting between widths zero-extends or truncates, keeping the low bits.
  Constructing from a `bit_tracker` puts it in bit 0.
- `to_string()` prints one character per bit, most significant first: `0`/`1`
  for constants, otherwise the gate character (`*` unknown, `!`, `&`, `|`, `^`).

## Measuring circuits

`circuit_metrics.h` provides `measure_circuit(bits)`. It counts each node of the
shared DAG once and reports nodes, edges, inputs, constants, gates by type,
depth (longest path, leaves at depth 0), width per depth, fan-out and output
depths. `int_tracker::max_depth()` counts unknown leaves as depth 1 and
constants as 0, so it can be one more than `measure_circuit`'s depth. The
`arithmetic_circuit_benchmark` executable prints these shapes for the operators
above at 8, 32, 64 and 256 bits.
