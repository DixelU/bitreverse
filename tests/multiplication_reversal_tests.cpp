// Modular and elliptic-curve arithmetic of multiplication_reversal.h, on
// constants and, through the gate evaluator, on symbolic operands.

#include <array>
#include <cstddef>
#include <cstdint>
#include <initializer_list>
#include <vector>

#include "multiplication_reversal.h"
#include "test_support.h"

// A complete small curve makes exhaustive scalar tests inexpensive. Its
// generator has order seven on y^2 = x^3 + 7 over the field of thirteen elements.
template<>
struct Secp256k1<4>
{
	static inline const uint_arbitrary_t<4> p{13};
	static inline const ECPoint<4> G{.x = 7, .y = 5, .is_infinity = false};
};

namespace
{

namespace br = dixelu::bitreverse;
using test::require;

bool constant_bit(const br::bit_tracker& bit)
{
	require(bit.is_constant(), "expected a concrete result bit");
	return bit.bit_state->state != 0;
}

template<std::size_t Bits>
void require_equal(const uint_arbitrary_t<Bits>& actual,
	const uint_arbitrary_t<Bits>& expected, const char* message)
{
	for (std::size_t i = 0; i < Bits; ++i)
		require(constant_bit(actual.bits[i]) == constant_bit(expected.bits[i]), message);
}

template<std::size_t Bits>
void append(std::vector<br::bit_tracker>& roots, const uint_arbitrary_t<Bits>& value)
{
	roots.insert(roots.end(), value.bits.begin(), value.bits.end());
}

void width_conversion_tests()
{
	for (unsigned value = 0; value < 16; ++value)
	{
		const uint_arbitrary_t<4> narrow{value};
		const uint_arbitrary_t<8> wide{narrow};
		require(test::to_integer(wide) == value, "widening must zero-extend");
		require(test::to_integer(uint_arbitrary_t<4>{wide}) == value, "width roundtrip changed the low bits");
	}
	const uint_arbitrary_t<8> all_bits{0xab};
	require(test::to_integer(uint_arbitrary_t<4>{all_bits}) == 0xb, "narrowing must retain the least-significant bits");
	const uint_arbitrary_t<4> input{br::unknown};
	const uint_arbitrary_t<8> extended{input};
	for (std::size_t i = 0; i < 4; ++i)
	{
		require(!constant_bit(extended.bits[i]), "symbolic widening must zero-fill upper bits");
		require(extended.bits[i + 4].bit_state == input.bits[i].bit_state, "symbolic widening should reuse its input nodes");
	}
	const uint_arbitrary_t<4> narrowed{extended};
	for (std::size_t i = 0; i < 4; ++i)
		require(narrowed.bits[i].bit_state == input.bits[i].bit_state, "symbolic narrowing should reuse its low-bit nodes");
}

void concrete_field_tests()
{
	const uint_arbitrary_t<4> modulus{13};
	for (unsigned a = 0; a < 16; ++a)
		for (unsigned b = 0; b < 16; ++b)
		{
			const uint_arbitrary_t<4> lhs{a}, rhs{b};
			require(test::to_integer(mod_multiply(lhs, rhs, modulus)) == (a * b) % 13,
				"modular multiplication lost high product bits");
			if (a < 13 && b < 13)
			{
				require(test::to_integer(mod_add(lhs, rhs, modulus)) == (a + b) % 13, "modular addition lost its carry");
				require(test::to_integer(mod_subtract(lhs, rhs, modulus)) == (a + 13 - b) % 13,
					"modular subtraction wrapped at the integer width");
			}
		}
	for (unsigned a = 1; a < 13; ++a)
		require((a * test::to_integer(mod_inverse(uint_arbitrary_t<4>{a}, modulus))) % 13 == 1,
			"Fermat inverse disagrees with the field inverse");
}

void symbolic_field_tests()
{
	const uint_arbitrary_t<4> lhs{br::unknown}, rhs{br::unknown};
	const uint_arbitrary_t<4> modulus{13};
	const auto product = mod_multiply(lhs, rhs, modulus);
	const auto sum = mod_add(lhs, rhs, modulus);
	const auto difference = mod_subtract(lhs, rhs, modulus);
	const auto inverse = mod_inverse(lhs, modulus);
	std::vector<br::bit_tracker> roots;
	for (const auto& value : {lhs, rhs, product, sum, difference, inverse})
		append(roots, value);
	test::evaluator eval(roots);
	for (unsigned a = 0; a < 16; ++a)
		for (unsigned b = 0; b < 16; ++b)
		{
			eval.reset();
			eval.assign(lhs, a);
			eval.assign(rhs, b);
			eval.evaluate();
			require(eval.word(product) == (a * b) % 13, "symbolic multiplication disagrees with integer arithmetic");
			if (a < 13 && b < 13)
			{
				require(eval.word(sum) == (a + b) % 13, "symbolic addition disagrees with integer arithmetic");
				require(eval.word(difference) == (a + 13 - b) % 13, "symbolic subtraction disagrees with integer arithmetic");
			}
			if (a > 0 && a < 13 && b == 0)
				require((a * eval.word(inverse)) % 13 == 1, "symbolic inverse disagrees with integer arithmetic");
		}
}

void secp256k1_tests()
{
	using integer = uint_arbitrary_t<256>;
	const auto& p = Secp256k1<256>::p;
	const integer one{1};
	const integer zero{0};
	const auto p_minus_one = p - one;
	require_equal(mod_multiply(p_minus_one, p_minus_one, p), one, "(p - 1)^2 must be one in the secp256k1 field");
	require_equal(mod_multiply(p, p, p), zero, "p^2 mod p must be zero");
	require_equal(mod_add(p_minus_one, p_minus_one, p), p - integer{2}, "secp256k1 addition must retain the 257th bit");
	require_equal(mod_add(p_minus_one, one, p), zero, "p must reduce to zero");
	require_equal(mod_subtract(zero, p_minus_one, p), one, "secp256k1 negative difference must reduce correctly");
	require_equal(mod_subtract(one, one, p), zero, "equal residues must subtract to zero");

	const ECPoint<256> twice{
		.x = from_hex<256>("c6047f9441ed7d6d3045406e95c07cd85c778e4b8cef3ca7abac09b95c709ee5"),
		.y = from_hex<256>("1ae168fea63dc339a3c58419466ceaeef7f632653266d0e1236431a950cfe52a"),
		.is_infinity = false};
	const ECPoint<256> thrice{
		.x = from_hex<256>("f9308a019258c31049344f85f89d5229b531c845836f99b08601f113bce036f9"),
		.y = from_hex<256>("388f7b0f632de8140fe337e62a37f3566500a99934c2231b6cb9fd7584b8e672"),
		.is_infinity = false};
	const auto& generator = Secp256k1<256>::G;
	require(constant_bit(ec_double(generator) == twice), "secp256k1 doubling does not match 2G");
	require(constant_bit(ec_add(generator, twice) == thrice), "secp256k1 addition does not match 3G");
}

void point_and_scalar_tests()
{
	const auto& generator = Secp256k1<4>::G;
	const ECPoint<4> infinity{.x = 0, .y = 0, .is_infinity = true};
	const ECPoint<4> negative{.x = 7, .y = 8, .is_infinity = false};
	require(constant_bit(ec_add(generator, infinity) == generator), "P + infinity must equal P");
	require(constant_bit(ec_add(infinity, generator) == generator), "infinity + P must equal P");
	require(constant_bit(ec_add(infinity, infinity).is_infinity), "infinity + infinity must be infinity");
	require(constant_bit(ec_double(infinity).is_infinity), "doubling infinity must preserve infinity");
	require(constant_bit(ec_add(generator, negative).is_infinity), "P + (-P) must be infinity");
	require(constant_bit(ec_double(ECPoint<4>{.x = 1, .y = 0}).is_infinity), "doubling a point with y = 0 must return infinity");
	require(constant_bit(ec_add(generator, generator) == ec_double(generator)), "P + P must use the doubling formula");

	// Independently calculated multiples on y^2 = x^3 + 7 mod 13.
	constexpr std::array<unsigned, 7> xs{0, 7, 8, 11, 11, 8, 7};
	constexpr std::array<unsigned, 7> ys{0, 5, 5, 8, 5, 8, 8};
	const uint_arbitrary_t<4> scalar{br::unknown};
	const auto symbolic_result = ec_multiply(scalar, generator);
	std::vector<br::bit_tracker> roots{symbolic_result.is_infinity};
	for (const auto& value : {scalar, symbolic_result.x, symbolic_result.y})
		append(roots, value);
	test::evaluator eval(roots);
	for (unsigned value = 0; value < 16; ++value)
	{
		const auto residue = value % 7;
		const auto concrete_result = ec_multiply(uint_arbitrary_t<4>{value}, generator);
		eval.reset();
		eval.assign(scalar, value);
		eval.evaluate();
		require(constant_bit(concrete_result.is_infinity) == (residue == 0),
			"concrete scalar multiplication returned the wrong infinity flag");
		require(eval.bit(symbolic_result.is_infinity) == (residue == 0),
			"symbolic scalar multiplication returned the wrong infinity flag");
		if (residue != 0)
		{
			require(test::to_integer(concrete_result.x) == xs[residue] && test::to_integer(concrete_result.y) == ys[residue],
				"scalar multiplication interpreted a scalar bit at the wrong weight");
			require(eval.word(symbolic_result.x) == xs[residue] && eval.word(symbolic_result.y) == ys[residue],
				"symbolic scalar multiplication disagrees with known multiples");
		}
	}
}

} // namespace

int main()
{
	return test::run("multiplication_reversal", {
		{"width_conversion", width_conversion_tests},
		{"concrete_field", concrete_field_tests},
		{"symbolic_field", symbolic_field_tests},
		{"point_and_scalar", point_and_scalar_tests},
		{"secp256k1", secp256k1_tests},
	});
}
