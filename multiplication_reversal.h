#pragma once

#include <cstddef>
#include <string>

#include "bitreverse.h"

using namespace dixelu::bitreverse;

// Fixed-width unsigned symbolic integer: arithmetic wraps modulo 2^Bits.
template<std::size_t Bits>
using uint_arbitrary_t = dixelu::bitreverse::int_tracker<Bits>;

// Helper to create uint_arbitrary_t from hex string
template<std::size_t Bits>
uint_arbitrary_t<Bits> from_hex(const std::string& hex) {
	uint_arbitrary_t<Bits> res(0);
	for (char c : hex) {
		res <<= 4;
		if (c >= '0' && c <= '9') res |= uint_arbitrary_t<Bits>(c - '0');
		else if (c >= 'a' && c <= 'f') res |= uint_arbitrary_t<Bits>(c - 'a' + 10);
		else if (c >= 'A' && c <= 'F') res |= uint_arbitrary_t<Bits>(c - 'A' + 10);
	}
	return res;
}

// Finite affine points use canonical field coordinates (0 <= x,y < p).
template<std::size_t Bits>
struct ECPoint
{
	uint_arbitrary_t<Bits> x{};
	uint_arbitrary_t<Bits> y{};
	bit_tracker is_infinity = false;

	bit_tracker operator==(const ECPoint& other) const
	{
		bit_tracker both_inf = is_infinity & other.is_infinity;
		bit_tracker neither_inf = (!is_infinity) & (!other.is_infinity);
		bit_tracker coords_match = uint_arbitrary_t<Bits>::are_equal(x, other.x) & uint_arbitrary_t<Bits>::are_equal(y, other.y);
		return both_inf | (neither_inf & coords_match);
	}
};

// The secp256k1 constants require at least 256 bits. Smaller test curves must
// specialize this template instead of truncating these parameters.
template<std::size_t Bits>
struct Secp256k1
{
	static_assert(Bits >= 256, "secp256k1 parameters require at least 256 bits");
	// Finite field prime p = 2^256 - 2^32 - 977
	static const uint_arbitrary_t<Bits> p;
	// Curve order n
	static const uint_arbitrary_t<Bits> n;
	// Base Generator Point G
	static const ECPoint<Bits> G;
};

template<std::size_t Bits>
const uint_arbitrary_t<Bits> Secp256k1<Bits>::p = from_hex<Bits>("fffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f");
template<std::size_t Bits>
const uint_arbitrary_t<Bits> Secp256k1<Bits>::n = from_hex<Bits>("fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141");
template<std::size_t Bits>
const ECPoint<Bits> Secp256k1<Bits>::G = {
	.x = from_hex<Bits>("79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"),
	.y = from_hex<Bits>("483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8"),
	.is_infinity = false
};

// Widen BEFORE multiplication; widening its result cannot recover lost bits.
// Narrow only after reduction, when the result is guaranteed to fit in Bits.
template<std::size_t Bits>
uint_arbitrary_t<Bits> mod_multiply(
	const uint_arbitrary_t<Bits>& a, const uint_arbitrary_t<Bits>& b,
	const uint_arbitrary_t<Bits>& m)
{
	using wide = uint_arbitrary_t<2 * Bits>;
	return uint_arbitrary_t<Bits>{(wide{a} * wide{b}) % wide{m}};
}

// Addition and subtraction require canonical inputs (0 <= a,b < m).
// Their results stay canonical using one correction, without division.
template<std::size_t Bits>
uint_arbitrary_t<Bits> mod_add(
	const uint_arbitrary_t<Bits>& a, const uint_arbitrary_t<Bits>& b,
	const uint_arbitrary_t<Bits>& m)
{
	using extended = uint_arbitrary_t<Bits + 1>;
	const auto sum = extended{a} + extended{b};
	auto reduced = sum;
	bit_tracker no_underflow;
	reduced.self_sub_ret_carry(extended{m}, no_underflow);
	return uint_arbitrary_t<Bits>{
		extended::__execute_ternary_assign(no_underflow, reduced, sum)};
}

template<std::size_t Bits>
uint_arbitrary_t<Bits> mod_subtract(
	const uint_arbitrary_t<Bits>& a, const uint_arbitrary_t<Bits>& b,
	const uint_arbitrary_t<Bits>& m)
{
	auto difference = a;
	bit_tracker no_underflow;
	difference.self_sub_ret_carry(b, no_underflow);
	// On underflow, adding m modulo 2^Bits cancels the earlier wraparound.
	return uint_arbitrary_t<Bits>::__execute_ternary_assign(
		no_underflow, difference, difference + m);
}

inline bool is_known_bit(const bit_tracker& bit, bool value)
{
	return bit.bit_state->operation == '=' &&
		static_cast<bool>(bit.bit_state->state) == value;
}

template<std::size_t Bits>
ECPoint<Bits> select_point(
	const bit_tracker& condition, const ECPoint<Bits>& t, const ECPoint<Bits>& f)
{
	return ECPoint<Bits>{
		.x = uint_arbitrary_t<Bits>::__execute_ternary_assign(condition, t.x, f.x),
		.y = uint_arbitrary_t<Bits>::__execute_ternary_assign(condition, t.y, f.y),
		.is_infinity = execute_ternary_operation(condition, t.is_infinity, f.is_infinity)
	};
}

// Fermat inversion requires prime m > 2 and a nonzero residue. At zero this
// circuit returns zero; point operations discard it for exceptional cases.
template<std::size_t Bits>
uint_arbitrary_t<Bits> mod_inverse(const uint_arbitrary_t<Bits>& a, const uint_arbitrary_t<Bits>& m)
{
	// Fermat's Little Theorem: a^(m-2) mod m
	uint_arbitrary_t<Bits> res(1);
	uint_arbitrary_t<Bits> base = a % m;
	uint_arbitrary_t<Bits> exp = m - uint_arbitrary_t<Bits>(2);

	for (size_t i = 0; i < Bits; ++i)
	{
		res = mod_multiply(res, res, m);
		// Square-then-multiply consumes the exponent MSB first; bits[0]
		// is the MSB. Skip unused products when the exponent bit is known.
		if (is_known_bit(exp.bits[i], false))
			continue;
		auto multiplied = mod_multiply(res, base, m);
		res = uint_arbitrary_t<Bits>::__execute_ternary_assign(exp.bits[i], multiplied, res);
	}
	return res;
}

// --- Core Point Arithmetic Functions ---

template<std::size_t Bits>
ECPoint<Bits> ec_double(const ECPoint<Bits>& P)
{
	bit_tracker is_zero_y = uint_arbitrary_t<Bits>::are_equal(P.y, uint_arbitrary_t<Bits>(0));
	bit_tracker res_is_inf = P.is_infinity | is_zero_y;
	if (is_known_bit(res_is_inf, true))
		return ECPoint<Bits>{.is_infinity = true};

	const auto& p = Secp256k1<Bits>::p;

	// lambda = (3 * x^2) * inverse(2 * y) mod p
	const auto square = mod_multiply(P.x, P.x, p);
	const auto num = mod_add(mod_add(square, square, p), square, p);
	const auto den = mod_add(P.y, P.y, p);
	const auto lambda = mod_multiply(num, mod_inverse(den, p), p);

	// x3 = (lambda^2 - 2 * x) mod p
	const auto x3 = mod_subtract(mod_multiply(lambda, lambda, p), mod_add(P.x, P.x, p), p);
	// y3 = (lambda * (x - x3) - y) mod p
	const auto y3 = mod_subtract(mod_multiply(lambda, mod_subtract(P.x, x3, p), p), P.y, p);

	return ECPoint<Bits>{.x = x3, .y = y3, .is_infinity = res_is_inf};
}

template<std::size_t Bits>
ECPoint<Bits> ec_add(const ECPoint<Bits>& P, const ECPoint<Bits>& Q)
{
	if (is_known_bit(P.is_infinity, true))
		return Q;
	if (is_known_bit(Q.is_infinity, true))
		return P;

	bit_tracker x_equal = uint_arbitrary_t<Bits>::are_equal(P.x, Q.x);
	bit_tracker y_equal = uint_arbitrary_t<Bits>::are_equal(P.y, Q.y);

	const auto& p = Secp256k1<Bits>::p;

	ECPoint<Bits> result{.is_infinity = true};
	if (!is_known_bit(x_equal, true))
	{
		// lambda = (y2 - y1) * inverse(x2 - x1) mod p
		const auto num = mod_subtract(Q.y, P.y, p);
		const auto den = mod_subtract(Q.x, P.x, p);
		const auto lambda = mod_multiply(num, mod_inverse(den, p), p);
		const auto x3 = mod_subtract(mod_subtract(mod_multiply(lambda, lambda, p), P.x, p), Q.x, p);
		const auto y3 = mod_subtract(mod_multiply(lambda, mod_subtract(P.x, x3, p), p), P.y, p);
		result = ECPoint<Bits>{.x = x3, .y = y3, .is_infinity = false};
	}
	if (!is_known_bit(x_equal, false))
	{
		ECPoint<Bits> double_res{.is_infinity = true};
		if (!is_known_bit(y_equal, false))
			double_res = ec_double(P);
		const auto same_x = select_point(y_equal, double_res, ECPoint<Bits>{.is_infinity = true});
		result = select_point(x_equal, same_x, result);
	}

	return select_point(P.is_infinity, Q, select_point(Q.is_infinity, P, result));
}

// Double-and-Add Scalar Multiplication (Unrolled for symbolic scalars)
template<std::size_t Bits>
ECPoint<Bits> ec_multiply(const uint_arbitrary_t<Bits>& scalar, ECPoint<Bits> base)
{
	ECPoint<Bits> result{.is_infinity = true};
	ECPoint<Bits> current_base = base;

	size_t first_bit = 0;
	while (first_bit < Bits && is_known_bit(scalar.bits[first_bit], false))
		++first_bit;
	for (size_t i = Bits; i-- > first_bit;)
	{
		// LSB first here: current_base carries the increasing power of two.
		const auto& bit = scalar.bits[i];
		if (!is_known_bit(bit, false))
			result = select_point(bit, ec_add(result, current_base), result);
		if (i > first_bit)
			current_base = ec_double(current_base);
	}
	return result;
}

