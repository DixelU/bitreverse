#include <concepts>
#include <type_traits>
#include <string>
#include <iostream>

#include "bitreverse.h"

using namespace dixelu::bitreverse;

// Concrete placeholder for an arbitrary-precision integer type.
// Replace this with your actual template class (e.g., boost::multiprecision::number, etc.)
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

// Represents an affine point on the curve using the template type
template<std::size_t Bits>
struct ECPoint
{
	uint_arbitrary_t<Bits> x;
	uint_arbitrary_t<Bits> y;
	bit_tracker is_infinity = false;

	bit_tracker operator==(const ECPoint& other) const
	{
		bit_tracker both_inf = is_infinity & other.is_infinity;
		bit_tracker neither_inf = (!is_infinity) & (!other.is_infinity);
		bit_tracker coords_match = uint_arbitrary_t<Bits>::are_equal(x, other.x) & uint_arbitrary_t<Bits>::are_equal(y, other.y);
		return both_inf | (neither_inf & coords_match);
	}
};

// Curve Parameters scaled to the requested compile-time width
template<std::size_t Bits>
struct Secp256k1
{
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

// Helper modular inverse function (via Fermat's Little Theorem)
template<std::size_t Bits>
uint_arbitrary_t<Bits> mod_inverse(const uint_arbitrary_t<Bits>& a, const uint_arbitrary_t<Bits>& m)
{
	// Fermat's Little Theorem: a^(m-2) mod m
	uint_arbitrary_t<Bits> res(1);
	uint_arbitrary_t<Bits> base = a % m;
	uint_arbitrary_t<Bits> exp = m - uint_arbitrary_t<Bits>(2);

	for (volatile size_t i = 0; i < Bits; ++i)
	{
		res = (res * res) % m;
		uint_arbitrary_t<Bits> multiplied = (res * base) % m;
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

	const auto& p = Secp256k1<Bits>::p;

	// lambda = (3 * x^2) * inverse(2 * y) mod p
	uint_arbitrary_t<Bits> num = (uint_arbitrary_t<Bits>(3) * P.x * P.x) % p;
	uint_arbitrary_t<Bits> den = (uint_arbitrary_t<Bits>(2) * P.y) % p;
	uint_arbitrary_t<Bits> lambda = (num * mod_inverse(den, p)) % p;

	// x3 = (lambda^2 - 2 * x) mod p
	uint_arbitrary_t<Bits> x3 = (lambda * lambda + p + p - (uint_arbitrary_t<Bits>(2) * P.x) % p) % p;
	// y3 = (lambda * (x - x3) - y) mod p
	uint_arbitrary_t<Bits> y3 = (lambda * (P.x + p - x3) + p - P.y) % p;

	return ECPoint<Bits>{.x = x3, .y = y3, .is_infinity = res_is_inf};
}

template<std::size_t Bits>
ECPoint<Bits> ec_add(const ECPoint<Bits>& P, const ECPoint<Bits>& Q)
{
	bit_tracker x_equal = uint_arbitrary_t<Bits>::are_equal(P.x, Q.x);
	bit_tracker y_equal = uint_arbitrary_t<Bits>::are_equal(P.y, Q.y);

	const auto& p = Secp256k1<Bits>::p;

	// lambda = (y2 - y1) * inverse(x2 - x1) mod p
	uint_arbitrary_t<Bits> num = (Q.y + p - P.y) % p;
	uint_arbitrary_t<Bits> den = (Q.x + p - P.x) % p;
	uint_arbitrary_t<Bits> lambda = (num * mod_inverse(den, p)) % p;

	// x3 = (lambda^2 - x1 - x2) mod p
	uint_arbitrary_t<Bits> x3 = (lambda * lambda + p + p - P.x - Q.x) % p;
	// y3 = (lambda * (x1 - x3) - y1) mod p
	uint_arbitrary_t<Bits> y3 = (lambda * (P.x + p - x3) + p - P.y) % p;

	ECPoint<Bits> normal_res{.x = x3, .y = y3, .is_infinity = false};
	ECPoint<Bits> double_res = ec_double(P);

	// Selection logic
	auto select = [](const bit_tracker& cond, const ECPoint<Bits>& t, const ECPoint<Bits>& f) {
		return ECPoint<Bits>{
			.x = uint_arbitrary_t<Bits>::__execute_ternary_assign(cond, t.x, f.x),
			.y = uint_arbitrary_t<Bits>::__execute_ternary_assign(cond, t.y, f.y),
			.is_infinity = execute_ternary_operation(cond, t.is_infinity, f.is_infinity)
		};
	};

	ECPoint<Bits> res_if_x_eq = select(y_equal, double_res, ECPoint<Bits>{.is_infinity = true});
	ECPoint<Bits> res_P_Q = select(x_equal, res_if_x_eq, normal_res);

	return select(P.is_infinity, Q, select(Q.is_infinity, P, res_P_Q));
}

// Double-and-Add Scalar Multiplication (Unrolled for symbolic scalars)
template<std::size_t Bits>
ECPoint<Bits> ec_multiply(const uint_arbitrary_t<Bits>& scalar, ECPoint<Bits> base)
{
	ECPoint<Bits> result{.is_infinity = true};
	ECPoint<Bits> current_base = base;

	auto select = [](const bit_tracker& cond, const ECPoint<Bits>& t, const ECPoint<Bits>& f) {
		return ECPoint<Bits>{
			.x = uint_arbitrary_t<Bits>::__execute_ternary_assign(cond, t.x, f.x),
			.y = uint_arbitrary_t<Bits>::__execute_ternary_assign(cond, t.y, f.y),
			.is_infinity = execute_ternary_operation(cond, t.is_infinity, f.is_infinity)
		};
	};

	for (volatile size_t i = 0; i < Bits; ++i)
	{
		bit_tracker bit = scalar.bits[Bits - 1 - i];
		ECPoint<Bits> added = ec_add(result, current_base);
		result = select(bit, added, result);
		current_base = ec_double(current_base);
	}
	return result;
}

// --- The Main Verification Function ---

template<std::size_t Bits>
bool verify_private_key(const uint_arbitrary_t<Bits>& private_key, const ECPoint<Bits>& target_public_key)
{
	// 2. Compute P = private_key * G
	ECPoint<Bits> computed_public_key = ec_multiply<Bits>(private_key, Secp256k1<Bits>::G);

	size_t counter = 0;
	bit_tracker match = (computed_public_key == target_public_key);
	bit_tracker expected = true;
	dixelu::bitreverse::assert_equality(match, expected,
		[&](const dixelu::bitreverse::collision_resolution::crs_state& solution)
		{
			++counter;

			std::cout << "=== SOLUTION " << counter << " ===" << std::endl;

			for (auto single_char : private_key.bits)
			{
				bool val = solution.assignments.at(single_char.bit_state);
				std::cout << (val ? '1' : '0');
			}
			std::cout << std::endl;
		});

	return counter > 0;
}

int main()
{
	uint_arbitrary_t<256> private_key = dixelu::bitreverse::unknown;
	
	// Example Public Key X and Y
	// You should replace these with your actual target point components
	ECPoint<256> target_pub;
	target_pub.x = from_hex<256>("92252af37a85ac73775808d8aef18e108430ab41d17984fce9be981af76f6af3");
	target_pub.y = from_hex<256>("d631cd2a1f63dbb42614c70a08313715fd86343c53d87195dc5bd8cd17cc186e");
	target_pub.is_infinity = false;

	return !verify_private_key<256>(private_key, target_pub);
}
