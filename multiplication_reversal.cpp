#include <concepts>
#include <type_traits>

#include "bitreverse.h"

// Concrete placeholder for an arbitrary-precision integer type.
// Replace this with your actual template class (e.g., boost::multiprecision::number, etc.)
template<std::size_t Bits>
using uint_arbitrary_t = dixelu::bitreverse::int_tracker<Bits>;

// Represents an affine point on the curve using the template type
template<std::size_t Bits>
struct ECPoint
{
	uint_arbitrary_t<Bits> x;
	uint_arbitrary_t<Bits> y;
	bool is_infinity = false;

	bool operator==(const ECPoint& other) const
	{
		if (is_infinity && other.is_infinity)
			return true;
		if (is_infinity || other.is_infinity)
			return false;
		return (x == other.x) && (y == other.y);
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

// Helper modular inverse function (e.g., via Extended Euclidean Algorithm)
template<std::size_t Bits>
uint_arbitrary_t<Bits> mod_inverse(const uint_arbitrary_t<Bits>& a, const uint_arbitrary_t<Bits>& m);

// --- Core Point Arithmetic Functions ---

template<std::size_t Bits>
ECPoint<Bits> ec_double(const ECPoint<Bits>& P)
{
	if (P.is_infinity || P.y == uint_arbitrary_t<Bits>(0))
	{
		return ECPoint<Bits>{.is_infinity = true};
	}
	const auto& p = Secp256k1<Bits>::p;

	// lambda = (3 * x^2) * inverse(2 * y) mod p
	uint_arbitrary_t<Bits> num = (uint_arbitrary_t<Bits>(3) * P.x * P.x) % p;
	uint_arbitrary_t<Bits> den = (uint_arbitrary_t<Bits>(2) * P.y) % p;
	uint_arbitrary_t<Bits> lambda = (num * mod_inverse(den, p)) % p;

	// x3 = (lambda^2 - 2 * x) mod p
	uint_arbitrary_t<Bits> x3 = (lambda * lambda + p + p - (uint_arbitrary_t<Bits>(2) * P.x) % p) % p;
	// y3 = (lambda * (x - x3) - y) mod p
	uint_arbitrary_t<Bits> y3 = (lambda * (P.x + p - x3) + p - P.y) % p;

	return ECPoint<Bits>{.x = x3, .y = y3, .is_infinity = false};
}

template<std::size_t Bits>
ECPoint<Bits> ec_add(const ECPoint<Bits>& P, const ECPoint<Bits>& Q)
{
	if (P.is_infinity)
		return Q;
	if (Q.is_infinity)
		return P;
	if (P.x == Q.x)
	{
		if (P.y == Q.y)
			return ec_double(P);
		return ECPoint<Bits>{.is_infinity = true}; // P + (-P) = Infinity
	}
	const auto& p = Secp256k1<Bits>::p;

	// lambda = (y2 - y1) * inverse(x2 - x1) mod p
	uint_arbitrary_t<Bits> num = (Q.y + p - P.y) % p;
	uint_arbitrary_t<Bits> den = (Q.x + p - P.x) % p;
	uint_arbitrary_t<Bits> lambda = (num * mod_inverse(den, p)) % p;

	// x3 = (lambda^2 - x1 - x2) mod p
	uint_arbitrary_t<Bits> x3 = (lambda * lambda + p + p - P.x - Q.x) % p;
	// y3 = (lambda * (x1 - x3) - y1) mod p
	uint_arbitrary_t<Bits> y3 = (lambda * (P.x + p - x3) + p - P.y) % p;

	return ECPoint<Bits>{.x = x3, .y = y3, .is_infinity = false};
}

// Double-and-Add Scalar Multiplication
template<std::size_t Bits>
ECPoint<Bits> ec_multiply(uint_arbitrary_t<Bits> scalar, ECPoint<Bits> base)
{
	ECPoint<Bits> result{.is_infinity = true};
	ECPoint<Bits> current_base = base;

	while (scalar > uint_arbitrary_t<Bits>(0))
	{
		if ((scalar & uint_arbitrary_t<Bits>(1)) == uint_arbitrary_t<Bits>(1))
		{
			result = ec_add(result, current_base);
		}
		current_base = ec_double(current_base);
		scalar = scalar >> 1;
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
	dixelu::bitreverse::assert_equality(computed_public_key, target_public_key,
		[&](const dixelu::bitreverse::collision_resolution::crs_state& solution)
		{
			++counter;

			std::cout << "=== SOLUTION " << counter << " ===" << std::endl;

			for (auto single_char : private_key)
			{
				dixelu::bitreverse::assign_assert_result(
					single_char,
					solution.assignments);

				auto string = single_char.__to_string();
				auto value = std::stoi(string, 0, 2);

				std::cout << string << std::flush;
			}
			std::cout << std::endl;
		});

	return counter > 0;
}

int main()
{
	uint_arbitrary_t<256> private_key = dixelu::bitreverse::unknown;
	uint_arbitrary_t<256> public_key{};

	public_key |= 0x67b45a14; public_key <<= 32;
	public_key |= 0x714638ab; public_key <<= 32;
	public_key |= 0x73adb971; public_key <<= 32;
	public_key |= 0x2c742dfe; public_key <<= 32;
	public_key |= 0x9b5be26f; public_key <<= 32;
	public_key |= 0xf84e86f7; public_key <<= 32;
	public_key |= 0x5a054741; public_key <<= 32;
	public_key |= 0x05e92606; //public_key <<= 32;

	return !verify_private_key<256>(private_key, public_key);
}
