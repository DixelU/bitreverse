#include <iostream>

#include "multiplication_reversal.h"

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

			for (const auto& single_char : private_key.bits)
			{
				const bool val = solution.assignments.at(single_char.bit_state);
				std::cout << (val ? '1' : '0');
			}
			std::cout << std::endl;
		});

	return counter > 0;
}

int main()
{
	const uint_arbitrary_t<256> private_key = dixelu::bitreverse::unknown;

	// Example Public Key X and Y
	// You should replace these with your actual target point components
	ECPoint<256> target_pub;
	target_pub.x = from_hex<256>("92252af37a85ac73775808d8aef18e108430ab41d17984fce9be981af76f6af3");
	target_pub.y = from_hex<256>("d631cd2a1f63dbb42614c70a08313715fd86343c53d87195dc5bd8cd17cc186e");
	target_pub.is_infinity = false;

	return !verify_private_key<256>(private_key, target_pub);
}
