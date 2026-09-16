#include <iostream>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <vector>

#include "sha1.h"
#include "sha256.h"

namespace br = dixelu::bitreverse;

namespace
{
using bytes = std::vector<br::int_tracker<8>>;
using node = br::details::bitstate;
using values = std::unordered_map<const node*, bool>;

void require(bool condition, const std::string& message)
{
	if (!condition) throw std::runtime_error(message);
}

bytes tracked(const std::string& message)
{
	bytes result;
	for (unsigned char byte : message) result.emplace_back(byte);
	return result;
}

// Evaluate the shared DAG iteratively: these circuits exceed the recursion
// depth safe for a Windows thread. Unknown leaves must have explicit values.
bool evaluate(const node* root, values& known)
{
	std::vector<const node*> pending{root};
	while (!pending.empty())
	{
		const node* current = pending.back();
		if (known.contains(current))
		{
			pending.pop_back();
			continue;
		}
		if (current->operation == '=')
		{
			known.emplace(current, current->state != 0);
			pending.pop_back();
			continue;
		}
		require(current->operation != '*', "symbolic SHA evaluation has an unbound input");
		const node* lhs = current->_1.get();
		const node* rhs = current->_2.get();
		if (!known.contains(lhs))
		{
			pending.push_back(lhs);
			continue;
		}
		if (current->operation != '!' && !known.contains(rhs))
		{
			pending.push_back(rhs);
			continue;
		}
		bool result;
		switch (current->operation)
		{
			case '!': result = !known.at(lhs); break;
			case '&': result = known.at(lhs) && known.at(rhs); break;
			case '|': result = known.at(lhs) || known.at(rhs); break;
			case '^': result = known.at(lhs) != known.at(rhs); break;
			default: throw std::runtime_error("unexpected SHA circuit gate");
		}
		known.emplace(current, result);
		pending.pop_back();
	}
	return known.at(root);
}

template<size_t Width>
std::string hex_digest(const br::int_tracker<Width>& digest, values known = {})
{
	constexpr char alphabet[] = "0123456789abcdef";
	std::string result;
	for (size_t offset = 0; offset < Width; offset += 4)
	{
		unsigned nibble = 0;
		for (size_t bit = offset; bit < offset + 4; ++bit)
			nibble = (nibble << 1) | evaluate(digest.bits[bit].bit_state.get(), known);
		result += alphabet[nibble];
	}
	return result;
}

struct vector
{
	std::string message;
	const char* sha1;
	const char* sha256;
};

void known_vectors()
{
	// Empty, abc, and abcdbc... are standard SHA known-answer vectors.
	// Boundary/binary/1000-byte answers were generated independently with
	// System.Security.Cryptography.SHA1/SHA256.HashData, not these headers.
	std::vector<vector> cases{
		{"", "da39a3ee5e6b4b0d3255bfef95601890afd80709", "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"},
		{"abc", "a9993e364706816aba3e25717850c26c9cd0d89d", "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"},
		{std::string(55, 'a'), "c1c8bbdc22796e28c0e15163d20899b65621d65a", "9f4390f8d30c2dd92ec9f095b65e2b9ae9b0a925a5258e241c9f1e910f734318"},
		{std::string(56, 'a'), "c2db330f6083854c99d4b5bfb6e8f29f201be699", "b35439a4ac6f0948b6d6f9e3c6af0f5f590ce20f1bde7090ef7970686ec6738a"},
		{std::string(63, 'a'), "03f09f5b158a7a8cdad920bddc29b81c18a551f5", "7d3e74a05d7db15bce4ad9ec0658ea98e3f06eeecf16b4c6fff2da457ddc2f34"},
		{std::string(64, 'a'), "0098ba824b5c16427bd7a1122a5a442a25ec644d", "ffe054fe7ae0cb6dc65c3af9b61d5209f439851db43d0ba5997337df154668eb"},
		{std::string(65, 'a'), "11655326c708d70319be2610e8a57d9a5b959d3b", "635361c48bb9eab14198e76ea8ab7f1a41685d6ad62aa9146d301d4f17eb0ae0"},
		{std::string(1000, 'a'), "291e9a6c66994949b57ba5e650361e98fc36b1ba", "41edece42d63e8d9bf515a9ba6932e1c20cbc9f5a5d134645adb5db1b9737ea3"},
		{"abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq", "84983e441c3bd26ebaae4aa1f95129e5e54670f1", "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"}
	};
	std::string binary;
	for (unsigned byte = 0; byte < 256; ++byte) binary += static_cast<char>(byte);
	cases.push_back({binary, "4916d6bdb7f78e6803698cab32d1586ea457dfc8", "40aff2e9d2d8922e47afd4648e6967497158785fbd1da870e7110266bf944880"});
	for (const auto& test : cases)
	{
		const auto input = tracked(test.message);
		require(hex_digest(br::hash::sha1(input)) == test.sha1,
			"SHA-1 known vector failed at message length " + std::to_string(input.size()));
		require(hex_digest(br::hash::sha256(input)) == test.sha256,
			"SHA-256 known vector failed at message length " + std::to_string(input.size()));
	}
}

values assign_byte(const br::int_tracker<8>& input, unsigned byte)
{
	values result;
	for (size_t bit = 0; bit < 8; ++bit)
		result.emplace(input.bits[bit].bit_state.get(), ((byte >> (7 - bit)) & 1) != 0);
	return result;
}

void symbolic_vectors()
{
	bytes input{br::unknown};
	const auto sha1 = br::hash::sha1(input);
	const auto sha256 = br::hash::sha256(input);
	struct byte_vector { unsigned byte; const char* sha1; const char* sha256; };
	const byte_vector cases[] = {
		{0, "5ba93c9db0cff93f52b521d7420e43f6eda2784f", "6e340b9cffb37a989ca544e6bb780a2c78901d3fb33738768511a30617afa01d"},
		{1, "bf8b4530d8d246dd74ac53a13471bba17941dff7", "4bf5122f344554c53bde2ebb8cd2b7e3d1600ad631c385a5d7cce23c7785459a"},
		{127, "23833462f55515a900e016db2eb943fb474c19f6", "620bfdaa346b088fb49998d92f19a7eaf6bfc2fb0aee015753966da1028cb731"},
		{128, "c78ebd3c85a39a596d9f5cfd2b8d240bc1b9c125", "76be8b528d0075f7aae98d6fa57a6d3c83ae480a8469e668d7b0af968995ac71"},
		{255, "85e53271e14006f0265921d02d4d736cdc580b0b", "a8100ae6aa1940d0b663bb31cd466142ebbdbd5187131b92d93818987832eb89"}
	};
	for (const auto& test : cases)
	{
		const auto assignments = assign_byte(input.front(), test.byte);
		require(hex_digest(sha1, assignments) == test.sha1, "symbolic SHA-1 byte vector failed");
		require(hex_digest(sha256, assignments) == test.sha256, "symbolic SHA-256 byte vector failed");
	}

	// Carry symbolic state through a second compression block after padding.
	input = tracked(std::string(56, 'a'));
	input.front() = br::unknown;
	const auto assignments = assign_byte(input.front(), 'a');
	require(hex_digest(br::hash::sha1(input), assignments) ==
		"c2db330f6083854c99d4b5bfb6e8f29f201be699", "symbolic SHA-1 multi-block vector failed");
	require(hex_digest(br::hash::sha256(input), assignments) ==
		"b35439a4ac6f0948b6d6f9e3c6af0f5f590ce20f1bde7090ef7970686ec6738a",
		"symbolic SHA-256 multi-block vector failed");
}
}

int main()
{
	try
	{
		known_vectors();
		symbolic_vectors();
		std::cout << "SHA-1/SHA-256 known and symbolic vectors passed\n";
		return 0;
	}
	catch (const std::exception& error)
	{
		std::cerr << error.what() << '\n';
		return 1;
	}
}
