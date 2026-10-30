// The tracked hashes on concrete and symbolic messages, and solver reversal
// of a few unknown message bytes.

#include <array>
#include <cstddef>
#include <cstdint>
#include <initializer_list>
#include <string>
#include <string_view>
#include <vector>

#include "bitreverse.h"
#include "crc32.h"
#include "md5.h"
#include "reference_hashes.h"
#include "sha1.h"
#include "sha256.h"
#include "test_support.h"
#include "text.h"

namespace br = dixelu::bitreverse;
namespace cr = br::collision_resolution;
using test::require;

namespace
{

template<size_t Width>
std::string hex(const br::int_tracker<Width>& digest)
{
	return support::to_hex_number(test::constant_bits(test::lsb_first(digest)));
}

template<size_t Width>
std::string hex(const br::int_tracker<Width>& digest, const test::evaluator& eval)
{
	std::vector<bool> bits;
	for (const auto& bit : test::lsb_first(digest))
		bits.push_back(eval.bit(bit));
	return support::to_hex_number(bits);
}

// The digest bytes of a reference MD5 state, as conventionally printed.
std::string hex(const std::array<std::uint32_t, 4>& state)
{
	std::vector<std::uint8_t> bytes;
	for (const auto word : state)
		for (unsigned byte = 0; byte < 4; ++byte)
			bytes.push_back(static_cast<std::uint8_t>(word >> (8 * byte)));
	return support::to_hex(bytes);
}

struct known_answer
{
	std::string message;
	std::string_view md5, sha1, sha256;
};

std::string all_bytes()
{
	std::string bytes;
	for (unsigned byte = 0; byte < 256; ++byte)
		bytes += static_cast<char>(byte);
	return bytes;
}

// "", "abc" and "abcdbcde..." are standard SHA answers; "message digest", the
// alphanumerics and the digit string are RFC 1321 MD5 answers. The others were
// generated independently of these headers, with .NET
// System.Security.Cryptography and GNU coreutils. From 56 bytes on, the
// padding spills into another block.
const std::vector<known_answer> known_answers{
	{"", "d41d8cd98f00b204e9800998ecf8427e", "da39a3ee5e6b4b0d3255bfef95601890afd80709",
		"e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"},
	{"a", "0cc175b9c0f1b6a831c399e269772661", "86f7e437faa5a7fce15d1ddcb9eaeaea377667b8",
		"ca978112ca1bbdcafac231b39a23dc4da786eff8147c4e72b9807785afee48bb"},
	{"abc", "900150983cd24fb0d6963f7d28e17f72", "a9993e364706816aba3e25717850c26c9cd0d89d",
		"ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"},
	{"message digest", "f96b697d7cb7938d525a2f31aaf161d0", "c12252ceda8be8994d5fa0290a47231c1d16aae3",
		"f7846f55cf23e14eebeab5b4e1550cad5b509e3348fbc4efa3a1413d393cb650"},
	{std::string(55, 'a'), "ef1772b6dff9a122358552954ad0df65", "c1c8bbdc22796e28c0e15163d20899b65621d65a",
		"9f4390f8d30c2dd92ec9f095b65e2b9ae9b0a925a5258e241c9f1e910f734318"},
	{std::string(56, 'a'), "3b0c8ac703f828b04c6c197006d17218", "c2db330f6083854c99d4b5bfb6e8f29f201be699",
		"b35439a4ac6f0948b6d6f9e3c6af0f5f590ce20f1bde7090ef7970686ec6738a"},
	{"abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq", "8215ef0796a20bcaaae116d3876c664a",
		"84983e441c3bd26ebaae4aa1f95129e5e54670f1", "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"},
	{"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789", "d174ab98d277d9f5a5611c2c9f419d9f",
		"761c457bf73b14d27e9e9265c46f4b4dda11f940", "db4bfcbd4da0cd85a60c3c37d3fbd8805c77f15fc6b1fdfe614ee0a7c8fdb4c0"},
	{std::string(63, 'a'), "b06521f39153d618550606be297466d5", "03f09f5b158a7a8cdad920bddc29b81c18a551f5",
		"7d3e74a05d7db15bce4ad9ec0658ea98e3f06eeecf16b4c6fff2da457ddc2f34"},
	{std::string(64, 'a'), "014842d480b571495a4a0363793f7367", "0098ba824b5c16427bd7a1122a5a442a25ec644d",
		"ffe054fe7ae0cb6dc65c3af9b61d5209f439851db43d0ba5997337df154668eb"},
	{std::string(65, 'a'), "c743a45e0d2e6a95cb859adae0248435", "11655326c708d70319be2610e8a57d9a5b959d3b",
		"635361c48bb9eab14198e76ea8ab7f1a41685d6ad62aa9146d301d4f17eb0ae0"},
	{"12345678901234567890123456789012345678901234567890123456789012345678901234567890",
		"57edf4a22be3c955ac49da2e2107b67a", "50abf5706a150990a08b2c5ea40fa0e585554732",
		"f371bc4a311f2b009eef952dd83ca80e2b60026c8e935592d0f9c308453c813e"},
	{all_bytes(), "e2c865db4162bed963bfaa9ef6ac18f0", "4916d6bdb7f78e6803698cab32d1586ea457dfc8",
		"40aff2e9d2d8922e47afd4648e6967497158785fbd1da870e7110266bf944880"},
	{std::string(1000, 'a'), "cabe45dcc9ae5b66ba86600cca6b8ba8", "291e9a6c66994949b57ba5e650361e98fc36b1ba",
		"41edece42d63e8d9bf515a9ba6932e1c20cbc9f5a5d134645adb5db1b9737ea3"},
};

void known_vectors()
{
	for (const auto& [message, md5, sha1, sha256] : known_answers)
	{
		const auto input = test::tracked(message);
		const auto length = " at message length " + std::to_string(message.size());
		require(hex(br::hash::md5(input)) == md5, "MD5 known vector failed" + length);
		require(hex(br::hash::sha1(input)) == sha1, "SHA-1 known vector failed" + length);
		require(hex(br::hash::sha256(input)) == sha256, "SHA-256 known vector failed" + length);
	}
	require(test::to_integer(br::hash::crc32(test::tracked("123456789"))) == 0xcbf43926U,
		"tracked CRC must match the standard check vector");
}

// Other tests use the native reference implementations as oracles.
void reference_vectors()
{
	for (const auto& [message, md5, sha1, sha256] : known_answers)
		if (message.size() < 56)
			require(hex(reference::md5_one_block(message)) == md5, "reference MD5 known vector failed");
	// md5("md5!"), the four bytes packed little endian.
	require(hex(reference::md5_four_bytes(0x2135646dU)) == "b682c4079aa8cae8524986bf8de47f5c",
		"reference four-byte MD5 known vector failed");
	require(reference::crc32("123456789") == 0xcbf43926U, "reference CRC known vector mismatch");
}

void symbolic_vectors()
{
	std::vector<br::itu8> input{br::unknown};
	const auto sha1 = br::hash::sha1(input);
	const auto sha256 = br::hash::sha256(input);
	std::vector<br::bit_tracker> roots(input.front().bits.begin(), input.front().bits.end());
	roots.insert(roots.end(), sha1.bits.begin(), sha1.bits.end());
	roots.insert(roots.end(), sha256.bits.begin(), sha256.bits.end());
	test::evaluator eval(roots);
	struct byte_answer
	{
		unsigned byte;
		std::string_view sha1, sha256;
	};
	for (const auto& [byte, sha1_answer, sha256_answer] : {
		byte_answer{0, "5ba93c9db0cff93f52b521d7420e43f6eda2784f", "6e340b9cffb37a989ca544e6bb780a2c78901d3fb33738768511a30617afa01d"},
		byte_answer{1, "bf8b4530d8d246dd74ac53a13471bba17941dff7", "4bf5122f344554c53bde2ebb8cd2b7e3d1600ad631c385a5d7cce23c7785459a"},
		byte_answer{127, "23833462f55515a900e016db2eb943fb474c19f6", "620bfdaa346b088fb49998d92f19a7eaf6bfc2fb0aee015753966da1028cb731"},
		byte_answer{128, "c78ebd3c85a39a596d9f5cfd2b8d240bc1b9c125", "76be8b528d0075f7aae98d6fa57a6d3c83ae480a8469e668d7b0af968995ac71"},
		byte_answer{255, "85e53271e14006f0265921d02d4d736cdc580b0b", "a8100ae6aa1940d0b663bb31cd466142ebbdbd5187131b92d93818987832eb89"}})
	{
		eval.reset();
		eval.assign(input.front(), byte);
		eval.evaluate();
		require(hex(sha1, eval) == sha1_answer, "symbolic SHA-1 byte vector failed");
		require(hex(sha256, eval) == sha256_answer, "symbolic SHA-256 byte vector failed");
	}

	// Carry symbolic state through a second compression block after padding.
	auto message = test::tracked(std::string(56, 'a'));
	message.front() = br::unknown;
	const auto two_block_sha1 = br::hash::sha1(message);
	const auto two_block_sha256 = br::hash::sha256(message);
	roots.assign(message.front().bits.begin(), message.front().bits.end());
	roots.insert(roots.end(), two_block_sha1.bits.begin(), two_block_sha1.bits.end());
	roots.insert(roots.end(), two_block_sha256.bits.begin(), two_block_sha256.bits.end());
	test::evaluator two_blocks(roots);
	two_blocks.assign(message.front(), 'a');
	two_blocks.evaluate();
	require(hex(two_block_sha1, two_blocks) == "c2db330f6083854c99d4b5bfb6e8f29f201be699", "symbolic SHA-1 multi-block vector failed");
	require(hex(two_block_sha256, two_blocks) == "b35439a4ac6f0948b6d6f9e3c6af0f5f590ce20f1bde7090ef7970686ec6738a",
		"symbolic SHA-256 multi-block vector failed");
}

// Seven unknown bytes under CRC-32 make an affine system the search solves
// without enumerating them.
void crc32_reversal()
{
	const auto expected = br::hash::crc32(test::tracked("bitrev!"));
	auto message = test::unknown_bytes(7);
	cr::solution first;
	br::solver_statistics statistics;
	const size_t count = br::assert_equality<32>(br::hash::crc32(message), expected, br::solver_options{},
		[&](const cr::solution& solution)
		{
			first = solution;
			return false;
		}, &statistics);
	require(count == 1 && statistics.solutions == 1 && statistics.variables == 56 && statistics.affine_enabled &&
		statistics.affine_atoms != 0 && statistics.affine_passes != 0,
		"seven-byte CRC stream must find one solution before stopping");
	for (auto& byte : message)
		br::assign_assert_result<8>(byte, first.assignments);
	require(br::hash::crc32(message).to_string() == expected.to_string(), "reversed message must reproduce the target CRC");
}

void md5_reversal()
{
	const auto target = br::hash::md5(test::tracked("md5!"));
	auto message = test::tracked("md5");
	message.emplace_back(br::unknown);
	cr::solution first;
	require(br::assert_equality<128>(br::hash::md5(message), target, [&](const cr::solution& solution)
	{
		first = solution;
		return false;
	}) == 1, "one-byte MD5 reversal must find a solution");
	br::assign_assert_result<8>(message.back(), first.assignments);
	require(br::hash::md5(message).to_string() == target.to_string(), "reversed MD5 candidate must reproduce the target");
}

} // namespace

int main()
{
	return test::run("hash", {
		{"known_vectors", known_vectors},
		{"reference_vectors", reference_vectors},
		{"symbolic_vectors", symbolic_vectors},
		{"crc32_reversal", crc32_reversal},
		{"md5_reversal", md5_reversal},
	});
}
