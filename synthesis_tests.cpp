#include <array>
#include <bit>
#include <cstdint>
#include <iostream>
#include <map>
#include <set>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

#include "inverse.h"
#include "md5.h"

namespace br = dixelu::bitreverse;
namespace inv = br::inversion;

namespace
{
using models = std::set<inv::values>;
using truth_table = std::map<inv::values, models>;

void require(bool condition, const char* message)
{
	if (!condition) throw std::runtime_error(message);
}

template<class Exception = std::exception, class Function>
void require_throws(Function&& function, const char* message)
{
	try { function(); }
	catch (const Exception&) { return; }
	throw std::runtime_error(message);
}

inv::values integer_bits(std::uint64_t value, size_t width)
{
	inv::values result(width);
	for (size_t bit = 0; bit < width; ++bit) result[bit] = ((value >> bit) & 1) != 0;
	return result;
}

inv::bits unknown_bits(size_t width)
{
	inv::bits result(width);
	for (auto& bit : result) bit = br::unknown;
	return result;
}

inv::program roundtrip(const inv::program& program)
{
	std::stringstream artifact;
	program.save(artifact);
	return inv::program::load(artifact);
}

models collect(const inv::program& program, const inv::values& target)
{
	models result;
	br::solver_statistics statistics;
	statistics.decisions = 71;
	statistics.propagations = 89;
	const auto count = program.solve(target, [&](const inv::values& value)
	{
		require(result.insert(value).second, "synthesis emitted duplicate preimages");
		return true;
	}, 0, {}, &statistics);
	require(count == result.size(), "synthesis returned an incorrect preimage count");
	require(statistics.decisions == 0 && statistics.propagations == 0,
		"synthesized queries must not invoke solver decisions or propagation");
	return result;
}

void check_target(const inv::program& program, const inv::values& target, const models& expected)
{
	const auto canonical = program.evaluate(target, {});
	if (expected.empty())
		require(!canonical, "canonical synthesis accepted an unreachable output");
	else
		require(canonical && *canonical == *expected.begin(),
			"canonical synthesis did not choose the false-first matching input");
	require(collect(program, target) == expected,
		"synthesized preimages disagree with independent forward truth table");
}

void check_table(const inv::program& original, const truth_table& expected)
{
	inv::synthesis_statistics statistics;
	const auto synthesized = original.synthesized({}, &statistics);
	require(synthesized.is_synthesized(), "synthesis did not select the direct nonlinear backend");
	require(!original.is_synthesized(), "synthesis must leave the original program unchanged");
	const auto loaded = roundtrip(synthesized);
	require(loaded.is_synthesized(), "saved synthesis lost its backend");
	require(loaded.synthesized_node_count() == synthesized.synthesized_node_count() &&
		loaded.synthesized_function_node_count() == synthesized.synthesized_function_node_count() &&
		loaded.synthesized_relation_node_count() == synthesized.synthesized_relation_node_count() &&
		loaded.synthesized_selector_node_count() == synthesized.synthesized_selector_node_count() &&
		loaded.synthesized_validity_node_count() == synthesized.synthesized_validity_node_count(),
		"synthesis node counts changed after persistence");
	require(original.output_count() < 16, "exhaustive output test is too large");
	for (size_t output = 0; output < (size_t{1} << original.output_count()); ++output)
	{
		const auto target = integer_bits(output, original.output_count());
		const auto found = expected.find(target);
		const models empty;
		const auto& inputs = found == expected.end() ? empty : found->second;
		check_target(synthesized, target, inputs);
		check_target(loaded, target, inputs);
	}
	require_throws<std::invalid_argument>([&] { loaded.evaluate(inv::values(original.output_count() + 1), {}); },
		"canonical synthesis must reject the wrong target width");
	require_throws<std::invalid_argument>([&] { loaded.evaluate(inv::values(original.output_count()), {true}); },
		"canonical nonlinear synthesis must reject free parameter bits");
	require_throws<std::invalid_argument>([&]
	{
		loaded.solve(inv::values(original.output_count() + 1), [](const auto&) { return true; });
	}, "synthesis enumeration must reject the wrong target width");
	require_throws<std::invalid_argument>([&] { loaded.solve(inv::values(original.output_count()), {}); },
		"synthesis enumeration must reject an empty callback");
}

void shared_operand_and_reversible_tests()
{
	const auto x = unknown_bits(3);
	truth_table toffoli;
	truth_table shared;
	for (unsigned input = 0; input < 8; ++input)
	{
		const auto v = integer_bits(input, 3);
		toffoli[{v[0] != (v[1] && v[2]), v[1], v[2]}].insert(v);
		shared[{v[0] && v[1], v[0] || v[1], v[0] && v[1]}].insert(v);
	}
	check_table(inv::program::compile(x, {x[0] ^ (x[1] & x[2]), x[1], x[2]}), toffoli);
	const auto shared_program = inv::program::compile(x, {x[0] & x[1], x[0] | x[1], x[0] & x[1]});
	check_table(shared_program, shared);
	const auto compiled = roundtrip(shared_program.synthesized());
	require(compiled.solve({false, true, false}, [](const auto&) { return true; }, 2) == 2,
		"synthesis must honor a solution limit");
	require(compiled.solve({false, true, false}, [](const auto&) { return false; }, 0) == 1,
		"synthesis must honor callback cancellation");
	require_throws<std::logic_error>([&] { (void)compiled.free_bit_count(); },
		"nonlinear branches must not claim a fixed affine parameter count");
}

template<size_t Width>
void arithmetic_tests()
{
	br::int_tracker<Width> x{br::unknown};
	truth_table odd, square;
	constexpr auto mask = (1U << Width) - 1;
	for (unsigned input = 0; input <= mask; ++input)
	{
		odd[integer_bits((input * 3U + 5U) & mask, Width)].insert(integer_bits(input, Width));
		square[integer_bits(input * input & mask, Width)].insert(integer_bits(input, Width));
	}
	check_table(inv::program::compile(inv::bits_of(x), inv::bits_of(x * br::int_tracker<Width>{3} + br::int_tracker<Width>{5})), odd);
	check_table(inv::program::compile(inv::bits_of(x), inv::bits_of(x * x)), square);
}

void random_circuit_tests()
{
	std::uint32_t random = 0x6fbd2731U;
	const auto next = [&]() { random ^= random << 13; random ^= random >> 17; random ^= random << 5; return random; };
	for (unsigned trial = 0; trial < 24; ++trial)
	{
		const auto inputs = unknown_bits(6);
		inv::bits gates = inputs;
		std::vector<inv::values> concrete;
		for (unsigned bit = 0; bit < 6; ++bit)
		{
			inv::values values(64);
			for (unsigned input = 0; input < 64; ++input) values[input] = ((input >> bit) & 1) != 0;
			concrete.push_back(std::move(values));
		}
		for (unsigned gate = 0; gate < 32; ++gate)
		{
			const size_t lhs = next() % gates.size(), rhs = next() % gates.size();
			const unsigned operation = next() % 4;
			inv::values values(64);
			switch (operation)
			{
			case 0: gates.push_back(gates[lhs] & gates[rhs]); break;
			case 1: gates.push_back(gates[lhs] | gates[rhs]); break;
			case 2: gates.push_back(gates[lhs] ^ gates[rhs]); break;
			default: gates.push_back(!gates[lhs]); break;
			}
			for (unsigned input = 0; input < 64; ++input)
			{
				const bool a = concrete[lhs][input], b = concrete[rhs][input];
				values[input] = operation == 0 ? a && b : operation == 1 ? a || b : operation == 2 ? a != b : !a;
			}
			concrete.push_back(std::move(values));
		}
		inv::bits outputs;
		std::vector<size_t> selected;
		for (unsigned output = 0; output < 5; ++output)
		{
			selected.push_back(next() % gates.size());
			outputs.push_back(gates[selected.back()]);
		}
		const size_t restriction = next() % gates.size();
		const bool restricted = trial % 3 == 0;
		truth_table expected;
		for (unsigned input = 0; input < 64; ++input)
		{
			if (restricted && !concrete[restriction][input]) continue;
			inv::values output;
			for (const auto gate : selected) output.push_back(concrete[gate][input]);
			expected[output].insert(integer_bits(input, 6));
		}
		check_table(inv::program::compile(inputs, outputs, restricted ? inv::bits{gates[restriction]} : inv::bits{}), expected);
	}
}

void restrictions_aliases_and_constants_tests()
{
	const auto x = unknown_bits(3);
	truth_table restricted, aliases, no_outputs;
	for (unsigned input = 0; input < 8; ++input)
	{
		const auto v = integer_bits(input, 3);
		if ((v[0] || v[1]) && !v[2]) restricted[{v[0] != v[1]}].insert(v);
		aliases[{v[0] && v[1]}].insert({v[0], v[0], v[1], v[2], true});
		no_outputs[{}].insert(v);
	}
	check_table(inv::program::compile(x, {x[0] ^ x[1]}, {x[0] | x[1], !x[2]}), restricted);
	check_table(inv::program::compile({x[0], x[0], x[1], x[2], br::bit_tracker(true)}, {x[0] & x[1]}), aliases);
	check_table(inv::program::compile(x, {}), no_outputs);
	check_table(inv::program::compile({}, {}), truth_table{{{}, models{inv::values{}}}});
	check_table(inv::program::compile({}, {br::bit_tracker(true), br::bit_tracker(false)}),
		truth_table{{{true, false}, models{inv::values{}}}});
	check_table(inv::program::compile(x, {x[0] & x[1]}, {br::bit_tracker(false)}), {});
	check_table(inv::program::compile(x, {x[0] ^ x[0]}), truth_table{{{false}, no_outputs.at({})}});
}

void limits_and_artifact_tests()
{
	const auto x = unknown_bits(3);
	const auto original = inv::program::compile(x, {x[0] ^ (x[1] & x[2]), x[1], x[2]});
	for (const bool node_limit : {false, true})
	{
		inv::synthesis_options options;
		if (node_limit) options.max_nodes = 2;
		else options.max_operations = 1;
		require_throws<inv::synthesis_limit>([&] { (void)original.synthesized(options); },
			"exceeding a synthesis budget must fail explicitly");
		require(!original.is_synthesized(), "failed synthesis must not modify the original");
		inv::values recovered;
		require(original.solve({true, true, true}, [&](const auto& value) { recovered = value; return true; }) == 1 &&
			recovered == inv::values{false, true, true}, "original solver broke after failed synthesis");
	}
	const auto synthesized = original.synthesized();
	std::ostringstream serialized;
	synthesized.save(serialized);
	const auto valid = serialized.str();
	const auto marker = valid.find("SYNTHESIZED\n");
	require(marker != std::string::npos, "saved synthesis is missing its backend marker");
	const auto reject = [&](const std::string& text)
	{
		require_throws([&] { std::istringstream stream(text); (void)inv::program::load(stream); },
			"damaged synthesized artifact was accepted");
	};
	for (size_t length = marker; length < valid.find("END_INVERSE") + 11; ++length) reject(valid.substr(0, length));
	for (size_t offset = marker + 12; offset < valid.find("CHECKSUM"); ++offset)
	{
		if (valid[offset] < '0' || valid[offset] > '9') continue;
		auto damaged = valid;
		damaged[offset] = valid[offset] == '0' ? '1' : '0';
		reject(damaged);
	}
	reject(valid + "trailing garbage");
	std::stringstream formatted;
	formatted << std::hex << std::showbase;
	synthesized.save(formatted);
	require(formatted.str() == valid, "saved synthesis must ignore caller stream numeric formatting");
	(void)inv::program::load(formatted);
}

// Scalar one-block reference, independently evaluated with native uint32_t
// arithmetic. Its digest order is checked against published MD5 test vectors.
inv::values reference_md5(const std::string& message)
{
	require(message.size() < 56, "one-block reference MD5 message is too long");
	constexpr std::array<unsigned, 64> shifts{
		7,12,17,22,7,12,17,22,7,12,17,22,7,12,17,22,
		5,9,14,20,5,9,14,20,5,9,14,20,5,9,14,20,
		4,11,16,23,4,11,16,23,4,11,16,23,4,11,16,23,
		6,10,15,21,6,10,15,21,6,10,15,21,6,10,15,21};
	constexpr std::array<std::uint32_t,64> constants{
		0xd76aa478,0xe8c7b756,0x242070db,0xc1bdceee,0xf57c0faf,0x4787c62a,0xa8304613,0xfd469501,
		0x698098d8,0x8b44f7af,0xffff5bb1,0x895cd7be,0x6b901122,0xfd987193,0xa679438e,0x49b40821,
		0xf61e2562,0xc040b340,0x265e5a51,0xe9b6c7aa,0xd62f105d,0x02441453,0xd8a1e681,0xe7d3fbc8,
		0x21e1cde6,0xc33707d6,0xf4d50d87,0x455a14ed,0xa9e3e905,0xfcefa3f8,0x676f02d9,0x8d2a4c8a,
		0xfffa3942,0x8771f681,0x6d9d6122,0xfde5380c,0xa4beea44,0x4bdecfa9,0xf6bb4b60,0xbebfbc70,
		0x289b7ec6,0xeaa127fa,0xd4ef3085,0x04881d05,0xd9d4d039,0xe6db99e5,0x1fa27cf8,0xc4ac5665,
		0xf4292244,0x432aff97,0xab9423a7,0xfc93a039,0x655b59c3,0x8f0ccc92,0xffeff47d,0x85845dd1,
		0x6fa87e4f,0xfe2ce6e0,0xa3014314,0x4e0811a1,0xf7537e82,0xbd3af235,0x2ad7d2bb,0xeb86d391};
	std::array<std::uint32_t,16> words{};
	for (size_t i = 0; i < message.size(); ++i) words[i / 4] |= std::uint32_t(static_cast<unsigned char>(message[i])) << (8 * (i % 4));
	words[message.size() / 4] |= 0x80U << (8 * (message.size() % 4));
	words[14] = static_cast<std::uint32_t>(message.size() * 8);
	std::array<std::uint32_t,4> state{0x67452301,0xefcdab89,0x98badcfe,0x10325476};
	auto a = state[0], b = state[1], c = state[2], d = state[3];
	for (unsigned round = 0; round < 64; ++round)
	{
		std::uint32_t f;
		unsigned word;
		if (round < 16) { f = (b & c) | (~b & d); word = round; }
		else if (round < 32) { f = (d & b) | (~d & c); word = (5 * round + 1) % 16; }
		else if (round < 48) { f = b ^ c ^ d; word = (3 * round + 5) % 16; }
		else { f = c ^ (b | ~d); word = 7 * round % 16; }
		const auto next_b = b + std::rotl(a + f + constants[round] + words[word], static_cast<int>(shifts[round]));
		a = d; d = c; c = b; b = next_b;
	}
	state[0] += a; state[1] += b; state[2] += c; state[3] += d;
	inv::values digest(128);
	for (size_t byte = 0; byte < 16; ++byte)
		for (size_t bit = 0; bit < 8; ++bit)
			digest[(15 - byte) * 8 + bit] = (state[byte / 4] >> (8 * (byte % 4) + bit)) & 1;
	return digest;
}

std::string digest_hex(const inv::values& digest)
{
	std::string result;
	for (size_t nibble = 32; nibble-- > 0;)
	{
		unsigned value = 0;
		for (unsigned bit = 0; bit < 4; ++bit) value |= static_cast<unsigned>(digest[nibble * 4 + bit]) << bit;
		result += "0123456789abcdef"[value];
	}
	return result;
}

inv::values message_bits(const std::string& message)
{
	inv::values result;
	for (const unsigned char byte : message)
	{
		const auto value = integer_bits(byte, 8);
		result.insert(result.end(), value.begin(), value.end());
	}
	return result;
}

void md5_synthesis_test()
{
	require(digest_hex(reference_md5("")) == "d41d8cd98f00b204e9800998ecf8427e", "scalar MD5 empty vector mismatch");
	require(digest_hex(reference_md5("abc")) == "900150983cd24fb0d6963f7d28e17f72", "scalar MD5 abc vector mismatch");
	const auto synthesized = []
	{
		std::vector<br::itu8> message{'m', 'd', '5'};
		message.emplace_back(br::unknown);
		const auto original = inv::program::compile(inv::bits_of(message), inv::bits_of(br::hash::md5(message)));
		require(original.unknown_count() == 8 && original.input_count() == 32, "MD5 prefix must be baked into the circuit");
		return original.synthesized();
	}();
	const auto loaded = roundtrip(synthesized);
	truth_table expected;
	for (unsigned byte = 0; byte < 256; ++byte)
	{
		std::string message = "md5";
		message.push_back(static_cast<char>(byte));
		expected[reference_md5(message)].insert(message_bits(message));
	}
	for (const auto& [target, inputs] : expected)
	{
		check_target(synthesized, target, inputs);
		check_target(loaded, target, inputs);
	}
	inv::values absent(128, false);
	require(!expected.contains(absent), "MD5 rejection test unexpectedly chose an existing digest");
	check_target(loaded, absent, {});
	for (size_t bit = 0; bit < 128; ++bit)
	{
		auto changed = expected.begin()->first;
		changed[bit] = !changed[bit];
		if (!expected.contains(changed)) check_target(loaded, changed, {});
	}
}
}

int main()
{
	try
	{
		shared_operand_and_reversible_tests();
		arithmetic_tests<4>();
		arithmetic_tests<8>();
		random_circuit_tests();
		restrictions_aliases_and_constants_tests();
		limits_and_artifact_tests();
		md5_synthesis_test();
		std::cout << "All synthesis tests passed\n";
	}
	catch (const std::exception& error)
	{
		std::cerr << "Synthesis test failure: " << error.what() << '\n';
		return 1;
	}
}
