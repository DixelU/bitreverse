#include <cstdint>
#include <iomanip>
#include <iostream>
#include <set>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

#include "crc32.h"
#include "inverse.h"
#include "md5.h"

namespace br = dixelu::bitreverse;
namespace inv = br::inversion;

namespace
{

void require(bool condition, const char* message)
{
	if (!condition)
		throw std::runtime_error(message);
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
	for (size_t bit = 0; bit < width; ++bit)
		result[bit] = ((value >> bit) & 1) != 0;
	return result;
}

std::uint64_t integer_value(const inv::values& value)
{
	require(value.size() <= 64, "integer helper width exceeded");
	std::uint64_t result = 0;
	for (size_t bit = 0; bit < value.size(); ++bit)
		if (value[bit])
			result |= std::uint64_t{1} << bit;
	return result;
}

inv::values constants(const inv::bits& value)
{
	inv::values result;
	for (const auto& bit : value)
	{
		require(bit.bit_state->operation == '=', "expected a concrete forward result");
		result.push_back(bit.bit_state->state != 0);
	}
	return result;
}

std::vector<br::itu8> tracked(const std::string& text)
{
	std::vector<br::itu8> result;
	for (const unsigned char byte : text)
		result.emplace_back(byte);
	return result;
}

std::string bytes(const inv::values& value)
{
	require(value.size() % 8 == 0, "expected whole-byte input");
	std::string result;
	for (size_t offset = 0; offset < value.size(); offset += 8)
	{
		unsigned byte = 0;
		for (size_t bit = 0; bit < 8; ++bit)
			byte |= static_cast<unsigned>(value[offset + bit]) << bit;
		result.push_back(static_cast<char>(byte));
	}
	return result;
}

std::uint32_t reference_crc32(const std::string& message)
{
	std::uint32_t value = 0xffffffffU;
	for (const unsigned char byte : message)
	{
		value ^= byte;
		for (unsigned bit = 0; bit < 8; ++bit)
			value = (value >> 1) ^ ((value & 1) ? 0xedb88320U : 0);
	}
	return ~value;
}

using models = std::set<inv::values>;

models collect(const inv::program& program, const inv::values& target,
	const br::solver_options& options = {}, br::solver_statistics* statistics = nullptr)
{
	models result;
	const auto count = program.solve(target, [&](const inv::values& value)
	{
		require(result.insert(value).second, "inverse emitted a duplicate input");
		return true;
	}, 0, options, statistics);
	require(count == result.size(), "inverse returned the wrong solution count");
	return result;
}

inv::program roundtrip(const inv::program& program)
{
	std::stringstream artifact;
	program.save(artifact);
	return inv::program::load(artifact);
}

void bit_order_and_affine_tests()
{
	require(constants(inv::bits_of(br::itu8{0x12})) == integer_bits(0x12, 8),
		"bits_of must expose least-significant bits first");
	require(bytes(constants(inv::bits_of(std::vector<br::itu8>{0x12, 0xa5}))) ==
		std::string("\x12\xa5", 2), "vector bits_of must preserve byte order");
	const auto program = []
	{
		br::int_tracker<4> x{br::unknown};
		const auto inputs = inv::bits_of(x);
		const auto a = inputs[0] ^ inputs[1] ^ !inputs[2];
		const auto b = inputs[1] ^ inputs[3];
		return inv::program::compile(inputs, {a, b, a ^ b});
	}();
	require(program.is_affine() && program.free_bit_count() == 2,
		"affine compilation must identify rank and free parameters");
	const auto loaded = roundtrip(program);
	for (unsigned target = 0; target < 8; ++target)
	{
		models expected;
		for (unsigned input = 0; input < 16; ++input)
		{
			const auto x = integer_bits(input, 4);
			const bool a = x[0] ^ x[1] ^ !x[2];
			const bool b = x[1] ^ x[3];
			if (inv::values{a, b, a != b} == integer_bits(target, 3))
				expected.insert(x);
		}
		br::solver_statistics statistics;
		require(collect(loaded, integer_bits(target, 3), {}, &statistics) == expected,
			"saved affine inverse disagrees with exhaustive forward evaluation");
		require(statistics.decisions == 0 && statistics.affine_enabled,
			"direct affine inverse must not perform search decisions");
		models evaluated;
		for (unsigned seed = 0; seed < 4; ++seed)
			if (const auto value = loaded.evaluate(integer_bits(target, 3), integer_bits(seed, 2)))
				evaluated.insert(*value);
		require(evaluated == expected, "free affine parameters must cover exactly all inverse branches");
	}
	require_throws<std::invalid_argument>([&] { loaded.evaluate({false}, {false, false}); },
		"evaluate must reject a target with the wrong width");
	require_throws<std::invalid_argument>([&] { loaded.evaluate({false, false, false}, {}); },
		"evaluate must reject free parameters with the wrong width");
	require_throws<std::invalid_argument>([&] { loaded.solve({false}, [](const auto&) { return true; }); },
		"solve must reject a target with the wrong width");
	require_throws<std::invalid_argument>([&] { loaded.solve({false, false, false}, {}); },
		"solve must reject an empty callback");
	const auto callback = [](const inv::values&) { return true; };
	require(loaded.solve({false, false, false}, callback, 2) == 2,
		"direct affine solution limit must be honored");
	require(loaded.solve({false, false, false}, [](const auto&) { return false; }, 0) == 1,
		"direct affine callback must stop enumeration");
}

void nonlinear_and_requirement_tests()
{
	std::stringstream artifact;
	{
		br::int_tracker<3> x{br::unknown};
		const auto inputs = inv::bits_of(x);
		const auto conjunction = inputs[0] & inputs[1];
		const auto program = inv::program::compile(inputs,
			{conjunction, inputs[0] | inputs[1], conjunction});
		require(!program.is_affine(), "AND/OR relation must use nonlinear search");
		program.save(artifact);
	}
	const auto loaded = inv::program::load(artifact);
	require_throws<std::logic_error>([&] { loaded.evaluate({false, false, false}, {}); },
		"nonlinear evaluate must explicitly require search");
	for (const bool learning : {false, true})
	{
		br::solver_options options;
		options.conflict_learning = learning;
		options.affine_reasoning = !learning;
		for (unsigned query = 0; query < 16; ++query)
		{
			const unsigned target = query % 8;
			models expected;
			for (unsigned input = 0; input < 8; ++input)
			{
				const auto x = integer_bits(input, 3);
				if (inv::values{x[0] && x[1], x[0] || x[1], x[0] && x[1]} == integer_bits(target, 3))
					expected.insert(x);
			}
			require(collect(loaded, integer_bits(target, 3), options) == expected,
				"nonlinear repeated query lost output correlations or retained stale state");
		}
		require(loaded.solve({false, true, false}, [](const auto&) { return true; }, 2, options) == 2,
			"nonlinear solution limit must be honored");
		require(loaded.solve({false, true, false}, [](const auto&) { return false; }, 0, options) == 1,
			"nonlinear callback must stop enumeration");
	}
	br::bit_tracker x, y;
	x = br::unknown; y = br::unknown;
	const auto restricted = roundtrip(inv::program::compile({x, y}, {x ^ y}, {x | y}));
	for (const bool learning : {false, true})
	{
		br::solver_options options;
		options.conflict_learning = learning; options.affine_reasoning = !learning;
		require(collect(restricted, {false}, options) == models{{true, true}},
			"saved nonlinear requirement must constrain all queries");
		require(collect(restricted, {true}, options) == models{{false, true}, {true, false}},
			"saved requirement must preserve every matching input");
	}
	const auto affine_restricted = roundtrip(inv::program::compile({x, y}, {x ^ y}, {x}));
	require(affine_restricted.is_affine() && affine_restricted.free_bit_count() == 0,
		"affine requirements must participate in factorization");
	require(collect(affine_restricted, {true}) == models{{true, false}},
		"direct affine evaluation must enforce requirements");
}

void aliases_unused_and_constant_tests()
{
	br::bit_tracker x, unused;
	x = br::unknown; unused = br::unknown;
	const auto aliases = roundtrip(inv::program::compile({x, x, unused, br::bit_tracker(true)}, {x}));
	require(aliases.input_count() == 4 && aliases.unknown_count() == 2 && aliases.free_bit_count() == 1,
		"duplicate input aliases must share one unknown");
	require(collect(aliases, {false}) == models{{false, false, false, true}, {false, false, true, true}},
		"input aliases, unused input, and constants must survive saving");
	const auto canceled = roundtrip(inv::program::compile({x, unused}, {x ^ x}));
	require(canceled.unknown_count() == 2 && canceled.free_bit_count() == 2,
		"optimized-out unknowns must remain inverse parameters");
	require(collect(canceled, {false}).size() == 4 && collect(canceled, {true}).empty(),
		"constant output must preserve all inputs and reject impossible targets");
	const auto constant = roundtrip(inv::program::compile({}, {br::bit_tracker(true), br::bit_tracker(false)}));
	require(collect(constant, {true, false}) == models{inv::values{}},
		"constant relation must return one empty input");
	require(collect(constant, {false, false}).empty(), "constant relation must reject impossible output");
	const auto empty = roundtrip(inv::program::compile({}, {}));
	require(collect(empty, {}) == models{inv::values{}}, "empty relation must have one empty model");
	const auto no_outputs = roundtrip(inv::program::compile({x, unused}, {}));
	require(collect(no_outputs, {}).size() == 4, "no output constraints must enumerate all declared inputs");
	const auto impossible = roundtrip(inv::program::compile({x}, {x}, {br::bit_tracker(false)}));
	require(collect(impossible, {false}).empty() && collect(impossible, {true}).empty(),
		"false baked requirement must make every target impossible");
	require_throws<std::invalid_argument>([&] { inv::program::compile({x}, {x ^ unused}); },
		"undeclared unknowns must be rejected");
	require_throws<std::invalid_argument>([&] { inv::program::compile({x ^ unused}, {x}); },
		"non-leaf input ports must be rejected");
	br::bit_tracker null_bit;
	null_bit.bit_state = {};
	require_throws<std::invalid_argument>([&] { inv::program::compile({null_bit}, {}); },
		"null input bits must be rejected");
}

void crc_tests()
{
	require(reference_crc32("123456789") == 0xcbf43926U, "reference CRC known vector mismatch");
	require(integer_value(constants(inv::bits_of(br::hash::crc32(tracked("123456789"))))) == 0xcbf43926U,
		"tracked CRC must match the standard check vector");
	for (const size_t byte_count : {size_t{4}, size_t{5}})
	{
		const auto program = [byte_count]
		{
			std::vector<br::itu8> message;
			for (size_t byte = 0; byte < byte_count; ++byte)
				message.emplace_back(br::unknown);
			return inv::program::compile(inv::bits_of(message), inv::bits_of(br::hash::crc32(message)));
		}();
		const auto loaded = roundtrip(program);
		require(loaded.is_affine() && loaded.free_bit_count() == (byte_count - 4) * 8,
			"CRC inverse must have the expected direct affine family dimension");
		for (const std::uint32_t target : {0U, 1U, 0xffffffffU, 0x12345678U, 0xcbf43926U})
		{
			models seen;
			const unsigned seeds = byte_count == 4 ? 1 : 256;
			for (unsigned seed = 0; seed < seeds; ++seed)
			{
				const auto value = loaded.evaluate(integer_bits(target, 32), integer_bits(seed, loaded.free_bit_count()));
				require(value.has_value() && reference_crc32(bytes(*value)) == target,
					"CRC inverse failed independent forward validation");
				require(seen.insert(*value).second, "CRC free seeds must produce distinct messages");
			}
			br::solver_statistics statistics;
			statistics.decisions = 123;
			require(loaded.solve(integer_bits(target, 32), [](const auto&) { return true; }, 1, {}, &statistics) == 1 &&
				statistics.decisions == 0 && statistics.affine_enabled,
				"factorized CRC queries must make zero solver decisions");
		}
	}
	const auto prefix = []
	{
		auto message = tracked("fixed:");
		for (unsigned byte = 0; byte < 4; ++byte) message.emplace_back(br::unknown);
		return roundtrip(inv::program::compile(inv::bits_of(message), inv::bits_of(br::hash::crc32(message))));
	}();
	const auto recovered = prefix.evaluate(integer_bits(0xabcdef01, 32), {});
	require(recovered && bytes(*recovered).starts_with("fixed:") && reference_crc32(bytes(*recovered)) == 0xabcdef01,
		"CRC specialization must bake and preserve known message bytes");
}

void md5_persistence_test()
{
	std::stringstream artifact;
	{
		auto message = tracked("md5");
		message.emplace_back(br::unknown);
		const auto program = inv::program::compile(inv::bits_of(message), inv::bits_of(br::hash::md5(message)));
		require(!program.is_affine() && program.unknown_count() == 8 && program.input_count() == 32,
			"MD5 specialization must bake the prefix and retain one unknown byte");
		program.save(artifact);
	}
	const auto loaded = inv::program::load(artifact);
	for (const std::string& message : {std::string("md5!"), std::string("md5?")})
	{
		const auto target = constants(inv::bits_of(br::hash::md5(tracked(message))));
		std::string recovered;
		const auto count = loaded.solve(target, [&](const inv::values& value)
		{
			recovered = bytes(value);
			return true;
		});
		require(count == 1 && recovered == message,
			"saved MD5 inverse must solve new targets after the original circuit is destroyed");
	}
}

void compiled_solver_tests()
{
	namespace cr = br::collision_resolution;
	br::bit_tracker x, y, unused;
	x = br::unknown; y = br::unknown; unused = br::unknown;
	const auto output = x & y;
	const auto circuit = std::make_shared<const cr::solver_core::compiled_circuit>(
		std::vector<dixelu::counted_ptr<br::details::bitstate>>{output.bit_state, unused.bit_state});
	const auto id = [&](const br::bit_tracker& bit) { return circuit->node_ids.at(bit.bit_state.get()); };
	const auto keep = [](const cr::crs_state& solution)
	{
		require(solution.assignments.size() == 3, "compiled solver must include unused unknown roots");
		return true;
	};
	for (const bool learning : {false, true})
	{
		br::solver_options options;
		options.conflict_learning = learning; options.affine_reasoning = false;
		require(cr::solve_compiled_stream(circuit, {}, keep, options) == 8,
			"compiled query must not implicitly assert its first root");
		require(cr::solve_compiled_stream(circuit, {{id(output), false}}, keep, options) == 6,
			"compiled false-output enumeration mismatch");
		require(cr::solve_compiled_stream(circuit, {{id(output), true}}, keep, options) == 2,
			"compiled queries must have fresh state");
		require(cr::solve_compiled_stream(circuit, {{id(x), false}, {id(x), true}}, keep, options) == 0,
			"contradictory runtime bindings must be unsatisfiable");
		require(cr::solve_compiled_stream(circuit, {{id(output), true}, {id(x), false}}, keep, options) == 0,
			"runtime bindings must jointly constrain internal nodes and inputs");
		require_throws<std::out_of_range>([&]
		{
			cr::solve_compiled_stream(circuit, {{cr::solver_core::no_node, false}}, keep, options);
		}, "compiled query must validate binding IDs");
	}
	require_throws<std::invalid_argument>([&] { cr::solve_compiled_stream(nullptr, {}, keep); },
		"compiled query must reject a null circuit");
}

void malformed_artifact_tests()
{
	const auto reject = [](const std::string& text)
	{
		require_throws([&]
		{
			std::istringstream stream(text);
			(void)inv::program::load(stream);
		}, "malformed inverse artifact was accepted");
	};
	const std::string valid = []
	{
		br::bit_tracker lhs, rhs;
		lhs = br::unknown; rhs = br::unknown;
		std::ostringstream stream;
		inv::program::compile({lhs, rhs}, {lhs & rhs}).save(stream);
		return stream.str();
	}();
	{
		std::istringstream stream(valid);
		require(collect(inv::program::load(stream), {true}) == models{{true, true}},
			"valid search artifact must load");
	}
	reject("");
	reject("BITREVERSE_INVERSE 2\n");
	reject("BITREVERSE_INVERSE 1\n-1 0 0 0\n");
	reject("BITREVERSE_INVERSE 1\n1000001 0 0 0\n");
	reject("BITREVERSE_INVERSE 1\n1 1 1 0\n? 0 1 1\n0\n0\nSEARCH\nEND_INVERSE\n");
	reject("BITREVERSE_INVERSE 1\n1 1 1 0\n* 1 1 1\n0\n0\nSEARCH\nEND_INVERSE\n");
	reject("BITREVERSE_INVERSE 1\n1 1 1 0\n* 0 0 1\n0\n0\nSEARCH\nEND_INVERSE\n");
	reject("BITREVERSE_INVERSE 1\n1 0 1 0\n! 0 1 1\n0\nSEARCH\nEND_INVERSE\n");
	reject("BITREVERSE_INVERSE 1\n1 0 1 0\n! 0 0 1\n0\nSEARCH\nEND_INVERSE\n");
	reject("BITREVERSE_INVERSE 1\n2 0 1 0\n! 0 1 2\n! 0 0 2\n0\nSEARCH\nEND_INVERSE\n");
	reject("BITREVERSE_INVERSE 1\n1 1 1 0\n* 0 2 1\n0\n0\nSEARCH\nEND_INVERSE\n");
	reject("BITREVERSE_INVERSE 1\n1 1 1 0\n* 0 1 1\n0\n1\nSEARCH\nEND_INVERSE\n");
	reject("BITREVERSE_INVERSE 1\n2 1 1 0\n* 0 2 2\n= 1 2 2\n0\n0\nSEARCH\nEND_INVERSE\n");
	reject("BITREVERSE_INVERSE 1\n1 0 1 0\n* 0 1 1\n0\nSEARCH\nEND_INVERSE\n");
	reject("BITREVERSE_INVERSE 1\n2 1 1 0\n! 0 1 2\n* 0 2 2\n0\n0\nSEARCH\nEND_INVERSE\n");
	reject("BITREVERSE_INVERSE 1\n0 1 0 0\n0\nSEARCH\nEND_INVERSE\n");
	reject("BITREVERSE_INVERSE 1\n1 1 1 0\n* 0 1 1\n0\n0\nBOGUS\nEND_INVERSE\n");
	reject(valid + "trailing garbage");
	for (size_t length = 0; length < valid.find("END_INVERSE") + 11; ++length)
		reject(valid.substr(0, length));
	br::bit_tracker x; x = br::unknown;
	std::ostringstream serialized;
	inv::program::compile({x}, {x}).save(serialized);
	const auto affine = serialized.str();
	const auto marker = affine.find("BITREVERSE_AFFINE 1");
	require(marker != std::string::npos, "affine artifact must persist its factorization");
	auto wrong_version = affine;
	wrong_version.replace(marker, std::string("BITREVERSE_AFFINE 1").size(), "BITREVERSE_AFFINE 2");
	reject(wrong_version);
	reject(affine.substr(0, affine.find("END_AFFINE")));
}

void artifact_format_and_checksum_tests()
{
	br::int_tracker<12> word{br::unknown};
	const auto inputs = inv::bits_of(word);
	const auto program = inv::program::compile(inputs, inputs);
	std::ostringstream ordinary;
	program.save(ordinary);
	std::stringstream formatted;
	formatted << std::hex << std::showbase;
	program.save(formatted);
	require(formatted.str() == ordinary.str(),
		"artifact serialization must ignore caller hexadecimal and showbase flags");
	const auto loaded = inv::program::load(formatted);
	const auto target = integer_bits(0xa53, 12);
	require(loaded.evaluate(target, {}) == std::optional<inv::values>{target} &&
		collect(loaded, target) == models{target},
		"artifact must reload and preserve identity under hexadecimal stream flags");

	br::bit_tracker x; x = br::unknown;
	std::ostringstream serialized;
	inv::program::compile({x}, {x}).save(serialized);
	const auto original = serialized.str();
	require(original.find("CHECKSUM ") != std::string::npos,
		"saved artifact must include a payload checksum");
	std::istringstream intact(original);
	const auto identity = inv::program::load(intact);
	require(collect(identity, {false}) == models{{false}} &&
		collect(identity, {true}) == models{{true}},
		"checksummed identity artifact must support independent target queries");

	auto corrupted = original;
	size_t constant = corrupted.find("BITREVERSE_AFFINE 1");
	require(constant != std::string::npos, "identity must contain an affine payload");
	// The affine body starts with dimensions, pivots, then constants.
	for (unsigned line = 0; line < 3; ++line)
	{
		constant = corrupted.find('\n', constant);
		require(constant != std::string::npos, "identity affine payload is truncated");
		++constant;
	}
	constant = corrupted.find_first_not_of(" \t\r\n", constant);
	require(constant != std::string::npos && corrupted[constant] == '0',
		"identity affine constant must initially be zero");
	corrupted[constant] = '1';
	require_throws([&]
	{
		std::istringstream stream(corrupted);
		(void)inv::program::load(stream);
	}, "syntactically valid corruption of an affine constant must fail its checksum");
}

}

int main()
{
	try
	{
		bit_order_and_affine_tests();
		nonlinear_and_requirement_tests();
		aliases_unused_and_constant_tests();
		compiled_solver_tests();
		malformed_artifact_tests();
		artifact_format_and_checksum_tests();
		crc_tests();
		md5_persistence_test();
		std::cout << "All inverse tests passed\n";
	}
	catch (const std::exception& error)
	{
		std::cerr << "Inverse test failure: " << error.what() << '\n';
		return 1;
	}
}
