// inversion::program with its default backends, search or a direct affine
// inverse: compilation, queries and persistence.

#include <cstddef>
#include <cstdint>
#include <initializer_list>
#include <memory>
#include <optional>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

#include "crc32.h"
#include "inverse.h"
#include "inverse_support.h"
#include "md5.h"
#include "reference_hashes.h"
#include "test_support.h"

namespace br = dixelu::bitreverse;
namespace inv = br::inversion;
using test::models;
using test::require;
using test::to_bits;

namespace
{

void bit_order_and_affine()
{
	require(test::constant_bits(inv::bits_of(br::itu8{0x12})) == to_bits(0x12, 8),
		"bits_of must expose least-significant bits first");
	require(test::text_of(test::constant_bits(inv::bits_of(std::vector<br::itu8>{0x12, 0xa5}))) == "\x12\xa5",
		"vector bits_of must preserve byte order");
	const br::int_tracker<4> x = br::unknown;
	const auto inputs = inv::bits_of(x);
	const auto a = inputs[0] ^ inputs[1] ^ !inputs[2];
	const auto b = inputs[1] ^ inputs[3];
	const auto program = inv::program::compile(inputs, {a, b, a ^ b});
	require(program.is_affine() && program.free_bit_count() == 2,
		"affine compilation must identify rank and free parameters");
	const auto loaded = test::roundtrip(program);
	for (std::uint64_t target = 0; target < 8; ++target)
	{
		models expected;
		for (std::uint64_t input = 0; input < 16; ++input)
		{
			const auto v = to_bits(input, 4);
			const bool a_value = v[0] ^ v[1] ^ !v[2], b_value = v[1] ^ v[3];
			if (inv::values{a_value, b_value, a_value != b_value} == to_bits(target, 3))
				expected.insert(v);
		}
		br::solver_statistics statistics;
		require(test::collect(loaded, to_bits(target, 3), {}, &statistics) == expected,
			"saved affine inverse disagrees with exhaustive forward evaluation");
		require(statistics.decisions == 0 && statistics.affine_enabled,
			"direct affine inverse must not perform search decisions");
		models evaluated;
		for (std::uint64_t seed = 0; seed < 4; ++seed)
			if (const auto value = loaded.evaluate(to_bits(target, 3), to_bits(seed, 2)))
				evaluated.insert(*value);
		require(evaluated == expected, "free affine parameters must cover exactly all inverse branches");
	}
	test::check_rejects_malformed_queries(loaded);
	test::check_solve_limits(loaded, {false, false, false});
}

// The default backend against the fixtures every other backend must match.
void small_relations()
{
	for (const auto& [program, expected] : test::small_relations())
	{
		const auto loaded = test::roundtrip(program);
		test::check_rejects_malformed_queries(loaded);
		// Two passes over the targets: no query may depend on the one before.
		for (const auto& options : {test::dpll_affine, test::cdcl})
			for (size_t pass = 0; pass < 2; ++pass)
				for (std::uint64_t word = 0; word < (std::uint64_t{1} << loaded.output_count()); ++word)
				{
					const auto target = to_bits(word, loaded.output_count());
					require(test::collect(loaded, target, options) == test::preimages(expected, target),
						"inverse disagrees with the native truth table");
				}
	}
}

void nonlinear_and_requirements()
{
	// The circuit and its expressions are gone once the artifact is loaded.
	const auto loaded = test::roundtrip([]
	{
		const auto x = test::unknown_bits(3);
		const auto conjunction = x[0] & x[1];
		const auto program = inv::program::compile(x, {conjunction, x[0] | x[1], conjunction});
		require(!program.is_affine(), "AND/OR relation must use nonlinear search");
		return program;
	}());
	for (const auto& options : {test::dpll_affine, test::cdcl})
		test::check_solve_limits(loaded, {false, true, false}, options);

	const br::bit_tracker x = br::unknown, y = br::unknown;
	const auto restricted = test::roundtrip(inv::program::compile({x, y}, {x ^ y}, {x | y}));
	for (const auto& options : {test::dpll_affine, test::cdcl})
	{
		require(test::collect(restricted, {false}, options) == models{{true, true}},
			"saved nonlinear requirement must constrain all queries");
		require(test::collect(restricted, {true}, options) == models{{false, true}, {true, false}},
			"saved requirement must preserve every matching input");
	}
	const auto affine_restricted = test::roundtrip(inv::program::compile({x, y}, {x ^ y}, {x}));
	require(affine_restricted.is_affine() && affine_restricted.free_bit_count() == 0,
		"affine requirements must participate in factorization");
	require(test::collect(affine_restricted, {true}) == models{{true, false}},
		"direct affine evaluation must enforce requirements");
}

void aliases_unused_and_constants()
{
	const br::bit_tracker x = br::unknown, unused = br::unknown;
	const auto aliases = test::roundtrip(inv::program::compile({x, x, unused, br::bit_tracker(true)}, {x}));
	require(aliases.input_count() == 4 && aliases.unknown_count() == 2 && aliases.free_bit_count() == 1,
		"duplicate input aliases must share one unknown");
	require(test::collect(aliases, {false}) == models{{false, false, false, true}, {false, false, true, true}},
		"input aliases, unused input, and constants must survive saving");
	const auto canceled = test::roundtrip(inv::program::compile({x, unused}, {x ^ x}));
	require(canceled.unknown_count() == 2 && canceled.free_bit_count() == 2,
		"optimized-out unknowns must remain inverse parameters");
	require(test::collect(canceled, {false}).size() == 4 && test::collect(canceled, {true}).empty(),
		"constant output must preserve all inputs and reject impossible targets");
	const auto no_outputs = test::roundtrip(inv::program::compile({x, unused}, {}));
	require(test::collect(no_outputs, {}).size() == 4, "no output constraints must enumerate all declared inputs");
	const auto impossible = test::roundtrip(inv::program::compile({x}, {x}, {br::bit_tracker(false)}));
	require(test::collect(impossible, {false}).empty() && test::collect(impossible, {true}).empty(),
		"false baked requirement must make every target impossible");
	test::require_throws<std::invalid_argument>([&] { inv::program::compile({x}, {x ^ unused}); },
		"undeclared unknowns must be rejected");
	test::require_throws<std::invalid_argument>([&] { inv::program::compile({x ^ unused}, {x}); },
		"non-leaf input ports must be rejected");
	br::bit_tracker null_bit;
	null_bit.bit_state = {};
	test::require_throws<std::invalid_argument>([&] { inv::program::compile({null_bit}, {}); },
		"null input bits must be rejected");
}

void compiled_solver()
{
	namespace cr = br::collision_resolution;
	const br::bit_tracker x = br::unknown, y = br::unknown, unused = br::unknown;
	const auto output = x & y;
	const auto circuit = std::make_shared<const cr::solver_core::compiled_circuit>(
		std::vector<dixelu::counted_ptr<br::details::bitstate>>{output.bit_state, unused.bit_state});
	const auto id = [&](const br::bit_tracker& bit) { return circuit->node_ids.at(bit.bit_state.get()); };
	const auto keep = [](const cr::solution& solution)
	{
		require(solution.assignments.size() == 3, "compiled solver must include unused unknown roots");
		return true;
	};
	for (const auto& options : {test::dpll, test::cdcl})
	{
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
		test::require_throws<std::out_of_range>([&]
		{
			cr::solve_compiled_stream(circuit, {{cr::solver_core::no_node, false}}, keep, options);
		}, "compiled query must validate binding IDs");
	}
	test::require_throws<std::invalid_argument>([&] { cr::solve_compiled_stream(nullptr, {}, keep); },
		"compiled query must reject a null circuit");
}

void artifacts()
{
	const br::bit_tracker x = br::unknown, y = br::unknown;
	const auto search = inv::program::compile({x, y}, {x & y});
	require(test::collect(test::roundtrip(search), {true}) == models{{true, true}}, "valid search artifact must load");
	test::check_artifact(search);

	// Twelve inputs put multi-digit numbers into the artifact, where std::hex would show.
	const auto word = test::unknown_bits(12);
	const auto identity = inv::program::compile(word, word);
	require(identity.is_affine(), "the identity must compile to a direct affine inverse");
	test::check_artifact(identity);
	const auto loaded = test::roundtrip(identity);
	for (const std::uint64_t value : {0x000, 0xa53, 0xfff})
	{
		const auto target = to_bits(value, 12);
		require(loaded.evaluate(target, {}) == std::optional<inv::values>{target} &&
			test::collect(loaded, target) == models{target}, "identity artifact must answer independent target queries");
	}

	const auto reject = [](const std::string& text)
	{
		test::require_throws([&] { (void)test::load(text); }, "malformed inverse artifact was accepted: " + text);
	};
	for (const char* text : {
		"BITREVERSE_INVERSE 2\n",
		"BITREVERSE_INVERSE 1\n-1 0 0 0\n",
		"BITREVERSE_INVERSE 1\n1000001 0 0 0\n",
		"BITREVERSE_INVERSE 1\n1 1 1 0\n? 0 1 1\n0\n0\nSEARCH\nEND_INVERSE\n",
		"BITREVERSE_INVERSE 1\n1 1 1 0\n* 1 1 1\n0\n0\nSEARCH\nEND_INVERSE\n",
		"BITREVERSE_INVERSE 1\n1 1 1 0\n* 0 0 1\n0\n0\nSEARCH\nEND_INVERSE\n",
		"BITREVERSE_INVERSE 1\n1 0 1 0\n! 0 1 1\n0\nSEARCH\nEND_INVERSE\n",
		"BITREVERSE_INVERSE 1\n1 0 1 0\n! 0 0 1\n0\nSEARCH\nEND_INVERSE\n",
		"BITREVERSE_INVERSE 1\n2 0 1 0\n! 0 1 2\n! 0 0 2\n0\nSEARCH\nEND_INVERSE\n",
		"BITREVERSE_INVERSE 1\n1 1 1 0\n* 0 2 1\n0\n0\nSEARCH\nEND_INVERSE\n",
		"BITREVERSE_INVERSE 1\n1 1 1 0\n* 0 1 1\n0\n1\nSEARCH\nEND_INVERSE\n",
		"BITREVERSE_INVERSE 1\n2 1 1 0\n* 0 2 2\n= 1 2 2\n0\n0\nSEARCH\nEND_INVERSE\n",
		"BITREVERSE_INVERSE 1\n1 0 1 0\n* 0 1 1\n0\nSEARCH\nEND_INVERSE\n",
		"BITREVERSE_INVERSE 1\n2 1 1 0\n! 0 1 2\n* 0 2 2\n0\n0\nSEARCH\nEND_INVERSE\n",
		"BITREVERSE_INVERSE 1\n0 1 0 0\n0\nSEARCH\nEND_INVERSE\n",
		"BITREVERSE_INVERSE 1\n1 1 1 0\n* 0 1 1\n0\n0\nBOGUS\nEND_INVERSE\n"})
		reject(text);
	auto wrong_version = test::save(identity);
	const auto marker = wrong_version.find("BITREVERSE_AFFINE 1");
	require(marker != std::string::npos, "affine artifact must persist its factorization");
	wrong_version.replace(marker, std::string("BITREVERSE_AFFINE 1").size(), "BITREVERSE_AFFINE 2");
	reject(wrong_version);
}

void crc32_inverse()
{
	for (const size_t byte_count : {4, 5})
	{
		const auto loaded = test::roundtrip([byte_count]
		{
			const auto message = test::unknown_bytes(byte_count);
			return inv::program::compile(inv::bits_of(message), inv::bits_of(br::hash::crc32(message)));
		}());
		require(loaded.is_affine() && loaded.free_bit_count() == (byte_count - 4) * 8,
			"CRC inverse must have the expected direct affine family dimension");
		for (const std::uint32_t target : {0U, 1U, 0xffffffffU, 0x12345678U, 0xcbf43926U})
		{
			models seen;
			for (std::uint64_t seed = 0; seed < (std::uint64_t{1} << loaded.free_bit_count()); ++seed)
			{
				const auto value = loaded.evaluate(to_bits(target, 32), to_bits(seed, loaded.free_bit_count()));
				require(value.has_value() && reference::crc32(test::text_of(*value)) == target,
					"CRC inverse failed independent forward validation");
				require(seen.insert(*value).second, "CRC free seeds must produce distinct messages");
			}
			br::solver_statistics statistics{.decisions = 123};
			require(loaded.solve(to_bits(target, 32), [](const auto&) { return true; }, 1, {}, &statistics) == 1 &&
				statistics.decisions == 0 && statistics.affine_enabled,
				"factorized CRC queries must make zero solver decisions");
		}
	}
	const auto prefix = test::roundtrip([]
	{
		auto message = test::tracked("fixed:");
		for (const auto& byte : test::unknown_bytes(4))
			message.push_back(byte);
		return inv::program::compile(inv::bits_of(message), inv::bits_of(br::hash::crc32(message)));
	}());
	const auto recovered = prefix.evaluate(to_bits(0xabcdef01, 32), {});
	require(recovered && test::text_of(*recovered).starts_with("fixed:") &&
		reference::crc32(test::text_of(*recovered)) == 0xabcdef01,
		"CRC specialization must bake and preserve known message bytes");
}

void md5_persistence()
{
	const auto loaded = test::roundtrip([]
	{
		const auto program = test::md5_relation().program;
		require(!program.is_affine() && program.unknown_count() == 8 && program.input_count() == 32,
			"MD5 specialization must bake the prefix and retain one unknown byte");
		return program;
	}());
	for (const std::string message : {"md5!", "md5?"})
	{
		const auto target = test::constant_bits(inv::bits_of(br::hash::md5(test::tracked(message))));
		std::string recovered;
		const auto count = loaded.solve(target, [&](const inv::values& value)
		{
			recovered = test::text_of(value);
			return true;
		});
		require(count == 1 && recovered == message,
			"saved MD5 inverse must solve new targets after the original circuit is destroyed");
	}
}

} // namespace

int main()
{
	return test::run("inverse", {
		{"bit_order_and_affine", bit_order_and_affine},
		{"small_relations", small_relations},
		{"nonlinear_and_requirements", nonlinear_and_requirements},
		{"aliases_unused_and_constants", aliases_unused_and_constants},
		{"compiled_solver", compiled_solver},
		{"artifacts", artifacts},
		{"crc32_inverse", crc32_inverse},
		{"md5_persistence", md5_persistence},
	});
}
