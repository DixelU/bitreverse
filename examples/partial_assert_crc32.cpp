#include "partial_assert_demo_support.h"

#include <array>
#include <string>

namespace br = dixelu::bitreverse;
namespace demo = partial_assert_demo;
using u32 = br::int_tracker<32>;

namespace
{
constexpr std::uint32_t checkpoint_mask = 0xffff0000U;
constexpr size_t prefix_bytes = 2;

// Independent native reference. The checkpoint is the internal state before
// the final CRC complement, after exactly the first two message bytes.
std::uint32_t native_state(const std::vector<std::uint8_t>& message, size_t count)
{
	std::uint32_t crc = 0xffffffffU;
	for (size_t index = 0; index < count; ++index)
	{
		crc ^= message[index];
		for (unsigned bit = 0; bit < 8; ++bit)
			crc = (crc >> 1) ^ ((crc & 1U) ? 0xedb88320U : 0U);
	}
	return crc;
}

// Deliberately separate from crc32.h: the assertion is inside the byte loop,
// and eager mode simplifies its live state before processing the suffix.
u32 instrumented_crc32(const std::vector<br::itu8>& message,
	br::partial_assert_context& context, bool eager, std::uint32_t observed,
	br::bit_tracker& checkpoint, size_t& known_after_checkpoint)
{
	u32 crc = 0xffffffffU;
	for (size_t index = 0; index < message.size(); ++index)
	{
		crc ^= u32(message[index]);
		for (unsigned bit = 0; bit < 8; ++bit)
		{
			const auto mask = -(crc & u32(1));
			crc = (crc >> 1) ^ (u32(0xedb88320U) & mask);
		}
		if (index + 1 == prefix_bytes)
		{
			checkpoint = u32::are_equal(crc & u32(checkpoint_mask), u32(observed));
			if (eager)
			{
				context.partial_assert(checkpoint);
				known_after_checkpoint = context.known_variable_count();
				crc = context.simplify(crc);
			}
		}
	}
	return ~crc;
}

demo::result run(bool eager, const std::vector<std::uint8_t>& known,
	std::uint32_t observed, std::uint32_t expected_digest)
{
	demo::result result;
	const auto started = demo::clock::now();
	br::partial_assert_context context;
	std::vector<br::itu8> message;
	for (size_t index = 0; index < known.size(); ++index)
		message.push_back(index < prefix_bytes ? br::itu8(br::unknown) : br::itu8(known[index]));
	std::vector<br::bit_tracker> inputs;
	for (size_t index = 0; index < prefix_bytes; ++index) demo::append_bits(inputs, message[index]);
	br::bit_tracker checkpoint;
	const auto digest = instrumented_crc32(message, context, eager, observed,
		checkpoint, result.known_after_checkpoint);
	if (!eager)
	{
		context.partial_assert(checkpoint);
		result.known_after_checkpoint = context.known_variable_count();
	}
	context.partial_assert(digest, u32(expected_digest));
	result.build_and_assert_ms = demo::milliseconds(demo::clock::now() - started);
	result.digest_nodes = demo::node_count(digest);
	auto retained = context.requirements();
	retained.insert(retained.end(), inputs.begin(), inputs.end());
	demo::append_bits(retained, digest);
	result.retained_nodes = demo::node_count(retained);
	result.assertions = context.assertion_count();
	br::solver_options solve_options;
	solve_options.max_search_steps = 2000000;
	br::solver_statistics stats;
	const auto solve_started = demo::clock::now();
	result.solutions = context.solve(inputs, [&](const br::collision_resolution::crs_state& model)
	{
		auto recovered = known;
		for (size_t index = 0; index < prefix_bytes; ++index)
			recovered[index] = demo::decoded_byte(message[index], model);
		demo::require((native_state(recovered, prefix_bytes) & checkpoint_mask) == observed,
			"CRC32 recovered input violates the native checkpoint");
		demo::require(~native_state(recovered, recovered.size()) == expected_digest,
			"CRC32 recovered input fails independent native verification");
		demo::require(recovered == known, "CRC32 recovered a different input despite unique checkpoint");
		result.recovered = std::move(recovered);
		return true;
	}, 2, solve_options, &stats);
	result.final_solve_ms = demo::milliseconds(demo::clock::now() - solve_started);
	result.solver_decisions = stats.decisions;
	result.solver_steps = stats.search_steps;
	demo::require(result.solutions == 1, "CRC32 constraints did not have exactly one solution");
	return result;
}
}

int main()
{
	try
	{
		const std::vector<std::uint8_t> check_vector{'1','2','3','4','5','6','7','8','9'};
		demo::require(~native_state(check_vector, check_vector.size()) == 0xcbf43926U,
			"CRC32 native reference failed its standard check vector");
		const std::string text = "AB/checkpoints";
		const std::vector<std::uint8_t> known(text.begin(), text.end());
		const auto observed = native_state(known, prefix_bytes) & checkpoint_mask;
		const auto digest = ~native_state(known, known.size());
		// This native enumeration verifies the example's uniqueness claim; it
		// supplies no assignments or training data to partial_assert_context.
		size_t matching_prefixes = 0;
		for (unsigned word = 0; word < 65536; ++word)
		{
			const std::vector<std::uint8_t> prefix{
				static_cast<std::uint8_t>(word), static_cast<std::uint8_t>(word >> 8)};
			if ((native_state(prefix, prefix_bytes) & checkpoint_mask) == observed) ++matching_prefixes;
		}
		demo::require(matching_prefixes == 1, "CRC32 masked checkpoint is not unique in this example");
		std::cout << "CRC32: two unknown prefix bytes, known suffix /checkpoints.\n"
			<< "Additional observation: internal CRC after byte 2 & 0xffff0000 = 0x"
			<< std::hex << observed << "; final digest = 0x" << digest << std::dec << ".\n"
			<< "Both modes receive exactly these two constraints. The checkpoint is extra information, not derived from the digest.\n";
		const auto eager = run(true, known, observed, digest);
		const auto deferred = run(false, known, observed, digest);
		demo::print("eager inside algorithm", eager);
		demo::print("deferred until algorithm ends", deferred);
		demo::require(eager.known_after_checkpoint == 16, "CRC32 checkpoint did not determine all 16 unknown bits");
		demo::require(eager.digest_nodes < deferred.digest_nodes, "CRC32 eager simplification did not shrink final expressions");
		demo::require(eager.recovered == deferred.recovered, "CRC32 comparison recovered different inputs");
		std::cout << "Native verification passed; recovered prefix AB. Node counts are retained DAGs, not peak allocations.\n";
		return 0;
	}
	catch (const std::exception& error)
	{
		std::cerr << "partial_assert_crc32: " << error.what() << '\n';
		return 1;
	}
}
