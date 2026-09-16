#include <iostream>
#include <random>
#include <set>
#include <stdexcept>
#include <tuple>
#include <vector>

#include "partial_assert.h"

namespace br = dixelu::bitreverse;
namespace cr = br::collision_resolution;

namespace
{
using bits = std::vector<br::bit_tracker>;
using models = std::set<unsigned>;

void require(bool condition, const char* message)
{
	if (!condition) throw std::runtime_error(message);
}

template<class Exception, class Function>
void reject(Function&& function, const char* message)
{
	try { function(); }
	catch (const Exception&) { return; }
	throw std::runtime_error(message);
}

bool is_constant(const br::bit_tracker& bit, bool value)
{
	return bit.bit_state->operation == '=' && (bit.bit_state->state != 0) == value;
}

bits unknowns(size_t count)
{
	bits result(count);
	for (auto& bit : result) bit = br::unknown;
	return result;
}

template<size_t Width>
bits integer_bits(const br::int_tracker<Width>& value)
{
	return {value.bits.rbegin(), value.bits.rend()};
}

template<size_t Width>
unsigned constant_word(const br::int_tracker<Width>& value)
{
	unsigned result = 0;
	for (const auto& bit : value.bits)
	{
		require(bit.bit_state->operation == '=', "simplified integer still has an unknown bit");
		result = (result << 1) | static_cast<unsigned>(bit.bit_state->state);
	}
	return result;
}

auto statistics_key(const br::solver_statistics& value)
{
	return std::make_tuple(value.nodes, value.variables, value.decisions, value.propagations,
		value.affine_passes, value.affine_atoms, value.conflicts, value.solutions, value.learned_clauses,
		value.backjumps, value.conflict_analysis_cutoffs, value.peak_trail, value.search_steps,
		value.affine_enabled, value.elapsed);
}

struct context_snapshot
{
	size_t assertions, known_variables;
	br::solver_statistics statistics;
	std::vector<const br::details::bitstate*> roots;

	explicit context_snapshot(const br::partial_assert_context& context)
		: assertions(context.assertion_count()), known_variables(context.known_variable_count()),
		statistics(context.statistics())
	{
		for (const auto& root : context.requirements()) roots.push_back(root.bit_state.get());
	}

	void check(const br::partial_assert_context& context) const
	{
		require(context.assertion_count() == assertions && context.known_variable_count() == known_variables,
			"failed assertion changed counts or forced facts");
		require(statistics_key(context.statistics()) == statistics_key(statistics),
			"failed assertion changed successful propagation statistics");
		require(context.requirements().size() == roots.size(), "failed assertion changed retained requirement count");
		for (size_t i = 0; i < roots.size(); ++i)
			require(context.requirements()[i].bit_state.get() == roots[i], "failed assertion replaced a retained requirement");
	}
};

void propagation_only(const br::partial_assert_context& context)
{
	require(context.statistics().decisions == 0 && context.statistics().solutions == 0,
		"partial assertion performed search or froze a satisfying model");
}

models solve_models(const br::partial_assert_context& context, const bits& inputs, const br::solver_options& options = {})
{
	models actual;
	const auto count = context.solve(inputs, [&](const cr::crs_state& state)
	{
		unsigned word = 0;
		for (size_t i = 0; i < inputs.size(); ++i)
		{
			const auto found = state.assignments.find(inputs[i].bit_state);
			if (inputs[i].bit_state->operation == '=')
				word |= static_cast<unsigned>(inputs[i].bit_state->state) << i;
			else
			{
				require(found != state.assignments.end(), "explicit solve lost a requested free input");
				word |= static_cast<unsigned>(found->second) << i;
			}
		}
		require(actual.insert(word).second, "explicit solve emitted a duplicate input assignment");
		return true;
	}, 0, options);
	require(count == actual.size(), "explicit solve count differs from callbacks");
	return actual;
}

void no_arbitrary_freezing()
{
	const auto input = unknowns(3);
	const auto condition = input[0] | input[1];
	br::partial_assert_context context;
	context.partial_assert(condition);
	propagation_only(context);
	require(context.assertion_count() == 1 && context.known_variable_count() == 0,
		"a disjunction arbitrarily fixed one of its input bits");
	require(context.simplify(input[0]).bit_state.get() == input[0].bit_state.get() &&
		context.simplify(input[1]).bit_state.get() == input[1].bit_state.get(), "unforced unknown identity changed");
	const auto snapshot = context.simplify(condition);
	const char snapshot_operation = static_cast<char>(snapshot.bit_state->operation);
	const bool snapshot_state = snapshot.bit_state->state != 0;
	const auto snapshot_first = snapshot.bit_state->_1.get(), snapshot_second = snapshot.bit_state->_2.get();
	context.partial_assert(!input[0]);
	require(is_constant(context.simplify(input[0]), false) && is_constant(context.simplify(input[1]), true),
		"later assertion did not propagate through an earlier disjunction");
	require(context.known_variable_count() == 2, "forced leaf count includes gates or misses an implied leaf");
	require(context.requirements().size() == 2 && context.assertion_count() == 2,
		"simplification discarded an earlier relation");
	require(input[0].bit_state->operation == '*' && input[1].bit_state->operation == '*' &&
		input[0].bit_state->state == 0 && input[1].bit_state->state == 0 &&
		condition.bit_state->operation == '|' && condition.bit_state->state == 0,
		"propagation modified the original expression graph");
	require(snapshot.bit_state->operation == snapshot_operation && (snapshot.bit_state->state != 0) == snapshot_state &&
		snapshot.bit_state->_1.get() == snapshot_first && snapshot.bit_state->_2.get() == snapshot_second,
		"previous simplification snapshot was mutated");
	require(solve_models(context, input) == models{2, 6}, "explicit solve lost unconstrained inputs or retained requirements");
	const context_snapshot before(context);
	reject<br::partial_assert_conflict>([&] { context.partial_assert(!input[1]); },
		"contradictory assertion was accepted by propagation");
	before.check(context);
	require(is_constant(context.simplify(input[1]), true) && solve_models(context, input) == models{2, 6},
		"conflict rollback changed future simplification or solve results");
}

void equality_and_snapshots()
{
	br::int_tracker<4> lhs{br::unknown}, rhs{br::unknown};
	br::partial_assert_context context;
	context.partial_assert(lhs, rhs);
	propagation_only(context);
	require(context.known_variable_count() == 0, "symbolic equality chose values for disjoint input graphs");
	const auto snapshot = context.simplify(lhs);
	const auto branch = context;
	for (size_t bit = 0; bit < 4; ++bit)
		require(snapshot.bits[bit].bit_state.get() == lhs.bits[bit].bit_state.get(),
			"equality snapshot replaced an unforced input identity");
	context.partial_assert(rhs, br::int_tracker<4>{10});
	require(constant_word(context.simplify(lhs)) == 10 && constant_word(context.simplify(rhs)) == 10,
		"later constant equality did not simplify both disjoint graphs");
	require(context.known_variable_count() == 8 && branch.known_variable_count() == 0,
		"adding an assertion changed a copied context snapshot");
	for (size_t bit = 0; bit < 4; ++bit)
		require(snapshot.bits[bit].bit_state->operation == '*' && lhs.bits[bit].bit_state->operation == '*' &&
			rhs.bits[bit].bit_state->operation == '*', "simplification mutated an original or prior returned integer");
	const auto array = context.simplify(std::vector<br::int_tracker<4>>{lhs, rhs, lhs ^ br::int_tracker<4>{3}});
	require(array.size() == 3 && constant_word(array[0]) == 10 && constant_word(array[1]) == 10 && constant_word(array[2]) == 9,
		"vector integer simplification lost order or failed to fold constants");
	const context_snapshot before(context);
	reject<br::partial_assert_conflict>([&] { context.partial_assert(lhs, br::int_tracker<4>{11}); },
		"conflicting integer equality was accepted");
	before.check(context);
	require(solve_models(context, integer_bits(lhs)) == models{10}, "equalities did not survive explicit solve");
	require(solve_models(branch, integer_bits(lhs)).size() == 16, "copied context lost its independent free values");

	const auto input = unknowns(3);
	br::partial_assert_context aliases;
	aliases.partial_assert(!(input[0] ^ input[1]));
	aliases.partial_assert(!(input[1] ^ input[2]));
	require(aliases.known_variable_count() == 0 && solve_models(aliases, input) == models{0, 7},
		"nonconstant aliases were frozen or their relation was lost");
	aliases.partial_assert(input[2]);
	for (const auto& bit : input)
		require(is_constant(aliases.simplify(bit), true), "later alias value did not propagate across equalities");
}

void masked_comparison()
{
	br::int_tracker<8> value{br::unknown};
	br::partial_assert_context context;
	context.partial_assert(value & br::int_tracker<8>{0xf0}, br::int_tracker<8>{0xa0});
	const auto simplified = context.simplify(value);
	for (size_t bit = 0; bit < 8; ++bit)
	{
		if (bit < 4) require(is_constant(simplified.bits[bit], ((0xa0U >> (7 - bit)) & 1U) != 0),
			"masked equality failed to force a selected bit");
		else require(simplified.bits[bit].bit_state.get() == value.bits[bit].bit_state.get(),
			"masked equality froze or replaced an unselected input");
	}
	require(context.known_variable_count() == 4, "masked equality has the wrong forced input count");
	models expected;
	for (unsigned word = 0xa0; word < 0xb0; ++word) expected.insert(word);
	require(solve_models(context, integer_bits(value)) == expected, "masked comparison changed its complete model set");
	context.partial_assert(value & br::int_tracker<8>{0x0f}, br::int_tracker<8>{5});
	require(constant_word(context.simplify(value)) == 0xa5, "a later complementary mask did not finish the integer");
}

void deferred_unsat_and_solve_contract()
{
	const auto input = unknowns(2);
	br::partial_assert_options options;
	options.affine_reasoning = false;
	br::partial_assert_context context(options);
	for (const auto& clause : bits{input[0] | input[1], (!input[0]) | input[1], input[0] | (!input[1]), (!input[0]) | (!input[1])})
		context.partial_assert(clause);
	propagation_only(context);
	require(context.assertion_count() == 4 && context.known_variable_count() == 0,
		"propagation searched a formula with no unit consequences");
	require(solve_models(context, input).empty(), "explicit solve accepted a deferred non-BCP contradiction");
	propagation_only(context);

	br::partial_assert_context empty;
	require(solve_models(empty, input) == models{0, 1, 2, 3}, "empty context did not enumerate explicit free inputs");
	require(empty.solve(input, [](const auto&) { return true; }) == 1, "explicit solve default limit is not one");
	require(empty.solve(input, [](const auto&) { return false; }, 0) == 1, "explicit solve ignored callback cancellation");
	require(empty.solve(input, [](const auto&) { return true; }, 2) == 2, "explicit solve ignored a finite result limit");
	require(solve_models(empty, {}) == models{0}, "empty context without input leaves lost its empty model");
	require(empty.solve({}, {}, 0) == 1, "count-only empty context lost its empty assignment");
	require(empty.solve(input, {}, 0) == 4 && empty.solve(input) == 1, "count-only explicit solve ignored its result limit");
	const context_snapshot before(empty);
	br::solver_options bounded;
	bounded.max_search_steps = 1;
	reject<br::solver_limit>([&] { (void)empty.solve(input, {}, 1, bounded); },
		"explicit solve silently exceeded its budget when statistics were omitted");
	before.check(empty);
	reject<br::partial_assert_conflict>([&] { empty.partial_assert(br::bit_tracker(false)); },
		"constant false assertion was accepted");
	before.check(empty);
	empty.partial_assert(br::bit_tracker(true));
	require(solve_models(empty, input) == models{0, 1, 2, 3}, "constant true assertion changed free inputs");
}

bool native_value(const br::details::bitstate* node, const bits& inputs, unsigned word)
{
	if (node->operation == '=') return node->state != 0;
	if (node->operation == '*')
	{
		for (size_t bit = 0; bit < inputs.size(); ++bit)
			if (node == inputs[bit].bit_state.get()) return ((word >> bit) & 1U) != 0;
		throw std::runtime_error("simplification introduced an unrelated unknown identity");
	}
	const bool lhs = native_value(node->_1.get(), inputs, word);
	if (node->operation == '!') return !lhs;
	const bool rhs = native_value(node->_2.get(), inputs, word);
	if (node->operation == '&') return lhs && rhs;
	if (node->operation == '|') return lhs || rhs;
	if (node->operation == '^') return lhs != rhs;
	throw std::runtime_error("unexpected expression operation in native evaluator");
}

void random_reference_cases(bool affine)
{
	std::mt19937 random(0xa5172d9U);
	for (size_t trial = 0; trial < 12; ++trial)
	{
		const auto input = unknowns(4);
		bits formulas = input;
		br::partial_assert_options options;
		options.affine_reasoning = affine;
		br::partial_assert_context context(options);
		models expected;
		for (unsigned word = 0; word < 16; ++word) expected.insert(word);
		for (size_t step = 0; step < 20; ++step)
		{
			const auto lhs = formulas[random() % formulas.size()];
			const auto rhs = formulas[random() % formulas.size()];
			const auto operation = random() % 4;
			const auto formula = operation == 0 ? lhs & rhs : operation == 1 ? lhs | rhs : operation == 2 ? lhs ^ rhs : !lhs;
			formulas.push_back(formula);
			if (step % 3 != 2) continue;
			models proposed;
			for (const auto word : expected)
				if (native_value(formula.bit_state.get(), input, word)) proposed.insert(word);
			const context_snapshot before(context);
			try { context.partial_assert(formula); expected = std::move(proposed); }
			catch (const br::partial_assert_conflict&)
			{
				require(proposed.empty(), "propagation rejected a satisfiable native reference formula");
				before.check(context);
			}
			propagation_only(context);
			require(solve_models(context, input) == expected, "partial assertion differs from the exhaustive native model set");
			const auto simplified = context.simplify(formula);
			for (const auto word : expected)
			{
				require(native_value(simplified.bit_state.get(), input, word) == native_value(formula.bit_state.get(), input, word),
					"simplification changed expression values on a retained model");
				for (size_t bit = 0; bit < input.size(); ++bit)
				{
					const auto inferred = context.simplify(input[bit]);
					if (inferred.bit_state->operation == '=')
						require((inferred.bit_state->state != 0) == (((word >> bit) & 1U) != 0),
							"a propagated input fact excludes a valid native model");
				}
			}
			for (const auto& bit : input) require(bit.bit_state->operation == '*', "random propagation mutated an input leaf");
		}
	}
}

void budgets_and_rollback()
{
	const auto input = unknowns(4);
	br::partial_assert_options node_options;
	node_options.max_nodes = 1;
	br::partial_assert_context node_limited(node_options);
	node_limited.partial_assert(input[0]);
	const context_snapshot node_before(node_limited);
	reject<br::partial_assert_limit>([&] { node_limited.partial_assert(input[1] | input[2]); },
		"partial assertion exceeded its node budget");
	node_before.check(node_limited);
	require(is_constant(node_limited.simplify(input[0]), true), "node limit discarded an earlier fact");

	br::partial_assert_context measured;
	measured.partial_assert(input[0]);
	require(measured.statistics().search_steps > 0, "successful propagation did not record budget units");
	br::partial_assert_options step_options;
	step_options.max_steps = measured.statistics().search_steps;
	br::partial_assert_context step_limited(step_options);
	step_limited.partial_assert(input[0]);
	const context_snapshot step_before(step_limited);
	reject<br::partial_assert_limit>([&] { step_limited.partial_assert(input[1] ^ input[2] ^ input[3]); },
		"partial assertion exceeded its step budget");
	step_before.check(step_limited);
	require(is_constant(step_limited.simplify(input[0]), true), "step limit discarded an earlier fact");

	for (unsigned kind = 0; kind < 3; ++kind)
	{
		br::partial_assert_options invalid;
		if (kind == 0) invalid.max_nodes = 0;
		if (kind == 1) invalid.max_steps = 0;
		if (kind == 2) invalid.max_affine_bytes = 0;
		reject<std::invalid_argument>([&] { br::partial_assert_context rejected(invalid); },
			"partial assertion accepted an invalid zero budget");
	}
}

void affine_fallbacks()
{
	const auto input = unknowns(2);
	for (unsigned kind = 0; kind < 3; ++kind)
	{
		br::partial_assert_options options;
		if (kind == 0) options.max_affine_atoms = 0;
		if (kind == 1) options.max_affine_atoms = 1;
		if (kind == 2) options.max_affine_bytes = 1;
		br::partial_assert_context context(options);
		context.partial_assert(input[0] ^ input[1]);
		require(!context.statistics().affine_enabled && context.known_variable_count() == 0,
			"affine resource cap did not fall back to sound gate propagation");
		require(solve_models(context, input) == models{1, 2}, "affine fallback dropped the retained parity relation");
		context.partial_assert(!input[0]);
		require(is_constant(context.simplify(input[1]), true), "affine fallback disabled ordinary gate propagation");
		propagation_only(context);
	}

	const auto deep_inputs = unknowns(520);
	auto parity = deep_inputs.front();
	for (size_t i = 1; i < deep_inputs.size(); ++i) parity ^= deep_inputs[i];
	br::partial_assert_context deep;
	deep.partial_assert(parity);
	require(!deep.statistics().affine_enabled && deep.known_variable_count() == 0 && deep.assertion_count() == 1,
		"deep affine expression did not safely retain its relation without recursive affine preprocessing");
	propagation_only(deep);
	for (const auto& bit : deep_inputs)
		require(deep.simplify(bit).bit_state.get() == bit.bit_state.get(), "depth fallback froze an unforced parity input");
}

void retain_affine_facts_after_fallback()
{
	const auto input = unknowns(5);
	br::partial_assert_options options;
	options.max_affine_atoms = 3;
	br::partial_assert_context context(options);
	context.partial_assert(!(input[0] ^ input[1] ^ input[2]));
	context.partial_assert(!(input[0] ^ input[1]));
	require(is_constant(context.simplify(input[2]), false), "affine propagation did not combine parity equations");
	context.partial_assert(input[3] | input[4]);
	require(!context.statistics().affine_enabled, "expanded graph did not exercise affine fallback");
	require(is_constant(context.simplify(input[2]), false) && context.known_variable_count() >= 1,
		"later affine fallback discarded a previously proven fact");
	models expected;
	for (unsigned word = 0; word < 32; ++word)
		if (((word & 1U) == ((word >> 1) & 1U)) && !(word & 4U) && (word & 24U)) expected.insert(word);
	require(solve_models(context, input) == expected, "affine fallback changed accumulated parity constraints");
	const context_snapshot before(context);
	reject<br::partial_assert_conflict>([&] { context.partial_assert(input[2]); },
		"affine fallback forgot a fact when checking a later contradiction");
	before.check(context);
}

void malformed_graph_rollback()
{
	const auto input = unknowns(2);
	br::partial_assert_context context;
	context.partial_assert(input[0]);
	const context_snapshot before(context);
	br::bit_tracker null;
	null.bit_state = {};
	reject<std::invalid_argument>([&] { context.partial_assert(null); }, "partial assertion accepted a null root");
	before.check(context);
	reject<std::invalid_argument>([&] { (void)context.solve({null}); }, "explicit solve accepted a null input");
	before.check(context);

	auto unknown_node = dixelu::make_counted<br::details::bitstate>();
	unknown_node->operation = '?';
	const br::bit_tracker unknown_operation(std::move(unknown_node));
	reject<std::invalid_argument>([&] { context.partial_assert(unknown_operation); }, "partial assertion accepted an unknown operation");
	before.check(context);

	auto missing_node = dixelu::make_counted<br::details::bitstate>();
	missing_node->operation = '&';
	missing_node->_1 = input[1].bit_state;
	const br::bit_tracker missing_child(std::move(missing_node));
	reject<std::invalid_argument>([&] { context.partial_assert(missing_child); }, "partial assertion accepted a null gate child");
	before.check(context);

	auto cycle = dixelu::make_counted<br::details::bitstate>();
	cycle->operation = '!';
	cycle->_1 = cycle;
	struct cycle_cleanup
	{
		decltype(br::bit_tracker{}.bit_state) node;
		~cycle_cleanup() { node->_1 = {}; }
	} cleanup{cycle};
	const br::bit_tracker cyclic(std::move(cycle));
	reject<std::invalid_argument>([&] { context.partial_assert(cyclic); }, "partial assertion accepted a cyclic expression");
	before.check(context);
	require(is_constant(context.simplify(input[0]), true) && solve_models(context, input) == models{1, 3},
		"malformed graph rejection changed previously retained facts");
}

void copied_lifetime_and_cdcl()
{
	const auto copied = []
	{
		const auto local = unknowns(2);
		br::partial_assert_context source;
		source.partial_assert(local[0] | local[1]);
		source.partial_assert(!local[0]);
		return br::partial_assert_context(source);
	}();
	// Only the copied context owns the original DAG after the lambda returns.
	auto first = copied.requirements()[1].bit_state->_1;
	const auto& disjunction = copied.requirements()[0].bit_state;
	auto second = disjunction->_1 == first ? disjunction->_2 : disjunction->_1;
	const bits retained{br::bit_tracker(std::move(first)), br::bit_tracker(std::move(second))};
	require(is_constant(copied.simplify(retained[0]), false) && is_constant(copied.simplify(retained[1]), true),
		"copied context lost derived facts after its source and expressions were destroyed");
	require(solve_models(copied, retained) == models{2}, "copied context lost its owned requirements after source destruction");

	const auto input = unknowns(3);
	br::partial_assert_context context;
	context.partial_assert(input[0] | input[1]);
	context.partial_assert(!input[0]);
	br::solver_options cdcl;
	cdcl.affine_reasoning = false;
	cdcl.conflict_learning = true;
	const context_snapshot before(context);
	require(solve_models(context, input, cdcl) == models{2, 6},
		"CDCL enumeration changed retained derived facts or omitted a free input");
	before.check(context);
}
}

int main()
{
	try
	{
		no_arbitrary_freezing();
		equality_and_snapshots();
		masked_comparison();
		deferred_unsat_and_solve_contract();
		random_reference_cases(false);
		random_reference_cases(true);
		budgets_and_rollback();
		affine_fallbacks();
		retain_affine_facts_after_fallback();
		malformed_graph_rollback();
		copied_lifetime_and_cdcl();
		std::cout << "All partial assertion tests passed\n";
	}
	catch (const std::exception& error)
	{
		std::cerr << "Partial assertion test failure: " << error.what() << '\n';
		return 1;
	}
}
