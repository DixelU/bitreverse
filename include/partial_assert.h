#ifndef DIXELU_BITREVERSE_PARTIAL_ASSERT_H
#define DIXELU_BITREVERSE_PARTIAL_ASSERT_H

#include "bitreverse.h"

namespace dixelu::bitreverse
{

struct partial_assert_options
{
	std::size_t max_nodes{200000};
	std::size_t max_steps{2000000};
	std::size_t max_affine_atoms{4096};
	// Bounds dense affine coefficients plus temporary elimination rows.
	std::size_t max_affine_bytes{64 * 1024 * 1024};
	bool affine_reasoning{true};
};

class partial_assert_conflict : public std::runtime_error
{
public:
	partial_assert_conflict() : std::runtime_error("Partial assertion contradicts accumulated constraints") {}
};

class partial_assert_limit : public std::runtime_error
{
public:
	explicit partial_assert_limit(const char* message) : std::runtime_error(message) {}
};

// An explicit, monotonic constraint context. Assertions propagate facts without
// selecting a model. A successful call is NOT a satisfiability proof: unresolved
// nonlinear contradictions may still require search in solve().
//
// simplify() returns new expressions valid under this context's requirements;
// it never changes the original DAG or replaces an unresolved input identity.
// Keep the requirements when solving those expressions (solve() does this).
class partial_assert_context
{
	using node_ptr = counted_ptr<details::bitstate>;
	using node_t = const details::bitstate*;
	using compiled_circuit = collision_resolution::solver_core::compiled_circuit;
	using replacement_map = std::unordered_map<node_t, node_ptr>;

	struct graph_info
	{
		std::vector<node_ptr> postorder;
		std::size_t atoms{0};
		std::size_t affine_depth{0};
	};

	partial_assert_options options_;
	// These original roots own every node referenced by facts_. In particular,
	// never replace an asserted root with its simplified constant true.
	std::vector<bit_tracker> requirements_;
	std::unordered_map<node_t, bool> facts_;
	std::size_t known_variables_{0};
	solver_statistics statistics_;

	graph_info inspect(const std::vector<node_ptr>& roots) const
	{
		graph_info result;
		std::unordered_map<node_t, unsigned char> color;
		std::unordered_map<node_t, std::size_t> depths;
		std::vector<std::pair<node_ptr, bool>> stack;
		for (const auto& root : roots)
		{
			stack.emplace_back(root, false);
			while (!stack.empty())
			{
				auto [node, finish] = std::move(stack.back());
				stack.pop_back();
				if (!node)
					throw std::invalid_argument("Partial assertion has a null expression node");
				const node_t raw = node.get();
				const auto op = node->operation;
				if (finish)
				{
					std::size_t depth = 0;
					if (op == '!' || op == '^')
					{
						depth = depths.at(node->_1.get());
						if (op == '^') depth = std::max(depth, depths.at(node->_2.get()));
						++depth;
					}
					depths.emplace(raw, depth);
					result.affine_depth = std::max(result.affine_depth, depth);
					result.atoms += op == '*' || op == '&' || op == '|';
					color.at(raw) = 2;
					result.postorder.push_back(std::move(node));
					continue;
				}
				const auto existing = color.find(raw);
				if (existing != color.end())
				{
					if (existing->second == 1)
						throw std::invalid_argument("Partial assertion has a cyclic expression");
					continue;
				}
				if (op != '=' && op != '*' && op != '!' && op != '^' && op != '&' && op != '|')
					throw std::invalid_argument("Partial assertion has an unsupported operation");
				if (color.size() >= options_.max_nodes)
					throw partial_assert_limit("Partial assertion node budget exceeded");
				color.emplace(raw, 1);
				stack.emplace_back(node, true);
				const auto arity = details::operation_args_count[op];
				if (arity == 2) stack.emplace_back(node->_2, false);
				if (arity >= 1) stack.emplace_back(node->_1, false);
			}
		}
		return result;
	}

	solver_options bounded_affine_options(const graph_info& graph, solver_options selected) const
	{
		selected.max_affine_atoms = std::min(selected.max_affine_atoms, options_.max_affine_atoms);
		// Existing affine initialization recursively expands XOR/NOT chains.
		// Gate propagation remains available when dense/recursive work is unsafe.
		const std::size_t words = graph.atoms / 64 + (graph.atoms % 64 != 0);
		const bool fits = words == 0 ||
			graph.postorder.size() <= options_.max_affine_bytes / sizeof(std::uint64_t) / 2 / words;
		selected.affine_reasoning = selected.affine_reasoning && options_.affine_reasoning &&
			graph.atoms <= selected.max_affine_atoms && graph.affine_depth <= 512 && fits;
		return selected;
	}

	std::vector<node_ptr> roots() const
	{
		std::vector<node_ptr> result;
		result.reserve(requirements_.size());
		for (const auto& requirement : requirements_) result.push_back(requirement.bit_state);
		return result;
	}

	replacement_map replacements(const std::vector<node_ptr>& selected) const
	{
		replacement_map rebuilt;
		for (const auto& node : inspect(selected).postorder)
		{
			const node_t raw = node.get();
			if (const auto fact = facts_.find(raw); fact != facts_.end())
			{
				rebuilt.emplace(raw, details::make_boolean_constant(fact->second));
				continue;
			}
			const auto arity = details::operation_args_count[node->operation];
			if (arity == 0)
			{
				rebuilt.emplace(raw, node);
				continue;
			}
			const auto& lhs = rebuilt.at(node->_1.get());
			const node_ptr rhs = arity == 2 ? rebuilt.at(node->_2.get()) : node_ptr{};
			if (lhs == node->_1 && rhs == node->_2)
				rebuilt.emplace(raw, node);
			else
				rebuilt.emplace(raw, details::make_bitstate_operation(node->operation, lhs, rhs));
		}
		return rebuilt;
	}

public:
	explicit partial_assert_context(partial_assert_options options = {}) : options_(options)
	{
		if (!options_.max_nodes || !options_.max_steps || !options_.max_affine_bytes)
			throw std::invalid_argument("Partial assertion node, step, and byte budgets must be positive");
	}

	// Transactional: conflicts, invalid DAGs and resource limits leave the last
	// successful requirements, facts and statistics untouched. No SAT search.
	void partial_assert(bit_tracker condition)
	{
		using namespace collision_resolution::solver_core;
		const auto started = std::chrono::steady_clock::now();
		auto pending = requirements_;
		pending.push_back(std::move(condition));
		auto selected = roots();
		selected.push_back(pending.back().bit_state);
		const auto graph = inspect(selected);
		const compiled_circuit circuit(std::move(selected));
		solver_statistics next_statistics;
		next_statistics.nodes = circuit.nodes.size();
		next_statistics.variables = circuit.variables.size();
		search_budget budget{options_.max_steps, 0, &next_statistics};
		solver_state state(circuit, &next_statistics, &budget);
		gate_propagator gates(circuit);
		try
		{
			// Earlier facts are consequences of retained requirements, so they
			// remain valid even if the larger graph disables affine reasoning.
			for (std::size_t id = 0; id < circuit.nodes.size(); ++id)
			{
				const auto fact = facts_.find(circuit.nodes[id].get());
				if (fact == facts_.end()) continue;
				budget.tick();
				if (!state.set_value(id, fact->second))
					throw partial_assert_conflict();
			}
			for (const auto id : circuit.root_ids)
			{
				budget.tick();
				if (!state.set_value(id, true)) throw partial_assert_conflict();
			}
			std::size_t cursor = 0;
			if (!gates.propagate_pending(state, cursor)) throw partial_assert_conflict();
			// Often an early checkpoint already resolves every variable. Avoid
			// allocating a dense affine system when there is nothing left to infer.
			const bool unresolved = std::any_of(circuit.variables.begin(), circuit.variables.end(),
				[&](node_id id) { return state.value_of(id) == -1; });
			auto selected_options = bounded_affine_options(graph, {});
			selected_options.affine_reasoning = selected_options.affine_reasoning && unresolved;
			affine_propagator affine(circuit, selected_options);
			next_statistics.affine_atoms = affine.atom_count;
			next_statistics.affine_enabled = affine.active;
			while (affine.active)
			{
				const auto mark = state.trail.size();
				if (!affine.propagate(state) || !gates.propagate_pending(state, cursor))
					throw partial_assert_conflict();
				if (mark == state.trail.size()) break;
			}
		}
		catch (const solver_limit&)
		{
			throw partial_assert_limit("Partial assertion propagation step budget exceeded");
		}
		std::unordered_map<node_t, bool> next_facts;
		for (std::size_t id = 0; id < circuit.nodes.size(); ++id)
			if (state.value_of(id) != -1)
				next_facts.emplace(circuit.nodes[id].get(), state.value_of(id) != 0);
		std::size_t next_known = 0;
		for (const auto id : circuit.variables) next_known += state.value_of(id) != -1;
		next_statistics.elapsed = std::chrono::steady_clock::now() - started;
		requirements_.swap(pending);
		facts_.swap(next_facts);
		known_variables_ = next_known;
		statistics_ = next_statistics;
	}

	template<std::size_t N>
	void partial_assert(const int_tracker<N>& lhs, const int_tracker<N>& rhs)
	{
		partial_assert(int_tracker<N>::are_equal(lhs, rhs));
	}

	bit_tracker simplify(const bit_tracker& value) const
	{
		auto rebuilt = replacements({value.bit_state});
		return bit_tracker(node_ptr(rebuilt.at(value.bit_state.get())));
	}

	template<std::size_t N>
	std::vector<int_tracker<N>> simplify(const std::vector<int_tracker<N>>& values) const
	{
		std::vector<node_ptr> selected;
		for (const auto& value : values)
			for (const auto& bit : value.bits) selected.push_back(bit.bit_state);
		auto rebuilt = replacements(selected);
		auto result = values;
		for (auto& value : result)
			for (auto& bit : value.bits) bit.bit_state = rebuilt.at(bit.bit_state.get());
		return result;
	}

	template<std::size_t N>
	int_tracker<N> simplify(const int_tracker<N>& value) const
	{
		return simplify(std::vector<int_tracker<N>>{value}).front();
	}

	const std::vector<bit_tracker>& requirements() const { return requirements_; }
	std::size_t assertion_count() const { return requirements_.size(); }
	std::size_t known_variable_count() const { return known_variables_; }
	const solver_statistics& statistics() const { return statistics_; }

	// Explicit input leaves preserve inputs that disappear during simplification.
	// max_solutions == 0 enumerates all models; returning false stops the stream.
	// This is the only operation in this context that makes search decisions.
	std::size_t solve(const std::vector<bit_tracker>& inputs,
		collision_resolution::solution_callback callback = {},
		std::size_t max_solutions = 1, const solver_options& options = {},
		solver_statistics* statistics = nullptr) const
	{
		auto selected = roots();
		for (const auto& input : inputs)
		{
			if (!input.bit_state || (input.bit_state->operation != '*' && input.bit_state->operation != '='))
				throw std::invalid_argument("Partial assertion solve inputs must be original input leaves");
			selected.push_back(input.bit_state);
		}
		const auto graph = inspect(selected);
		auto circuit = std::make_shared<const compiled_circuit>(std::move(selected));
		std::vector<std::pair<std::size_t, bool>> bindings;
		for (std::size_t i = 0; i < requirements_.size(); ++i)
			bindings.emplace_back(circuit->root_ids[i], true);
		// Reuse earlier deductions even when the final graph crosses an affine
		// limit. They are justified by the original requirements retained above.
		for (std::size_t id = 0; id < circuit->nodes.size(); ++id)
			if (const auto fact = facts_.find(circuit->nodes[id].get()); fact != facts_.end())
				bindings.emplace_back(id, fact->second);
		std::size_t count = 0;
		return collision_resolution::solve_compiled_stream(circuit, bindings,
			[&](const collision_resolution::crs_state& solution)
			{
				++count;
				const bool keep_going = !callback || callback(solution);
				return keep_going && (!max_solutions || count < max_solutions);
			}, bounded_affine_options(graph, options), statistics);
	}
};

} // namespace dixelu::bitreverse

#endif // DIXELU_BITREVERSE_PARTIAL_ASSERT_H
