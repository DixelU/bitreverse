#ifndef DIXELU_BITREVERSE_SOLVER_COMPILED_CIRCUIT_H
#define DIXELU_BITREVERSE_SOLVER_COMPILED_CIRCUIT_H

// Internal header: included by bitreverse.h inside
// dixelu::bitreverse::collision_resolution::solver_core.

using node_t = const details::bitstate*;
using node_id = size_t;
inline constexpr node_id no_node = static_cast<node_id>(-1);

// Compressed sparse rows: two allocations instead of one vector per node.
// Keep argument-slot order (including duplicate children) for deterministic
// propagation and consumers such as circuit_metrics' topological traversal.
struct parent_adjacency
{
	std::vector<size_t> offsets;
	std::vector<node_id> edges;

	std::span<const node_id> operator[](node_id id) const
	{
		return std::span<const node_id>(edges).subspan(
			offsets[id], offsets[id + 1] - offsets[id]);
	}

	void build(const std::vector<std::array<node_id, 2>>& inputs)
	{
		offsets.assign(inputs.size() + 1, 0);
		for (const auto& children : inputs)
			for (const node_id child : children)
				if (child != no_node) ++offsets[child + 1];
		for (size_t id = 1; id < offsets.size(); ++id)
			offsets[id] += offsets[id - 1];
		edges.resize(offsets.back());
		// Fill backwards using the row ends as cursors, then restore offsets.
		for (node_id id = inputs.size(); id-- != 0;)
			for (const node_id child : inputs[id])
				if (child != no_node) edges[--offsets[child + 1]] = id;
		for (size_t id = 0; id < inputs.size(); ++id)
			offsets[id] = offsets[id + 1];
		offsets.back() = edges.size();
	}
};

struct compiled_circuit
{
	counted_ptr<details::bitstate> root_ptr;
	node_id root_id{no_node};
	std::vector<counted_ptr<details::bitstate>> root_ptrs;
	std::vector<node_id> root_ids;
	std::vector<counted_ptr<details::bitstate>> nodes;
	std::vector<std::uint8_t> operations;
	std::unordered_map<node_t, node_id> node_ids;
	std::vector<std::array<node_id, 2>> inputs;
	parent_adjacency parents;
	std::vector<node_id> variables;

	explicit compiled_circuit(counted_ptr<details::bitstate> root) :
		compiled_circuit(
			std::vector<counted_ptr<details::bitstate>>{std::move(root)})
	{
	}

	// Roots keep output expressions and explicitly declared inputs alive,
	// including unknown inputs optimized out of every output. Compilation
	// itself asserts no root value; compiled queries bind only supplied IDs.
	explicit compiled_circuit(
		std::vector<counted_ptr<details::bitstate>> roots) :
		root_ptr(roots.empty() ? counted_ptr<details::bitstate>{} : roots.front()),
		root_ptrs(std::move(roots))
	{
		build();
	}

	node_id add_node(const counted_ptr<details::bitstate>& node)
	{
		if (!node)
			return no_node;

		const node_t raw = node.get();
		const auto existing = node_ids.find(raw);
		if (existing != node_ids.end())
			return existing->second;

		const node_id id = nodes.size();
		node_ids.emplace(raw, id);
		nodes.push_back(node);
		operations.push_back(node->operation);
		inputs.push_back({no_node, no_node});
		return id;
	}

	void build()
	{
		for (const auto& root : root_ptrs)
			root_ids.push_back(add_node(root));
		root_id = root_ids.empty() ? no_node : root_ids.front();
		for (node_id id = 0; id < nodes.size(); ++id)
		{
			const auto& current = nodes[id];
			const std::uint8_t operation = current->operation;
			if (operation == '*')
				variables.push_back(id);

			const std::uint8_t argument_count =
				details::operation_args_count[operation];
			const counted_ptr<details::bitstate>* children[2] =
				{&current->_1, &current->_2};
			for (std::uint8_t index = 0;
				index < argument_count;
				++index)
			{
				const node_id child = add_node(*children[index]);
				inputs[id][index] = child;
			}
		}
		parents.build(inputs);

		// High-fanout inputs participate in more constraints and tend to
		// expose contradictions earlier.
		std::stable_sort(
			variables.begin(),
			variables.end(),
			[&](node_id lhs, node_id rhs)
			{
				return parents[lhs].size() > parents[rhs].size();
			});
	}
};

inline const compiled_circuit& require_compiled_circuit(
	const std::shared_ptr<const compiled_circuit>& circuit)
{
	if (!circuit)
		throw std::invalid_argument("Compiled circuit must not be null");
	return *circuit;
}

inline void validate_bindings(
	const compiled_circuit& circuit,
	const std::vector<std::pair<node_id, bool>>& bindings)
{
	for (const auto& [id, value] : bindings)
		if (id >= circuit.nodes.size())
			throw std::out_of_range("Compiled circuit binding has an invalid node ID");
}

#endif // DIXELU_BITREVERSE_SOLVER_COMPILED_CIRCUIT_H
