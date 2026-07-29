#ifndef DIXELU_BITREVERSE_SOLVER_COMPILED_CIRCUIT_H
#define DIXELU_BITREVERSE_SOLVER_COMPILED_CIRCUIT_H

// Internal header: included by bitreverse.h inside
// dixelu::bitreverse::collision_resolution::solver_core.

using node_t = const details::bitstate*;
using node_id = size_t;
inline constexpr node_id no_node = static_cast<node_id>(-1);

struct compiled_circuit
{
	counted_ptr<details::bitstate> root_ptr;
	node_id root_id{no_node};
	std::vector<counted_ptr<details::bitstate>> nodes;
	std::unordered_map<node_t, node_id> node_ids;
	std::vector<std::array<node_id, 2>> inputs;
	std::vector<std::vector<node_id>> parents;
	std::vector<node_id> variables;

	explicit compiled_circuit(counted_ptr<details::bitstate> root) :
		root_ptr(std::move(root))
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
		inputs.push_back({no_node, no_node});
		parents.emplace_back();
		return id;
	}

	void build()
	{
		root_id = add_node(root_ptr);
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
				if (child != no_node)
					parents[child].push_back(id);
			}
		}

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

#endif // DIXELU_BITREVERSE_SOLVER_COMPILED_CIRCUIT_H
