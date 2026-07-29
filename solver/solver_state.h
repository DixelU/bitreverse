#ifndef DIXELU_BITREVERSE_SOLVER_STATE_H
#define DIXELU_BITREVERSE_SOLVER_STATE_H

// Internal header: included by bitreverse.h inside
// dixelu::bitreverse::collision_resolution::solver_core.

struct solver_state
{
	const compiled_circuit& circuit;
	solver_statistics* statistics;
	std::vector<int8_t> values;
	std::vector<node_id> trail;
	std::vector<node_id> propagation_queue;

	explicit solver_state(
		const compiled_circuit& compiled,
		solver_statistics* stats = nullptr) :
		circuit(compiled),
		statistics(stats),
		values(compiled.nodes.size(), -1)
	{
		trail.reserve(compiled.nodes.size());
		propagation_queue.reserve(compiled.nodes.size());
	}

	int8_t value_of(node_id id) const
	{
		if (id == no_node)
			return -1;

		const auto& node = circuit.nodes[id];
		if (node->operation == '=')
			return static_cast<int8_t>(node->state);
		return values[id];
	}

	bool set_value(node_id id, bool value)
	{
		const int8_t current = value_of(id);
		if (current != -1)
			return current == static_cast<int8_t>(value);

		values[id] = static_cast<int8_t>(value);
		trail.push_back(id);
		propagation_queue.push_back(id);
		if (statistics)
			statistics->peak_trail =
				std::max(statistics->peak_trail, trail.size());
		return true;
	}

	void begin_assignment()
	{
		propagation_queue.clear();
	}

	void undo_to(size_t mark)
	{
		while (trail.size() > mark)
		{
			values[trail.back()] = -1;
			trail.pop_back();
		}
	}
};

#endif // DIXELU_BITREVERSE_SOLVER_STATE_H
