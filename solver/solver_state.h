#ifndef DIXELU_BITREVERSE_SOLVER_STATE_H
#define DIXELU_BITREVERSE_SOLVER_STATE_H

// Internal header: included by bitreverse.h inside
// dixelu::bitreverse::collision_resolution::solver_core.

struct search_budget
{
	size_t limit, steps{0};
	solver_statistics* statistics;

	void tick(size_t amount = 1)
	{
		if (limit && amount > limit - steps)
			throw solver_limit();
		// Saturate statistics in unlimited mode instead of wrapping the counter.
		steps += std::min(amount, std::numeric_limits<size_t>::max() - steps);
		if (statistics) statistics->search_steps = steps;
	}
};

// Preserve timing when a budget or a user callback interrupts a run.
struct run_timer
{
	solver_statistics* statistics;
	std::chrono::steady_clock::time_point started = std::chrono::steady_clock::now();
	~run_timer()
	{
		if (statistics)
			statistics->elapsed = std::chrono::steady_clock::now() - started;
	}
};

struct solver_state
{
	const compiled_circuit& circuit;
	solver_statistics* statistics;
	search_budget* budget;
	std::vector<int8_t> values;
	std::vector<node_id> trail;
	std::vector<node_id> propagation_queue;

	explicit solver_state(
		const compiled_circuit& compiled,
		solver_statistics* stats = nullptr,
		search_budget* search = nullptr) :
		circuit(compiled),
		statistics(stats),
		budget(search),
		values(compiled.nodes.size(), -1)
	{
		trail.reserve(compiled.nodes.size());
		propagation_queue.reserve(compiled.nodes.size());
	}

	void tick_search(size_t amount = 1) const
	{
		if (budget) budget->tick(amount);
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
