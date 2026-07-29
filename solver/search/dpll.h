#ifndef DIXELU_BITREVERSE_SOLVER_SEARCH_DPLL_H
#define DIXELU_BITREVERSE_SOLVER_SEARCH_DPLL_H

// Internal header: included by bitreverse.h inside
// dixelu::bitreverse::collision_resolution::dpll.

struct engine
{
	using solution_callback = std::function<bool(const crs_state&)>;

	bool target;
	bool first_only;
	bool collect_solutions;
	solver_options options;
	solver_statistics* statistics;
	solution_callback on_solution;
	bool stop_requested{false};
	size_t solution_count{0};

	compiled_circuit circuit;
	solver_state state;
	gate_propagator gates;
	affine_propagator affine;
	solutions_t solutions;

	engine(
		counted_ptr<details::bitstate> root,
		bool target_value,
		bool first,
		bool collect = true,
		solution_callback callback = {},
		solver_options selected_options = {},
		solver_statistics* selected_statistics = nullptr) :
		target(target_value),
		first_only(first),
		collect_solutions(collect),
		options(selected_options),
		statistics(selected_statistics),
		on_solution(std::move(callback)),
		circuit(std::move(root)),
		state(circuit, selected_statistics),
		gates(circuit),
		affine(circuit, selected_options)
	{
		if (options.conflict_learning)
			throw std::logic_error(
				"Conflict learning engine is not implemented yet");

		if (statistics)
		{
			statistics->reset();
			statistics->nodes = circuit.nodes.size();
			statistics->variables = circuit.variables.size();
			statistics->affine_atoms = affine.atom_count;
			statistics->affine_enabled = affine.active;
		}
	}

	bool propagate()
	{
		size_t cursor = 0;
		while (true)
		{
			if (!gates.propagate_pending(state, cursor))
				return false;

			const size_t previous_trail_size = state.trail.size();
			if (!affine.propagate(state))
				return false;
			if (state.trail.size() == previous_trail_size)
				return true;
		}
	}

	bool assign(node_id id, bool value)
	{
		state.begin_assignment();
		if (!state.set_value(id, value) || !propagate())
		{
			if (statistics)
				++statistics->conflicts;
			return false;
		}
		return true;
	}

	void record()
	{
		crs_state solution;
		for (const node_id variable : circuit.variables)
		{
			const int8_t value = state.value_of(variable);
			if (value != -1)
				solution.assignments[circuit.nodes[variable]] =
					value != 0;
		}

		++solution_count;
		if (statistics)
			++statistics->solutions;
		if (on_solution && !on_solution(solution))
			stop_requested = true;

		if (collect_solutions)
			solutions.insert(std::move(solution));
	}

	bool preferred_phase(node_id variable) const
	{
		size_t false_votes = 0;
		size_t true_votes = 0;

		for (const node_id parent : circuit.parents[variable])
		{
			const int8_t output = state.value_of(parent);
			if (output == -1)
				continue;

			switch (circuit.nodes[parent]->operation)
			{
				case '&':
					(output == 1 ? true_votes : false_votes) += 2;
					break;
				case '|':
					(output == 0 ? false_votes : true_votes) += 2;
					break;
				default:
					break;
			}
		}

		return true_votes > false_votes;
	}

	void search()
	{
		if (stop_requested || (first_only && solution_count != 0))
			return;

		node_id selected = no_node;
		for (const node_id variable : circuit.variables)
			if (state.value_of(variable) == -1)
			{
				selected = variable;
				break;
			}

		if (selected == no_node)
		{
			record();
			return;
		}

		const bool first_phase = preferred_phase(selected);
		for (const bool phase : {first_phase, !first_phase})
		{
			if (statistics)
				++statistics->decisions;

			const size_t mark = state.trail.size();
			if (assign(selected, phase))
				search();
			state.undo_to(mark);

			if (stop_requested || (first_only && solution_count != 0))
				return;
		}
	}

	solutions_t run()
	{
		const auto started = std::chrono::steady_clock::now();

		if (assign(circuit.root_id, target))
			search();

		if (statistics)
			statistics->elapsed =
				std::chrono::steady_clock::now() - started;
		return solutions;
	}
};

inline solutions_t resolve(
	bit_tracker& bit,
	bool state,
	bool first_only = false,
	const solver_options& options = {},
	solver_statistics* statistics = nullptr)
{
	engine solver(
		bit.bit_state,
		state,
		first_only,
		true,
		{},
		options,
		statistics);
	return solver.run();
}

inline size_t resolve_stream(
	bit_tracker& bit,
	bool state,
	engine::solution_callback on_solution,
	const solver_options& options = {},
	solver_statistics* statistics = nullptr)
{
	engine solver(
		bit.bit_state,
		state,
		false,
		false,
		std::move(on_solution),
		options,
		statistics);
	(void)solver.run();
	return solver.solution_count;
}

#endif // DIXELU_BITREVERSE_SOLVER_SEARCH_DPLL_H
