#ifndef DIXELU_BITREVERSE_SOLVER_SEARCH_DPLL_H
#define DIXELU_BITREVERSE_SOLVER_SEARCH_DPLL_H

// Internal header: included by bitreverse.h inside
// dixelu::bitreverse::collision_resolution::dpll.

struct engine
{
	using solution_callback = std::function<bool(const crs_state&)>;

	bool first_only;
	bool collect_solutions;
	solver_options options;
	solver_statistics* statistics;
	solver_core::search_budget budget;
	solution_callback on_solution;
	bool stop_requested{false};
	size_t solution_count{0};

	std::shared_ptr<const compiled_circuit> compiled;
	const compiled_circuit& circuit;
	std::vector<std::pair<node_id, bool>> bindings;
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
		engine(
			std::make_shared<const compiled_circuit>(std::move(root)),
			{{0, target_value}},
			first,
			collect,
			std::move(callback),
			selected_options,
			selected_statistics)
	{
	}

	engine(
		std::shared_ptr<const compiled_circuit> selected_circuit,
		std::vector<std::pair<node_id, bool>> selected_bindings,
		bool first,
		bool collect = true,
		solution_callback callback = {},
		solver_options selected_options = {},
		solver_statistics* selected_statistics = nullptr) :
		first_only(first),
		collect_solutions(collect),
		options(selected_options),
		statistics(selected_statistics),
		budget{selected_options.max_search_steps, 0, selected_statistics},
		on_solution(std::move(callback)),
		compiled(std::move(selected_circuit)),
		circuit(solver_core::require_compiled_circuit(compiled)),
		bindings(std::move(selected_bindings)),
		state(circuit, selected_statistics, &budget),
		gates(circuit),
		affine(circuit, selected_options)
	{
		solver_core::validate_bindings(circuit, bindings);
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
		budget.tick();
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
		budget.tick();
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
			budget.tick();
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

	bool preferred_phase(node_id variable)
	{
		size_t false_votes = 0;
		size_t true_votes = 0;

		for (const node_id parent : circuit.parents[variable])
		{
			budget.tick();
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
		budget.tick();

		node_id selected = no_node;
		for (const node_id variable : circuit.variables)
		{
			budget.tick();
			if (state.value_of(variable) == -1)
			{
				selected = variable;
				break;
			}
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
		const solver_core::run_timer timer{statistics};
		budget.tick();

		state.begin_assignment();
		bool consistent = true;
		for (const auto& [id, value] : bindings)
		{
			budget.tick();
			if (!state.set_value(id, value))
			{
				consistent = false;
				break;
			}
		}
		if (consistent)
			consistent = propagate();
		if (consistent)
			search();
		else if (statistics)
			++statistics->conflicts;

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
