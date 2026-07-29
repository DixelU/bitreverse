#ifndef DIXELU_BITREVERSE_SOLVER_SEARCH_CDCL_H
#define DIXELU_BITREVERSE_SOLVER_SEARCH_CDCL_H

// Internal header: included by bitreverse.h inside
// dixelu::bitreverse::collision_resolution::cdcl.

struct conflict_analysis
{
	std::vector<literal_t> learned;
	size_t backtrack_level{0};
};

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
	affine_propagator affine_probe;
	clause_database database;
	reasoned_gate_propagator gates;
	std::vector<int8_t> values;
	std::vector<size_t> levels;
	std::vector<clause_id> reasons;
	std::vector<literal_t> trail;
	std::vector<size_t> trail_limits;
	std::vector<literal_t> decision_literals;
	std::vector<std::uint8_t> decision_second_phases;
	size_t propagation_head{0};
	std::vector<std::uint8_t> seen;
	std::vector<node_id> seen_nodes;
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
		affine_probe(circuit, selected_options),
		database(circuit.nodes.size()),
		gates(circuit, database),
		values(circuit.nodes.size(), -1),
		levels(circuit.nodes.size(), 0),
		reasons(circuit.nodes.size(), no_clause),
		seen(circuit.nodes.size(), 0)
	{
		if (circuit.nodes.size() >
			std::numeric_limits<literal_t>::max() / 2U)
			throw std::length_error(
				"CDCL circuit contains too many nodes");

		if (affine_probe.active)
			throw std::logic_error(
				"CDCL cannot combine with active affine reasoning yet; "
				"disable affine reasoning or lower the affine atom cap");

		database.clauses.reserve(circuit.nodes.size() * 4 + 1);
		database.unit_clauses.reserve(circuit.nodes.size());
		trail.reserve(circuit.nodes.size());
		trail_limits.reserve(circuit.variables.size());
		decision_literals.reserve(circuit.variables.size());
		decision_second_phases.reserve(circuit.variables.size());
		seen_nodes.reserve(circuit.nodes.size());
		build_cnf();

		if (statistics)
		{
			statistics->reset();
			statistics->nodes = circuit.nodes.size();
			statistics->variables = circuit.variables.size();
			statistics->affine_atoms = affine_probe.atom_count;
			statistics->affine_enabled = false;
		}
	}

	size_t current_level() const
	{
		return trail_limits.size();
	}

	int8_t value_of(node_id variable) const
	{
		if (circuit.nodes[variable]->operation == '=')
			return static_cast<int8_t>(
				circuit.nodes[variable]->state);
		return values[variable];
	}

	int8_t literal_value(literal_t literal) const
	{
		const int8_t value =
			value_of(literal_variable(literal));
		if (value == -1)
			return -1;
		return static_cast<int8_t>(
			static_cast<bool>(value) !=
			literal_is_negated(literal));
	}

	bool enqueue(literal_t literal, clause_id reason)
	{
		const node_id variable = literal_variable(literal);
		const int8_t value =
			static_cast<int8_t>(!literal_is_negated(literal));

		const int8_t current = value_of(variable);
		if (current != -1)
			return current == value;

		values[variable] = value;
		levels[variable] = current_level();
		reasons[variable] = reason;
		trail.push_back(literal);
		if (statistics)
			statistics->peak_trail =
				std::max(statistics->peak_trail, trail.size());
		return true;
	}

	void build_cnf()
	{
		gates.build_reason_clauses();
		database.add_clause(
			{literal_for_value(circuit.root_id, target)},
			false,
			false);
	}

	bool initialize_units()
	{
		if (database.contains_empty_clause)
			return false;

		for (const clause_id id : database.unit_clauses)
			if (!enqueue(database.clauses[id].literals[0], id))
				return false;
		return true;
	}

	clause_id propagate()
	{
		auto value = [this](node_id variable)
		{
			return value_of(variable);
		};
		auto assign = [this](
			literal_t literal,
			clause_id reason)
		{
			return enqueue(literal, reason);
		};

		while (propagation_head < trail.size())
		{
			const literal_t assigned = trail[propagation_head++];
			if (statistics)
				++statistics->propagations;

			const literal_t became_false =
				negate_literal(assigned);
			if (database.watched_clause_count != 0)
			{
				auto& watching =
					database.watches[became_false];
				size_t index = 0;

				while (index < watching.size())
				{
					const clause_id id = watching[index];
					clause& current = database.clauses[id];

					size_t false_watch = 0;
					if (current.literals[current.watched[0]] !=
						became_false)
						false_watch = 1;
					const size_t other_watch = 1 - false_watch;
					const literal_t other =
						current.literals[
							current.watched[other_watch]];

					if (literal_value(other) == 1)
					{
						++index;
						continue;
					}

					size_t replacement =
						current.literals.size();
					for (size_t candidate = 0;
						candidate < current.literals.size();
						++candidate)
					{
						if (candidate == current.watched[0] ||
							candidate == current.watched[1])
							continue;
						if (literal_value(
							current.literals[candidate]) != 0)
						{
							replacement = candidate;
							break;
						}
					}

					if (replacement != current.literals.size())
					{
						current.watched[false_watch] =
							replacement;
						database.watches[
							current.literals[replacement]]
								.push_back(id);
						watching[index] = watching.back();
						watching.pop_back();
						continue;
					}

					if (literal_value(other) == 0)
						return id;
					if (!enqueue(other, id))
						return id;
					++index;
				}
			}

			const node_id assigned_node =
				literal_variable(assigned);
			clause_id conflict =
				gates.imply(assigned_node, value, assign);
			if (conflict != no_clause)
				return conflict;
			for (const node_id parent :
				circuit.parents[assigned_node])
			{
				conflict = gates.imply(parent, value, assign);
				if (conflict != no_clause)
					return conflict;
			}
		}

		return no_clause;
	}

	std::optional<conflict_analysis> analyze(clause_id conflict)
	{
		conflict_analysis result;
		result.learned.push_back(0);

		size_t current_level_literals = 0;
		size_t trail_index = trail.size();
		node_id resolved = no_node;
		literal_t pivot = 0;
		clause_id source = conflict;
		size_t analyzed_nodes = 0;

		const auto clear_seen = [this]()
		{
			for (const node_id variable : seen_nodes)
				seen[variable] = 0;
			seen_nodes.clear();
		};

		do
		{
			if (options.max_conflict_analysis_nodes != 0 &&
				analyzed_nodes++ >=
					options.max_conflict_analysis_nodes)
			{
				clear_seen();
				return std::nullopt;
			}

			const clause& reason_clause = database.clauses[source];
			for (const literal_t literal : reason_clause.literals)
			{
				const node_id variable =
					literal_variable(literal);
				if (variable == resolved ||
					seen[variable] ||
					levels[variable] == 0)
					continue;

				seen[variable] = 1;
				seen_nodes.push_back(variable);
				if (levels[variable] == current_level())
					++current_level_literals;
				else
					result.learned.push_back(literal);
			}

			do
			{
				if (trail_index == 0)
					throw std::logic_error(
						"CDCL conflict analysis lost its pivot");
				pivot = trail[--trail_index];
				resolved = literal_variable(pivot);
			}
			while (!seen[resolved]);

			seen[resolved] = 0;
			--current_level_literals;
			if (current_level_literals != 0)
			{
				source = reasons[resolved];
				if (source == no_clause)
					throw std::logic_error(
						"CDCL decision encountered before first UIP");
			}
		}
		while (current_level_literals != 0);

		result.learned[0] = negate_literal(pivot);

		if (result.learned.size() > 1)
		{
			size_t highest = 1;
			for (size_t index = 2;
				index < result.learned.size();
				++index)
				if (levels[literal_variable(
					result.learned[index])] >
					levels[literal_variable(
						result.learned[highest])])
					highest = index;

			std::swap(result.learned[1], result.learned[highest]);
			result.backtrack_level =
				levels[literal_variable(result.learned[1])];
		}

		clear_seen();
		return result;
	}

	void backtrack(size_t target_level)
	{
		if (current_level() <= target_level)
			return;

		const size_t target_trail_size =
			trail_limits[target_level];
		for (size_t index = trail.size();
			index > target_trail_size;
			--index)
		{
			const node_id variable =
				literal_variable(trail[index - 1]);
			values[variable] = -1;
			levels[variable] = 0;
			reasons[variable] = no_clause;
		}

		trail.resize(target_trail_size);
		trail_limits.resize(target_level);
		decision_literals.resize(target_level);
		decision_second_phases.resize(target_level);
		propagation_head = target_trail_size;
	}

	void begin_decision(
		literal_t literal,
		bool second_phase)
	{
		trail_limits.push_back(trail.size());
		decision_literals.push_back(literal);
		decision_second_phases.push_back(
			static_cast<std::uint8_t>(second_phase));
		if (statistics)
			++statistics->decisions;
		if (!enqueue(literal, no_clause))
			throw std::logic_error(
				"CDCL selected an assigned decision variable");
	}

	bool retry_chronologically()
	{
		while (current_level() != 0)
		{
			const size_t failed_level = current_level();
			const literal_t failed_decision =
				decision_literals.back();
			const bool was_second_phase =
				decision_second_phases.back() != 0;
			backtrack(failed_level - 1);

			if (was_second_phase)
				continue;

			const literal_t opposite =
				negate_literal(failed_decision);
			const int8_t opposite_value =
				literal_value(opposite);
			if (opposite_value == 1)
				return true;
			if (opposite_value == 0)
				continue;

			begin_decision(opposite, true);
			return true;
		}
		return false;
	}

	bool preferred_phase(node_id variable) const
	{
		size_t false_votes = 0;
		size_t true_votes = 0;
		for (const node_id parent : circuit.parents[variable])
		{
			const int8_t output = value_of(parent);
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

	node_id select_variable() const
	{
		for (const node_id variable : circuit.variables)
			if (values[variable] == -1)
				return variable;
		return no_node;
	}

	void record()
	{
		crs_state solution;
		for (const node_id variable : circuit.variables)
		{
			if (values[variable] == -1)
				throw std::logic_error(
					"CDCL attempted to record an incomplete model");
			solution.assignments[circuit.nodes[variable]] =
				values[variable] != 0;
		}

		++solution_count;
		if (statistics)
			++statistics->solutions;
		if (on_solution && !on_solution(solution))
			stop_requested = true;
		if (collect_solutions)
			solutions.insert(std::move(solution));
	}

	bool add_blocking_clause()
	{
		std::vector<literal_t> blocking;
		blocking.reserve(circuit.variables.size());
		for (const node_id variable : circuit.variables)
			blocking.push_back(
				literal_for_value(
					variable,
					values[variable] == 0));

		backtrack(0);
		if (blocking.empty())
			return false;

		std::stable_partition(
			blocking.begin(),
			blocking.end(),
			[&](literal_t literal)
			{
				return literal_value(literal) != 0;
			});

		size_t unassigned = 0;
		literal_t unit = 0;
		for (const literal_t literal : blocking)
		{
			const int8_t value = literal_value(literal);
			if (value == 1)
				return true;
			if (value == -1)
			{
				++unassigned;
				unit = literal;
			}
		}

		const clause_id id =
			database.add_clause(std::move(blocking));
		if (unassigned == 0)
			return false;
		if (unassigned == 1 && !enqueue(unit, id))
			return false;
		return true;
	}

	solutions_t run()
	{
		const auto started = std::chrono::steady_clock::now();

		bool consistent = initialize_units();
		if (!consistent && statistics)
			++statistics->conflicts;

		while (consistent && !stop_requested)
		{
			const clause_id conflict = propagate();
			if (conflict != no_clause)
			{
				if (statistics)
					++statistics->conflicts;
				if (current_level() == 0)
					break;

				std::optional<conflict_analysis> learned =
					analyze(conflict);
				if (!learned)
				{
					if (statistics)
						++statistics->conflict_analysis_cutoffs;
					consistent = retry_chronologically();
					continue;
				}

				const size_t previous_level = current_level();
				if (statistics &&
					learned->backtrack_level + 1 < previous_level)
					++statistics->backjumps;
				const bool retain_for_propagation =
					learned->backtrack_level + 1 < previous_level;
				backtrack(learned->backtrack_level);

				const literal_t asserting = learned->learned[0];
				const clause_id learned_id =
					database.add_clause(
						std::move(learned->learned),
						true,
						retain_for_propagation);
				if (statistics)
					++statistics->learned_clauses;
				if (!enqueue(asserting, learned_id))
					throw std::logic_error(
						"CDCL learned a non-asserting clause");
				continue;
			}

			const node_id selected = select_variable();
			if (selected == no_node)
			{
				record();
				if (stop_requested || first_only)
					break;
				consistent = add_blocking_clause();
				continue;
			}

			begin_decision(
				literal_for_value(
					selected,
					preferred_phase(selected)),
				false);
		}

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

#endif // DIXELU_BITREVERSE_SOLVER_SEARCH_CDCL_H
