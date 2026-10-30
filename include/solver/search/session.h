#ifndef DIXELU_BITREVERSE_SOLVER_SEARCH_SESSION_H
#define DIXELU_BITREVERSE_SOLVER_SEARCH_SESSION_H

#include <cstdint>
#include <memory>
#include <stdexcept>
#include <utility>
#include <vector>

#include "../compiled_circuit.h"
#include "../options.h"
#include "../solution.h"
#include "../solver_state.h"
#include "../statistics.h"

namespace dixelu::bitreverse::collision_resolution::solver_core
{

struct search_request
{
	std::shared_ptr<const compiled_circuit> circuit;
	// Required node values. Nodes without a binding are unconstrained.
	std::vector<std::pair<node_id, bool>> bindings;
	bool first_only{false};
	bool collect_solutions{true};
	solution_callback on_solution{};
	solver_options options{};
	solver_statistics* statistics{nullptr};
};

// The request and result bookkeeping shared by the DPLL and CDCL engines.
// Both engines enumerate complete assignments of circuit.variables.
struct search_session
{
	bool first_only;
	bool collect_solutions;
	solver_options options;
	solver_statistics* statistics;
	search_budget budget;
	solution_callback on_solution;
	bool stop_requested{false};
	size_t solution_count{0};

	std::shared_ptr<const compiled_circuit> compiled;
	const compiled_circuit& circuit;
	std::vector<std::pair<node_id, bool>> bindings;
	solutions_t solutions;

	explicit search_session(search_request request) :
		first_only(request.first_only),
		collect_solutions(request.collect_solutions),
		options(request.options),
		statistics(request.statistics),
		budget{request.options.max_search_steps, 0, request.statistics},
		on_solution(std::move(request.on_solution)),
		compiled(std::move(request.circuit)),
		circuit(require_compiled_circuit(compiled)),
		bindings(std::move(request.bindings))
	{
		validate_bindings(circuit, bindings);
	}

	void begin_statistics(size_t affine_atoms, bool affine_enabled) const
	{
		if (!statistics)
			return;

		statistics->reset();
		statistics->nodes = circuit.nodes.size();
		statistics->variables = circuit.variables.size();
		statistics->affine_atoms = affine_atoms;
		statistics->affine_enabled = affine_enabled;
	}

	// value_of(node) returns -1 (unassigned), 0 or 1.
	template<typename ValueOf>
	void record(ValueOf value_of)
	{
		solution found;
		for (const node_id variable : circuit.variables)
		{
			budget.tick();
			const int8_t value = value_of(variable);
			if (value == -1)
				throw std::logic_error("Search attempted to record an incomplete model");
			found.assignments[circuit.nodes[variable]] = value != 0;
		}

		++solution_count;
		if (statistics)
			++statistics->solutions;
		if (on_solution && !on_solution(found))
			stop_requested = true;
		if (collect_solutions)
			solutions.insert(std::move(found));
	}

	// Pick the phase that agrees with already-assigned AND/OR parents.
	template<typename ValueOf>
	bool preferred_phase(node_id variable, ValueOf value_of)
	{
		size_t false_votes = 0;
		size_t true_votes = 0;

		for (const node_id parent : circuit.parents[variable])
		{
			budget.tick();
			const int8_t output = value_of(parent);
			if (output == -1)
				continue;

			switch (circuit.operations[parent])
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
};

} // namespace dixelu::bitreverse::collision_resolution::solver_core

#endif // DIXELU_BITREVERSE_SOLVER_SEARCH_SESSION_H
