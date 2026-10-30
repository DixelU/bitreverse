#ifndef DIXELU_BITREVERSE_SOLVER_SEARCH_DPLL_H
#define DIXELU_BITREVERSE_SOLVER_SEARCH_DPLL_H

#include <cstdint>
#include <vector>

#include "../propagation/affine.h"
#include "../propagation/gates.h"
#include "../solver_state.h"
#include "session.h"

namespace dixelu::bitreverse::collision_resolution::solver_core
{

// Chronological DPLL over the unknown leaves, with native gate propagation
// and optional affine (GF(2)) propagation. Backtracking undoes a trail.
struct dpll_engine : search_session
{
	solver_state state;
	gate_propagator gates;
	affine_propagator affine;

	explicit dpll_engine(search_request request) :
		search_session(std::move(request)),
		state(circuit, statistics, &budget),
		gates(circuit),
		affine(circuit, options)
	{
		begin_statistics(affine.atom_count, affine.active);
	}

	bool propagate()
	{
		budget.tick();
		while (true)
		{
			if (!gates.propagate_pending(state, state.propagation_head))
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

	int8_t value_of(node_id id) const
	{
		return state.value_of(id);
	}

	void search()
	{
		const auto value = [this](node_id id) { return value_of(id); };

		struct decision
		{
			size_t mark, variable_index;
			bool first_phase, second_phase;
		};
		std::vector<decision> stack;
		size_t next_variable = 0;
		while (!stop_requested && !(first_only && solution_count != 0))
		{
			budget.tick();
			// Earlier variables are assigned by ancestors. Resume the ordered
			// scan here instead of re-scanning that prefix at every decision.
			while (next_variable < circuit.variables.size())
			{
				budget.tick();
				if (value_of(circuit.variables[next_variable]) == -1) break;
				++next_variable;
			}
			if (next_variable == circuit.variables.size())
			{
				record(value);
				if (stop_requested || first_only) break;
			}
			else
			{
				const auto selected = circuit.variables[next_variable];
				const bool phase = preferred_phase(selected, value);
				stack.push_back({state.trail.size(), next_variable++, phase, false});
				if (statistics) ++statistics->decisions;
				if (assign(selected, phase)) continue;
			}

			bool resumed = false;
			while (!stack.empty())
			{
				auto& branch = stack.back();
				state.undo_to(branch.mark);
				if (!branch.second_phase)
				{
					branch.second_phase = true;
					next_variable = branch.variable_index + 1;
					if (statistics) ++statistics->decisions;
					if (assign(circuit.variables[branch.variable_index], !branch.first_phase))
					{
						resumed = true;
						break;
					}
					state.undo_to(branch.mark);
				}
				stack.pop_back();
			}
			if (!resumed) break;
		}
		if (!stack.empty()) state.undo_to(stack.front().mark);
	}

	void run()
	{
		const run_timer timer{statistics};
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
	}
};

} // namespace dixelu::bitreverse::collision_resolution::solver_core

#endif // DIXELU_BITREVERSE_SOLVER_SEARCH_DPLL_H
