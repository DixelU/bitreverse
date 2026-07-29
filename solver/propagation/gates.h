#ifndef DIXELU_BITREVERSE_SOLVER_PROPAGATION_GATES_H
#define DIXELU_BITREVERSE_SOLVER_PROPAGATION_GATES_H

// Internal header: included by bitreverse.h inside
// dixelu::bitreverse::collision_resolution::solver_core.

struct gate_propagator
{
	const compiled_circuit& circuit;

	explicit gate_propagator(const compiled_circuit& compiled) :
		circuit(compiled) {}

	bool imply(solver_state& state, node_id gate) const
	{
		const std::uint8_t operation = circuit.nodes[gate]->operation;
		if (operation == '=' || operation == '*')
			return true;

		const node_id lhs_id = circuit.inputs[gate][0];
		const node_id rhs_id = circuit.inputs[gate][1];
		int8_t output = state.value_of(gate);
		int8_t lhs = state.value_of(lhs_id);
		int8_t rhs = state.value_of(rhs_id);

		if (operation == '!')
		{
			if (output != -1 && lhs != -1)
				return output == static_cast<int8_t>(!lhs);
			if (output != -1)
				return state.set_value(lhs_id, !output);
			if (lhs != -1)
				return state.set_value(gate, !lhs);
			return true;
		}

		if (operation == '^')
		{
			if (lhs != -1 && rhs != -1)
				return state.set_value(gate, lhs != rhs);
			if (output != -1 && lhs != -1)
				return state.set_value(rhs_id, output != lhs);
			if (output != -1 && rhs != -1)
				return state.set_value(lhs_id, output != rhs);
			return true;
		}

		if (operation == '&')
		{
			if (lhs == 0 || rhs == 0)
			{
				if (!state.set_value(gate, false))
					return false;
			}
			else if (lhs == 1 && rhs == 1)
			{
				if (!state.set_value(gate, true))
					return false;
			}

			output = state.value_of(gate);
			lhs = state.value_of(lhs_id);
			rhs = state.value_of(rhs_id);

			if (output == 1)
				return
					state.set_value(lhs_id, true) &&
					state.set_value(rhs_id, true);
			if (output == 0 && lhs == 1)
				return state.set_value(rhs_id, false);
			if (output == 0 && rhs == 1)
				return state.set_value(lhs_id, false);
			return true;
		}

		if (operation == '|')
		{
			if (lhs == 1 || rhs == 1)
			{
				if (!state.set_value(gate, true))
					return false;
			}
			else if (lhs == 0 && rhs == 0)
			{
				if (!state.set_value(gate, false))
					return false;
			}

			output = state.value_of(gate);
			lhs = state.value_of(lhs_id);
			rhs = state.value_of(rhs_id);

			if (output == 0)
				return
					state.set_value(lhs_id, false) &&
					state.set_value(rhs_id, false);
			if (output == 1 && lhs == 0)
				return state.set_value(rhs_id, true);
			if (output == 1 && rhs == 0)
				return state.set_value(lhs_id, true);
			return true;
		}

		throw std::logic_error("Unknown gate operation");
	}

	bool propagate_pending(
		solver_state& state,
		size_t& cursor) const
	{
		while (cursor < state.propagation_queue.size())
		{
			const node_id id = state.propagation_queue[cursor++];
			if (state.statistics)
				++state.statistics->propagations;

			if (!imply(state, id))
				return false;

			for (const node_id parent : circuit.parents[id])
				if (!imply(state, parent))
					return false;
		}
		return true;
	}
};

#endif // DIXELU_BITREVERSE_SOLVER_PROPAGATION_GATES_H
