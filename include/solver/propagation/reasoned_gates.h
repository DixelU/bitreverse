#ifndef DIXELU_BITREVERSE_SOLVER_PROPAGATION_REASONED_GATES_H
#define DIXELU_BITREVERSE_SOLVER_PROPAGATION_REASONED_GATES_H

// Internal header: included by bitreverse.h inside
// dixelu::bitreverse::collision_resolution::cdcl.
//
// Original circuit clauses are retained as implication/conflict reasons, but
// are not watched. Native gate propagation is substantially cheaper than
// revisiting the equivalent Tseitin clauses. Learned and model-blocking
// clauses continue to use the general watched-literal propagator.

struct reasoned_gate_propagator
{
	const compiled_circuit& circuit;
	clause_database& database;
	std::vector<std::array<clause_id, 4>> reason_clauses;

	reasoned_gate_propagator(
		const compiled_circuit& compiled,
		clause_database& clauses) :
		circuit(compiled),
		database(clauses),
		reason_clauses(
			compiled.nodes.size(),
			{no_clause, no_clause, no_clause, no_clause}) {}

	clause_id add_reason(
		node_id gate,
		size_t index,
		std::initializer_list<literal_t> literals)
	{
		const clause_id id = database.add_clause(
			std::vector<literal_t>(literals),
			false,
			false);
		reason_clauses[gate][index] = id;
		return id;
	}

	void build_reason_clauses()
	{
		for (node_id id = 0; id < circuit.nodes.size(); ++id)
		{
			const literal_t output = make_literal(id);
			const std::uint8_t operation =
				circuit.nodes[id]->operation;

			if (operation == '=' || operation == '*')
				continue;

			const literal_t lhs =
				make_literal(circuit.inputs[id][0]);
			if (operation == '!')
			{
				add_reason(id, 0, {output, lhs});
				add_reason(
					id,
					1,
					{negate_literal(output), negate_literal(lhs)});
				continue;
			}

			const literal_t rhs =
				make_literal(circuit.inputs[id][1]);
			switch (operation)
			{
				case '&':
					add_reason(
						id,
						0,
						{negate_literal(output), lhs});
					add_reason(
						id,
						1,
						{negate_literal(output), rhs});
					add_reason(
						id,
						2,
						{output, negate_literal(lhs),
							negate_literal(rhs)});
					break;

				case '|':
					add_reason(
						id,
						0,
						{output, negate_literal(lhs)});
					add_reason(
						id,
						1,
						{output, negate_literal(rhs)});
					add_reason(
						id,
						2,
						{negate_literal(output), lhs, rhs});
					break;

				case '^':
					add_reason(
						id,
						0,
						{negate_literal(lhs), negate_literal(rhs),
							negate_literal(output)});
					add_reason(
						id,
						1,
						{lhs, rhs, negate_literal(output)});
					add_reason(
						id,
						2,
						{negate_literal(lhs), rhs, output});
					add_reason(
						id,
						3,
						{lhs, negate_literal(rhs), output});
					break;

				default:
					throw std::logic_error(
						"Unsupported operation in CDCL circuit");
			}
		}
	}

	template<typename Enqueue>
	clause_id assign(
		node_id variable,
		bool value,
		clause_id reason,
		Enqueue& enqueue) const
	{
		return enqueue(
			literal_for_value(variable, value),
			reason)
			? no_clause
			: reason;
	}

	template<typename ValueOf, typename Enqueue>
	clause_id imply(
		node_id gate,
		ValueOf& value_of,
		Enqueue& enqueue) const
	{
		const std::uint8_t operation =
			circuit.nodes[gate]->operation;
		if (operation == '=' || operation == '*')
			return no_clause;

		const node_id lhs_id = circuit.inputs[gate][0];
		int8_t output = value_of(gate);
		int8_t lhs = value_of(lhs_id);
		const auto& reasons = reason_clauses[gate];

		if (operation == '!')
		{
			if (output != -1)
				return assign(
					lhs_id,
					!output,
					reasons[output == 1 ? 1 : 0],
					enqueue);
			if (lhs != -1)
				return assign(
					gate,
					!lhs,
					reasons[lhs == 1 ? 1 : 0],
					enqueue);
			return no_clause;
		}

		const node_id rhs_id = circuit.inputs[gate][1];
		int8_t rhs = value_of(rhs_id);

		if (operation == '^')
		{
			if (lhs != -1 && rhs != -1)
			{
				static constexpr size_t output_reason[2][2] =
					{{1, 3}, {2, 0}};
				return assign(
					gate,
					lhs != rhs,
					reasons[output_reason[lhs][rhs]],
					enqueue);
			}
			if (output != -1 && lhs != -1)
			{
				static constexpr size_t rhs_reason[2][2] =
					{{3, 2}, {1, 0}};
				return assign(
					rhs_id,
					output != lhs,
					reasons[rhs_reason[output][lhs]],
					enqueue);
			}
			if (output != -1 && rhs != -1)
			{
				static constexpr size_t lhs_reason[2][2] =
					{{2, 3}, {1, 0}};
				return assign(
					lhs_id,
					output != rhs,
					reasons[lhs_reason[output][rhs]],
					enqueue);
			}
			return no_clause;
		}

		if (operation == '&')
		{
			if (lhs == 0 || rhs == 0)
			{
				const clause_id reason =
					lhs == 0 ? reasons[0] : reasons[1];
				const clause_id conflict =
					assign(gate, false, reason, enqueue);
				if (conflict != no_clause)
					return conflict;
			}
			else if (lhs == 1 && rhs == 1)
			{
				const clause_id conflict =
					assign(gate, true, reasons[2], enqueue);
				if (conflict != no_clause)
					return conflict;
			}

			output = value_of(gate);
			lhs = value_of(lhs_id);
			rhs = value_of(rhs_id);
			if (output == 1)
			{
				clause_id conflict =
					assign(lhs_id, true, reasons[0], enqueue);
				if (conflict != no_clause)
					return conflict;
				return assign(
					rhs_id,
					true,
					reasons[1],
					enqueue);
			}
			if (output == 0 && lhs == 1)
				return assign(
					rhs_id,
					false,
					reasons[2],
					enqueue);
			if (output == 0 && rhs == 1)
				return assign(
					lhs_id,
					false,
					reasons[2],
					enqueue);
			return no_clause;
		}

		if (operation == '|')
		{
			if (lhs == 1 || rhs == 1)
			{
				const clause_id reason =
					lhs == 1 ? reasons[0] : reasons[1];
				const clause_id conflict =
					assign(gate, true, reason, enqueue);
				if (conflict != no_clause)
					return conflict;
			}
			else if (lhs == 0 && rhs == 0)
			{
				const clause_id conflict =
					assign(gate, false, reasons[2], enqueue);
				if (conflict != no_clause)
					return conflict;
			}

			output = value_of(gate);
			lhs = value_of(lhs_id);
			rhs = value_of(rhs_id);
			if (output == 0)
			{
				clause_id conflict =
					assign(lhs_id, false, reasons[0], enqueue);
				if (conflict != no_clause)
					return conflict;
				return assign(
					rhs_id,
					false,
					reasons[1],
					enqueue);
			}
			if (output == 1 && lhs == 0)
				return assign(
					rhs_id,
					true,
					reasons[2],
					enqueue);
			if (output == 1 && rhs == 0)
				return assign(
					lhs_id,
					true,
					reasons[2],
					enqueue);
			return no_clause;
		}

		throw std::logic_error("Unknown gate operation");
	}
};

#endif // DIXELU_BITREVERSE_SOLVER_PROPAGATION_REASONED_GATES_H
