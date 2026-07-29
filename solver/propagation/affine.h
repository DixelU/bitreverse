#ifndef DIXELU_BITREVERSE_SOLVER_PROPAGATION_AFFINE_H
#define DIXELU_BITREVERSE_SOLVER_PROPAGATION_AFFINE_H

// Internal header: included by bitreverse.h inside
// dixelu::bitreverse::collision_resolution::solver_core.

struct affine_propagator
{
	struct row
	{
		std::vector<std::uint64_t> coefficients;
		bool rhs{false};
	};

	const compiled_circuit& circuit;
	bool active{false};
	size_t atom_count{0};
	size_t word_count{0};
	std::vector<node_id> atom_nodes;
	std::vector<node_id> node_atom_columns;
	std::vector<std::uint64_t> coefficients;
	std::vector<std::uint8_t> constants;
	std::vector<std::uint8_t> ready;

	affine_propagator(
		const compiled_circuit& compiled,
		const solver_options& options) :
		circuit(compiled)
	{
		if (options.affine_reasoning)
			initialize(options.max_affine_atoms);
	}

	std::uint64_t* words(node_id id)
	{
		return coefficients.data() + id * word_count;
	}

	const std::uint64_t* words(node_id id) const
	{
		return coefficients.data() + id * word_count;
	}

	void build_form(node_id id)
	{
		if (ready[id] == 1)
			return;
		if (ready[id] == 2)
			throw std::logic_error("Cyclic bit expression");

		ready[id] = 2;
		std::uint64_t* destination = words(id);
		const std::uint8_t operation = circuit.nodes[id]->operation;

		if (node_atom_columns[id] != no_node)
		{
			const node_id column = node_atom_columns[id];
			destination[column / 64] |=
				std::uint64_t{1} << (column % 64);
		}
		else if (operation == '=')
			constants[id] = circuit.nodes[id]->state;
		else if (operation == '!')
		{
			const node_id lhs = circuit.inputs[id][0];
			build_form(lhs);
			std::copy_n(words(lhs), word_count, destination);
			constants[id] = !constants[lhs];
		}
		else if (operation == '^')
		{
			const node_id lhs = circuit.inputs[id][0];
			const node_id rhs = circuit.inputs[id][1];
			build_form(lhs);
			build_form(rhs);

			const std::uint64_t* lhs_words = words(lhs);
			const std::uint64_t* rhs_words = words(rhs);
			for (size_t word = 0; word < word_count; ++word)
				destination[word] = lhs_words[word] ^ rhs_words[word];
			constants[id] = constants[lhs] ^ constants[rhs];
		}
		else
			throw std::logic_error("Unsupported affine expression");

		ready[id] = 1;
	}

	void initialize(size_t max_atoms)
	{
		size_t affine_gate_count = 0;
		node_atom_columns.assign(circuit.nodes.size(), no_node);

		for (node_id id = 0; id < circuit.nodes.size(); ++id)
		{
			const std::uint8_t operation =
				circuit.nodes[id]->operation;
			if (operation == '^' || operation == '!')
				++affine_gate_count;

			if (operation == '*' ||
				operation == '&' ||
				operation == '|')
			{
				node_atom_columns[id] = atom_nodes.size();
				atom_nodes.push_back(id);
			}
		}

		atom_count = atom_nodes.size();
		if (affine_gate_count == 0 ||
			atom_count == 0 ||
			atom_count > max_atoms)
			return;

		word_count = (atom_count + 63) / 64;
		coefficients.assign(
			circuit.nodes.size() * word_count,
			std::uint64_t{0});
		constants.assign(circuit.nodes.size(), 0);
		ready.assign(circuit.nodes.size(), 0);

		for (node_id id = 0; id < circuit.nodes.size(); ++id)
			build_form(id);

		active = true;
	}

	bool propagate(solver_state& state) const
	{
		if (!active)
			return true;
		if (state.statistics)
			++state.statistics->affine_passes;

		std::vector<row> rows;
		rows.reserve(state.trail.size());

		for (const node_id id : state.trail)
		{
			row equation;
			equation.coefficients.assign(
				words(id),
				words(id) + word_count);
			equation.rhs =
				static_cast<bool>(state.values[id]) ^
				static_cast<bool>(constants[id]);
			rows.push_back(std::move(equation));
		}

		size_t rank = 0;
		for (size_t column = 0;
			column < atom_count && rank < rows.size();
			++column)
		{
			const size_t word = column / 64;
			const std::uint64_t bit =
				std::uint64_t{1} << (column % 64);

			size_t pivot = rank;
			while (pivot < rows.size() &&
				!(rows[pivot].coefficients[word] & bit))
				++pivot;
			if (pivot == rows.size())
				continue;

			std::swap(rows[rank], rows[pivot]);
			for (size_t row_index = 0;
				row_index < rows.size();
				++row_index)
			{
				if (row_index == rank ||
					!(rows[row_index].coefficients[word] & bit))
					continue;

				for (size_t current_word = 0;
					current_word < word_count;
					++current_word)
					rows[row_index].coefficients[current_word] ^=
						rows[rank].coefficients[current_word];
				rows[row_index].rhs =
					rows[row_index].rhs != rows[rank].rhs;
			}
			++rank;
		}

		for (const auto& equation : rows)
		{
			size_t set_bits = 0;
			size_t only_column = 0;
			for (size_t word = 0; word < word_count; ++word)
			{
				const size_t word_bits =
					std::popcount(equation.coefficients[word]);
				if (word_bits != 0)
				{
					set_bits += word_bits;
					if (set_bits == 1)
						only_column =
							word * 64 +
							std::countr_zero(
								equation.coefficients[word]);
				}
			}

			if (set_bits == 0)
			{
				if (equation.rhs)
					return false;
				continue;
			}

			if (set_bits == 1 &&
				!state.set_value(
					atom_nodes[only_column],
					equation.rhs))
				return false;
		}

		return true;
	}
};

#endif // DIXELU_BITREVERSE_SOLVER_PROPAGATION_AFFINE_H
