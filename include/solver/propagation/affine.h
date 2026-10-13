#ifndef DIXELU_BITREVERSE_SOLVER_PROPAGATION_AFFINE_H
#define DIXELU_BITREVERSE_SOLVER_PROPAGATION_AFFINE_H

// Internal header: included by bitreverse.h inside
// dixelu::bitreverse::collision_resolution::solver_core.

struct affine_propagator
{
	const compiled_circuit& circuit;

	bool active{false};
	size_t atom_count{0};
	size_t word_count{0};

	std::vector<node_id> atom_nodes;
	std::vector<node_id> node_atom_columns;
	std::vector<std::uint64_t> coefficients;
	std::vector<std::uint8_t> constants;
	std::vector<std::uint8_t> ready;

	// Reused elimination workspace, bounded by the number of atoms rather
	// than the number of assigned gates. Rows use a single contiguous buffer.
	std::vector<std::uint64_t> basis, known_mask, known_values;
	std::vector<node_id> pivot_rows, pivot_columns;
	std::vector<std::uint8_t> right_sides;

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
		const std::uint8_t operation = circuit.operations[id];

		if (node_atom_columns[id] != no_node)
		{
			const node_id column = node_atom_columns[id];
			destination[column / 64] |=
				std::uint64_t{1} << (column % 64);
		}
		else if (operation == '=')
		{
			constants[id] = circuit.nodes[id]->state;
		}
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
		for (node_id id = 0; id < circuit.nodes.size(); ++id)
		{
			const std::uint8_t operation =
				circuit.operations[id];
			if (operation == '^' || operation == '!')
				++affine_gate_count;

			if (operation == '*' ||
				operation == '&' ||
				operation == '|')
				++atom_count;
		}

		if (affine_gate_count == 0 || atom_count == 0 || atom_count > max_atoms)
			return;

		node_atom_columns.assign(circuit.nodes.size(), no_node);
		atom_nodes.reserve(atom_count);
		for (node_id id = 0; id < circuit.nodes.size(); ++id)
		{
			const auto operation = circuit.operations[id];
			if (operation == '*' || operation == '&' || operation == '|')
			{
				node_atom_columns[id] = atom_nodes.size();
				atom_nodes.push_back(id);
			}
		}

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

	bool propagate(solver_state& state)
	{
		if (!active)
			return true;
		if (state.statistics)
			++state.statistics->affine_passes;

		known_mask.assign(word_count, 0);
		known_values.assign(word_count, 0);
		for (size_t column = 0; column < atom_count; ++column)
		{
			state.tick_search();
			const auto value = state.value_of(atom_nodes[column]);
			if (value == -1)
				continue;

			const auto bit = std::uint64_t{1} << (column % 64);
			known_mask[column / 64] |= bit;

			if (value)
				known_values[column / 64] |= bit;
		}

		pivot_columns.clear();
		pivot_rows.assign(atom_count, no_node);
		const auto row_limit = std::min(atom_count, state.trail.size());
		basis.resize((row_limit + 1) * word_count);
		right_sides.resize(row_limit);
		size_t rank = 0;

		for (const node_id id : state.trail)
		{
			state.tick_search();
			if (node_atom_columns[id] != no_node)
				continue;

			auto* equation = basis.data() + rank * word_count;
			bool rhs = static_cast<bool>(state.values[id]) != static_cast<bool>(constants[id]);
			const auto* form = words(id);
			state.tick_search(word_count);

			for (size_t word = 0; word < word_count; ++word)
			{
				rhs ^= (std::popcount(form[word] & known_values[word]) & 1) != 0;
				equation[word] = form[word] & ~known_mask[word];
			}

			// Insert into a row-echelon basis. Redundant equations consume only
			// this scratch row, even when thousands of gates are assigned.
			bool inserted = false;
			for (size_t word = 0; word < word_count && !inserted; ++word)
			{
				state.tick_search();
				while (equation[word])
				{
					state.tick_search();
					const auto column = word * 64 + std::countr_zero(equation[word]);
					const auto pivot = pivot_rows[column];
					if (pivot == no_node)
					{
						pivot_rows[column] = rank;
						pivot_columns.push_back(column);
						right_sides[rank++] = rhs;
						inserted = true;
						break;
					}

					const auto* source = basis.data() + pivot * word_count;
					state.tick_search(word_count - word);

					for (size_t j = word; j < word_count; ++j)
						equation[j] ^= source[j];

					rhs ^= right_sides[pivot] != 0;
				}
			}

			if (!inserted && rhs)
				return false;
		}

		// Reduce pivots from highest column down. Insertion order need not be
		// column order. RREF singleton rows expose exactly the forced atoms.
		for (size_t column = atom_count; column-- != 0;)
		{
			state.tick_search();
			const auto pivot = pivot_rows[column];

			if (pivot == no_node)
				continue;

			const auto word = column / 64;
			const auto bit = std::uint64_t{1} << (column % 64);
			const auto* source = basis.data() + pivot * word_count;

			for (size_t row = 0; row < rank; ++row)
			{
				state.tick_search();
				if (pivot_columns[row] >= column)
					continue;

				auto* destination = basis.data() + row * word_count;
				if (!(destination[word] & bit))
					continue;

				state.tick_search(word_count - word);
				for (size_t j = word; j < word_count; ++j)
					destination[j] ^= source[j];

				right_sides[row] ^= right_sides[pivot];
			}
		}
		for (size_t row = 0; row < rank; ++row)
		{
			size_t set_bits = 0;
			for (size_t word = pivot_columns[row] / 64; word < word_count; ++word)
			{
				state.tick_search();
				set_bits += std::popcount(basis[row * word_count + word]);

				if (set_bits > 1)
					break;
			}

			if (set_bits == 1 && !state.set_value(atom_nodes[pivot_columns[row]], right_sides[row] != 0))
				return false;
		}

		return true;
	}
};

#endif // DIXELU_BITREVERSE_SOLVER_PROPAGATION_AFFINE_H
