#ifndef DIXELU_BITREVERSE_INVERSE_AFFINE_PLAN_H
#define DIXELU_BITREVERSE_INVERSE_AFFINE_PLAN_H

#include "../bitreverse.h"
#include "text_io.h"

#include <algorithm>
#include <bit>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <istream>
#include <limits>
#include <ostream>
#include <span>

namespace dixelu::bitreverse::inversion::detail
{

// An affine circuit is y = A*x + c over GF(2). Compilation stores R = T*A
// in reduced row-echelon form and T*c, so new targets require no elimination.
// Nonpivot input bits are independent parameters, in their original order.
class affine_plan
{
	using word = std::uint64_t;
	using circuit_type = collision_resolution::solver_core::compiled_circuit;
	static constexpr size_t max_dimension = 1U << 20;
	static constexpr size_t max_storage_bytes = 128U << 20;
	static constexpr size_t absent = static_cast<size_t>(-1);

	struct frame { size_t id; std::uint8_t next; };

	// The value of every node as an affine form: input coefficient words and
	// a constant, one row per node.
	struct linear_forms
	{
		size_t words;
		std::vector<word> coefficients;
		std::vector<std::uint8_t> constants;

		linear_forms(size_t count, size_t inputs)
			: words(words_for(inputs)), coefficients(count * words), constants(count) {}

		std::span<word> row(size_t id) { return {coefficients.data() + id * words, words}; }
		std::span<const word> row(size_t id) const { return {coefficients.data() + id * words, words}; }

		bool is_constant(size_t id) const
		{
			return std::ranges::all_of(row(id), [](word value) { return value == 0; });
		}

		void copy(size_t target, size_t source)
		{
			std::ranges::copy(row(source), row(target).begin());
			constants[target] = constants[source];
		}
	};

	size_t input_count_{};
	size_t output_count_{};

	std::vector<size_t> pivots_;
	std::vector<std::uint8_t> constants_;
	std::vector<word> reduced_;
	std::vector<word> transform_;

	static size_t words_for(size_t bits) { return bits / 64 + (bits % 64 != 0); }

	static bool reserve_budget(size_t& budget, const size_t count, const size_t bytes)
	{
		if (bytes && count > (max_storage_bytes - budget) / bytes)
			return false;

		budget += count * bytes;
		return true;
	}

	static bool within_compile_budget(size_t count, size_t inputs, size_t outputs)
	{
		const size_t input_words = words_for(inputs);
		const size_t output_words = words_for(outputs);
		size_t budget = 0;
		return reserve_budget(budget, count, input_words * sizeof(word)) &&
			reserve_budget(budget, count, sizeof(size_t) + 2 + sizeof(frame)) &&
			reserve_budget(budget, outputs, input_words * sizeof(word)) &&
			reserve_budget(budget, outputs, output_words * sizeof(word)) &&
			reserve_budget(budget, outputs, sizeof(std::uint8_t)) &&
			reserve_budget(budget, std::min(inputs, outputs), sizeof(size_t));
	}

	// Sets the form of node id from its children's forms; false when the gate
	// is not affine. AND and OR are affine only when an operand is constant or
	// both operands share their coefficients.
	static bool combine(const circuit_type& circuit, size_t id, size_t column, linear_forms& forms)
	{
		const auto operation = circuit.nodes[id]->operation;
		const size_t lhs = circuit.inputs[id][0];
		const size_t rhs = circuit.inputs[id][1];
		if (operation == '=')
			forms.constants[id] = circuit.nodes[id]->state;
		else if (operation == '*')
		{
			if (column == absent)
				throw std::invalid_argument("Affine input variable was not declared");

			forms.row(id)[column / 64] |= word{1} << (column % 64);
		}
		else if (operation == '!')
		{
			forms.copy(id, lhs);
			forms.constants[id] ^= 1;
		}
		else if (operation == '^')
		{
			std::ranges::transform(forms.row(lhs), forms.row(rhs), forms.row(id).begin(), std::bit_xor{});
			forms.constants[id] = forms.constants[lhs] ^ forms.constants[rhs];
		}
		else
		{
			const bool lhs_constant = forms.is_constant(lhs), rhs_constant = forms.is_constant(rhs);
			if (lhs_constant || rhs_constant)
			{
				const size_t known = lhs_constant ? lhs : rhs;
				if (forms.constants[known] == (operation == '|'))
					forms.constants[id] = forms.constants[known];
				else
					forms.copy(id, lhs_constant ? rhs : lhs);
			}
			else if (std::ranges::equal(forms.row(lhs), forms.row(rhs)))
			{
				if (forms.constants[lhs] == forms.constants[rhs])
					forms.copy(id, lhs);
				else
					forms.constants[id] = operation == '|';
			}
			else
				return false;
		}
		return true;
	}

	// Computes the forms of every node the outputs reach, children first;
	// false when one of them is not affine.
	static bool linearize(const circuit_type& circuit, const std::vector<size_t>& output_ids,
		const std::vector<size_t>& columns, linear_forms& forms)
	{
		const size_t count = circuit.nodes.size();
		std::vector<std::uint8_t> state(count);
		std::vector<frame> stack;
		stack.reserve(count);

		for (const size_t output : output_ids)
		{
			if (output >= count)
				throw std::invalid_argument("Invalid affine output node");

			if (state[output] == 2)
				continue;

			state[output] = 1;
			stack.push_back({.id = output, .next = 0});
			while (!stack.empty())
			{
				auto& [id, next] = stack.back();
				const auto operation = circuit.nodes[id]->operation;
				if (operation != '=' && operation != '*' && operation != '!' &&
					operation != '^' && operation != '&' && operation != '|')
					return false;

				const size_t arity = details::operation_args_count[operation];
				if (next < arity)
				{
					const size_t child = circuit.inputs[id][next++];

					if (child >= count)
						throw std::invalid_argument("Invalid child in affine circuit");

					if (state[child] == 1)
						throw std::invalid_argument("Cyclic affine circuit");

					if (state[child] == 0)
					{
						state[child] = 1;
						stack.push_back({child, 0});
					}

					continue;
				}

				if (!combine(circuit, id, columns[id], forms))
					return false;
				state[id] = 2;
				stack.pop_back();
			}
		}
		return true;
	}

	bool coefficient(size_t row, size_t column) const
	{
		return (reduced_[row * words_for(input_count_) + column / 64] >> (column % 64)) & 1;
	}

	void swap_rows(size_t a, size_t b)
	{
		const size_t input_words = words_for(input_count_);
		const size_t output_words = words_for(output_count_);
		std::swap_ranges(reduced_.begin() + a * input_words, reduced_.begin() + (a + 1) * input_words,
			reduced_.begin() + b * input_words);
		std::swap_ranges(transform_.begin() + a * output_words, transform_.begin() + (a + 1) * output_words,
			transform_.begin() + b * output_words);
		std::swap(constants_[a], constants_[b]);
	}

	void add_row(size_t row, size_t source)
	{
		const size_t input_words = words_for(input_count_);
		const size_t output_words = words_for(output_count_);
		for (size_t index = 0; index < input_words; ++index)
			reduced_[row * input_words + index] ^= reduced_[source * input_words + index];
		for (size_t index = 0; index < output_words; ++index)
			transform_[row * output_words + index] ^= transform_[source * output_words + index];
		constants_[row] ^= constants_[source];
	}

	// Gauss-Jordan elimination of the output rows. transform_ records the row
	// operations, so applying it to a target replays the elimination.
	static affine_plan reduce(const linear_forms& forms, const std::vector<size_t>& output_ids, size_t inputs)
	{
		const size_t outputs = output_ids.size();
		const size_t input_words = words_for(inputs);
		const size_t output_words = words_for(outputs);

		affine_plan plan;
		plan.input_count_ = inputs;
		plan.output_count_ = outputs;

		plan.constants_.resize(outputs);
		plan.reduced_.resize(outputs * input_words);
		plan.transform_.resize(outputs * output_words);

		plan.pivots_.reserve(std::min(inputs, outputs));

		for (size_t row = 0; row < outputs; ++row)
		{
			plan.constants_[row] = forms.constants[output_ids[row]];
			std::ranges::copy(forms.row(output_ids[row]), plan.reduced_.begin() + row * input_words);
			plan.transform_[row * output_words + row / 64] |= word{1} << (row % 64);
		}
		for (size_t column = 0; column < inputs && plan.pivots_.size() < outputs; ++column)
		{
			const size_t rank = plan.pivots_.size();
			size_t selected = rank;

			while (selected < outputs && !plan.coefficient(selected, column))
				++selected;

			if (selected == outputs)
				continue;

			plan.swap_rows(rank, selected);
			for (size_t row = 0; row < outputs; ++row)
			{
				if (row != rank && plan.coefficient(row, column))
					plan.add_row(row, rank);
			}
			plan.pivots_.push_back(column);
		}
		plan.reduced_.resize(plan.pivots_.size() * input_words);
		return plan;
	}

	void validate() const
	{
		const size_t input_words = words_for(input_count_);
		const size_t output_words = words_for(output_count_);

		std::vector<word> pivot_mask(input_words);
		for (size_t row = 0; row < pivots_.size(); ++row)
		{
			const size_t pivot = pivots_[row];
			if (pivot >= input_count_ || (row && pivots_[row - 1] >= pivot))
				throw std::runtime_error("Invalid affine plan pivot order");

			pivot_mask[pivot / 64] |= word{1} << (pivot % 64);
		}

		for (size_t row = 0; row < pivots_.size(); ++row)
		{
			const size_t pivot = pivots_[row];
			for (size_t index = 0; index < input_words; ++index)
			{
				const word value = reduced_[row * input_words + index];
				const word expected = index == pivot / 64 ? word{1} << (pivot % 64) : 0;
				if ((value & pivot_mask[index]) != expected ||
					(index < pivot / 64 && value != 0) ||
					(index == pivot / 64 && (value & ((word{1} << (pivot % 64)) - 1))))
					throw std::runtime_error("Affine plan equations are not reduced");
			}

			if (input_count_ % 64 && (reduced_[(row + 1) * input_words - 1] >> (input_count_ % 64)))
				throw std::runtime_error("Invalid affine plan coefficient padding");
		}
		for (size_t row = 0; row < output_count_; ++row)
		{
			bool nonzero = false;
			for (size_t index = 0; index < output_words; ++index)
				nonzero |= transform_[row * output_words + index] != 0;

			if (!nonzero || (output_count_ % 64 &&
				(transform_[(row + 1) * output_words - 1] >> (output_count_ % 64))))
				throw std::runtime_error("Invalid affine plan output transform");
		}
	}

public:
	static std::optional<affine_plan> compile(
		const circuit_type& circuit,
		const std::vector<size_t>& output_ids,
		const std::vector<size_t>& variable_ids)
	{
		const size_t count = circuit.nodes.size();
		const size_t inputs = variable_ids.size();
		const size_t outputs = output_ids.size();
		if (count > max_dimension || inputs > max_dimension || outputs > max_dimension)
			return std::nullopt;

		if (!within_compile_budget(count, inputs, outputs))
			return std::nullopt;

		std::vector columns(count, absent);
		for (size_t column = 0; column < inputs; ++column)
		{
			const size_t id = variable_ids[column];
			if (id >= count || circuit.nodes[id]->operation != '*' || columns[id] != absent)
				throw std::invalid_argument("Invalid affine input variable");
			columns[id] = column;
		}

		linear_forms forms(count, inputs);
		if (!linearize(circuit, output_ids, columns, forms))
			return std::nullopt;
		return reduce(forms, output_ids, inputs);
	}

	size_t input_count() const { return input_count_; }
	size_t output_count() const { return output_count_; }
	size_t free_bit_count() const { return input_count_ - pivots_.size(); }

	std::optional<std::vector<bool>> evaluate(
		const std::vector<bool>& target, const std::vector<bool>& free_bits) const
	{
		if (target.size() != output_count_ || free_bits.size() != free_bit_count())
			throw std::invalid_argument("Affine inverse argument dimensions do not match");

		const size_t input_words = words_for(input_count_);
		const size_t output_words = words_for(output_count_);

		std::vector<word> packed_target(output_words), packed_inputs(input_words);

		for (size_t bit = 0; bit < target.size(); ++bit)
		{
			if (target[bit])
				packed_target[bit / 64] |= word{1} << (bit % 64);
		}

		std::vector<bool> result(input_count_);
		size_t pivot = 0, free = 0;

		for (size_t bit = 0; bit < input_count_; ++bit)
		{
			if (pivot < pivots_.size() && pivots_[pivot] == bit)
				++pivot;
			else if (free_bits[free++])
			{
				result[bit] = true;
				packed_inputs[bit / 64] |= word{1} << (bit % 64);
			}
		}

		for (size_t row = 0; row < output_count_; ++row)
		{
			bool value = constants_[row];
			for (size_t index = 0; index < output_words; ++index)
				value ^= (std::popcount(transform_[row * output_words + index] & packed_target[index]) & 1) != 0;

			if (row >= pivots_.size())
			{
				if (value)
					return std::nullopt;
				continue;
			}

			for (size_t index = 0; index < input_words; ++index)
				value ^= (std::popcount(reduced_[row * input_words + index] & packed_inputs[index]) & 1) != 0;

			result[pivots_[row]] = value;
		}

		return result;
	}

	// Visits the solutions for every free-bit assignment, counting the free
	// bits up as a little-endian binary number.
	size_t enumerate(const std::vector<bool>& target,
		const std::function<bool(const std::vector<bool>&)>& callback, size_t maximum) const
	{
		std::vector<bool> free_bits(free_bit_count(), false);
		size_t count = 0;
		while (const auto solution = evaluate(target, free_bits))
		{
			++count;
			if (!callback(*solution) || (maximum && count >= maximum))
				break;
			size_t bit = 0;
			while (bit < free_bits.size() && free_bits[bit])
				free_bits[bit++] = false;
			if (bit == free_bits.size())
				break;
			free_bits[bit] = true;
		}
		return count;
	}

	void save(std::ostream& stream) const
	{
		stream << "BITREVERSE_AFFINE 1\n";
		write_fields(stream, {input_count_, output_count_, pivots_.size()});
		stream.put('\n');
		write_fields(stream, pivots_);
		stream.put('\n');
		write_fields(stream, constants_);
		stream.put('\n');
		write_fields(stream, reduced_);
		stream.put('\n');
		write_fields(stream, transform_);
		stream << "\nEND_AFFINE\n";

		if (!stream)
			throw std::runtime_error("Could not write affine inverse plan");
	}

	static affine_plan load(std::istream& stream)
	{
		text_reader<std::runtime_error> reader(stream, "affine plan");
		reader.expect("BITREVERSE_AFFINE");
		if (reader.number(std::numeric_limits<word>::max()) != 1)
			throw std::runtime_error("Unsupported affine inverse plan version");

		affine_plan plan;
		plan.input_count_ = reader.number(max_dimension);
		plan.output_count_ = reader.number(max_dimension);

		const size_t rank = reader.number(std::min(plan.input_count_, plan.output_count_));
		const size_t input_words = words_for(plan.input_count_);
		const size_t output_words = words_for(plan.output_count_);

		size_t budget = 0;
		if (!reserve_budget(budget, rank, sizeof(size_t)) ||
			!reserve_budget(budget, plan.output_count_, sizeof(std::uint8_t)) ||
			!reserve_budget(budget, rank, input_words * sizeof(word)) ||
			!reserve_budget(budget, plan.output_count_, output_words * sizeof(word)) ||
			!reserve_budget(budget, input_words, sizeof(word)))
			throw std::runtime_error("Affine inverse plan exceeds the storage limit");

		plan.pivots_.resize(rank);
		plan.constants_.resize(plan.output_count_);
		plan.reduced_.resize(rank * input_words);
		plan.transform_.resize(plan.output_count_ * output_words);

		for (auto& value : plan.pivots_) value = reader.number(plan.input_count_);
		for (auto& value : plan.constants_) value = static_cast<std::uint8_t>(reader.number(1));
		for (auto& value : plan.reduced_) value = reader.number(std::numeric_limits<word>::max());
		for (auto& value : plan.transform_) value = reader.number(std::numeric_limits<word>::max());
		reader.expect("END_AFFINE");

		plan.validate();
		return plan;
	}
};

} // namespace dixelu::bitreverse::inversion::detail

#endif // DIXELU_BITREVERSE_INVERSE_AFFINE_PLAN_H
