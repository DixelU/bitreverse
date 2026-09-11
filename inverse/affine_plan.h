#ifndef DIXELU_BITREVERSE_INVERSE_AFFINE_PLAN_H
#define DIXELU_BITREVERSE_INVERSE_AFFINE_PLAN_H

#include "../bitreverse.h"

#include <charconv>
#include <cstddef>
#include <cstdint>
#include <istream>
#include <ostream>
#include <string_view>

namespace dixelu::bitreverse::inversion::detail
{

// An affine circuit is y = A*x + c over GF(2). Compilation stores R = T*A
// in reduced row-echelon form and T*c, so new targets require no elimination.
// Nonpivot input bits are independent parameters, in their original order.
class affine_plan
{
	using word = std::uint64_t;
	static constexpr size_t max_dimension = 1U << 20;
	static constexpr size_t max_storage_bytes = 128U << 20;

	size_t input_count_{};
	size_t output_count_{};
	std::vector<size_t> pivots_;
	std::vector<std::uint8_t> constants_;
	std::vector<word> reduced_;
	std::vector<word> transform_;

	static size_t words_for(size_t bits) { return bits / 64 + (bits % 64 != 0); }

	static bool reserve_budget(size_t& budget, size_t count, size_t bytes)
	{
		if (bytes && count > (max_storage_bytes - budget) / bytes)
			return false;
		budget += count * bytes;
		return true;
	}

	static bool whitespace(int c)
	{
		return c == ' ' || c == '\t' || c == '\n' || c == '\r' ||
			c == '\f' || c == '\v';
	}

	static void skip_space(std::istream& stream)
	{
		while (whitespace(stream.peek()))
			stream.get();
	}

	static void expect(std::istream& stream, std::string_view token)
	{
		skip_space(stream);
		for (const char c : token)
			if (stream.get() != c)
				throw std::runtime_error("Invalid affine plan marker");
		const int next = stream.peek();
		if (next != std::char_traits<char>::eof() && !whitespace(next))
			throw std::runtime_error("Invalid affine plan token");
	}

	static word read_number(std::istream& stream, word limit)
	{
		skip_space(stream);
		word value = 0;
		size_t digits = 0;
		while (stream.peek() >= '0' && stream.peek() <= '9')
		{
			const word digit = static_cast<word>(stream.get() - '0');
			if (++digits > 20 || digit > limit || value > (limit - digit) / 10)
				throw std::runtime_error("Affine plan number exceeds its limit");
			value = value * 10 + digit;
		}
		const int next = stream.peek();
		if (!digits || (next != std::char_traits<char>::eof() && !whitespace(next)))
			throw std::runtime_error("Invalid affine plan number");
		return value;
	}

	static void write_number(std::ostream& stream, word value)
	{
		char buffer[20];
		const auto result = std::to_chars(buffer, buffer + sizeof(buffer), value);
		stream.write(buffer, result.ptr - buffer);
		stream.put(' ');
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
			if (input_count_ % 64 &&
				(reduced_[(row + 1) * input_words - 1] >> (input_count_ % 64)))
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
		const collision_resolution::solver_core::compiled_circuit& circuit,
		const std::vector<size_t>& output_ids,
		const std::vector<size_t>& variable_ids)
	{
		const size_t count = circuit.nodes.size();
		const size_t inputs = variable_ids.size();
		const size_t outputs = output_ids.size();
		if (count > max_dimension || inputs > max_dimension || outputs > max_dimension)
			return std::nullopt;
		if (circuit.inputs.size() != count)
			throw std::invalid_argument("Affine compilation requires a valid circuit");
		const size_t input_words = words_for(inputs);
		const size_t output_words = words_for(outputs);
		struct frame { size_t id; std::uint8_t next; };
		size_t budget = 0;
		if (!reserve_budget(budget, count, input_words * sizeof(word)) ||
			!reserve_budget(budget, count, sizeof(size_t) + 2 + sizeof(frame)) ||
			!reserve_budget(budget, outputs, input_words * sizeof(word)) ||
			!reserve_budget(budget, outputs, output_words * sizeof(word)) ||
			!reserve_budget(budget, outputs, sizeof(std::uint8_t)) ||
			!reserve_budget(budget, std::min(inputs, outputs), sizeof(size_t)))
			return std::nullopt;

		constexpr size_t absent = static_cast<size_t>(-1);
		std::vector<size_t> columns(count, absent);
		for (size_t column = 0; column < inputs; ++column)
		{
			const size_t id = variable_ids[column];
			if (id >= count || !circuit.nodes[id] || circuit.nodes[id]->operation != '*' ||
				columns[id] != absent)
				throw std::invalid_argument("Invalid affine input variable");
			columns[id] = column;
		}
		std::vector<word> forms(count * input_words);
		std::vector<std::uint8_t> constants(count), state(count);
		std::vector<frame> stack;
		stack.reserve(count);
		for (const size_t output : output_ids)
		{
			if (output >= count)
				throw std::invalid_argument("Invalid affine output node");
			if (state[output] == 2)
				continue;
			state[output] = 1;
			stack.push_back({output, 0});
			while (!stack.empty())
			{
				auto& current = stack.back();
				const size_t id = current.id;
				if (!circuit.nodes[id])
					throw std::invalid_argument("Null node in affine circuit");
				const auto operation = circuit.nodes[id]->operation;
				if (operation != '=' && operation != '*' && operation != '!' &&
					operation != '^' && operation != '&' && operation != '|')
					return std::nullopt;
				const size_t arity = dixelu::bitreverse::details::operation_args_count[operation];
				if (current.next < arity)
				{
					const size_t child = circuit.inputs[id][current.next++];
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
				const size_t lhs = circuit.inputs[id][0];
				const size_t rhs = circuit.inputs[id][1];
				auto copy_form = [&](size_t source)
				{
					for (size_t index = 0; index < input_words; ++index)
						forms[id * input_words + index] = forms[source * input_words + index];
					constants[id] = constants[source];
				};
				if (operation == '=')
					constants[id] = circuit.nodes[id]->state;
				else if (operation == '*')
				{
					if (columns[id] == absent)
						throw std::invalid_argument("Affine input variable was not declared");
					forms[id * input_words + columns[id] / 64] |= word{1} << (columns[id] % 64);
				}
				else if (operation == '!')
				{
					copy_form(lhs);
					constants[id] ^= 1;
				}
				else if (operation == '^')
				{
					for (size_t index = 0; index < input_words; ++index)
						forms[id * input_words + index] =
							forms[lhs * input_words + index] ^ forms[rhs * input_words + index];
					constants[id] = constants[lhs] ^ constants[rhs];
				}
				else
				{
					bool lhs_constant = true, rhs_constant = true, same_coefficients = true;
					for (size_t index = 0; index < input_words; ++index)
					{
						const word a = forms[lhs * input_words + index];
						const word b = forms[rhs * input_words + index];
						lhs_constant &= a == 0;
						rhs_constant &= b == 0;
						same_coefficients &= a == b;
					}
					if (lhs_constant || rhs_constant)
					{
						const size_t known = lhs_constant ? lhs : rhs;
						if (constants[known] == (operation == '|'))
							constants[id] = constants[known];
						else
							copy_form(lhs_constant ? rhs : lhs);
					}
					else if (same_coefficients)
					{
						if (constants[lhs] == constants[rhs])
							copy_form(lhs);
						else
							constants[id] = operation == '|';
					}
					else
						return std::nullopt;
				}
				state[id] = 2;
				stack.pop_back();
			}
		}

		affine_plan plan;
		plan.input_count_ = inputs;
		plan.output_count_ = outputs;
		plan.constants_.resize(outputs);
		plan.reduced_.resize(outputs * input_words);
		plan.transform_.resize(outputs * output_words);
		plan.pivots_.reserve(std::min(inputs, outputs));
		for (size_t row = 0; row < outputs; ++row)
		{
			plan.constants_[row] = constants[output_ids[row]];
			for (size_t index = 0; index < input_words; ++index)
				plan.reduced_[row * input_words + index] = forms[output_ids[row] * input_words + index];
			plan.transform_[row * output_words + row / 64] |= word{1} << (row % 64);
		}
		for (size_t column = 0; column < inputs && plan.pivots_.size() < outputs; ++column)
		{
			const size_t rank = plan.pivots_.size();
			const word mask = word{1} << (column % 64);
			size_t selected = rank;
			while (selected < outputs && !(plan.reduced_[selected * input_words + column / 64] & mask))
				++selected;
			if (selected == outputs)
				continue;
			for (size_t index = 0; index < input_words; ++index)
				std::swap(plan.reduced_[rank * input_words + index], plan.reduced_[selected * input_words + index]);
			for (size_t index = 0; index < output_words; ++index)
				std::swap(plan.transform_[rank * output_words + index], plan.transform_[selected * output_words + index]);
			std::swap(plan.constants_[rank], plan.constants_[selected]);
			for (size_t row = 0; row < outputs; ++row)
			{
				if (row == rank || !(plan.reduced_[row * input_words + column / 64] & mask))
					continue;
				for (size_t index = 0; index < input_words; ++index)
					plan.reduced_[row * input_words + index] ^= plan.reduced_[rank * input_words + index];
				for (size_t index = 0; index < output_words; ++index)
					plan.transform_[row * output_words + index] ^= plan.transform_[rank * output_words + index];
				plan.constants_[row] ^= plan.constants_[rank];
			}
			plan.pivots_.push_back(column);
		}
		plan.reduced_.resize(plan.pivots_.size() * input_words);
		return plan;
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
			if (target[bit])
				packed_target[bit / 64] |= word{1} << (bit % 64);
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

	void save(std::ostream& stream) const
	{
		stream << "BITREVERSE_AFFINE 1\n";
		write_number(stream, input_count_);
		write_number(stream, output_count_);
		write_number(stream, pivots_.size());
		stream.put('\n');
		for (const auto value : pivots_) write_number(stream, value);
		stream.put('\n');
		for (const auto value : constants_) write_number(stream, value);
		stream.put('\n');
		for (const auto value : reduced_) write_number(stream, value);
		stream.put('\n');
		for (const auto value : transform_) write_number(stream, value);
		stream << "\nEND_AFFINE\n";
		if (!stream)
			throw std::runtime_error("Could not write affine inverse plan");
	}

	static affine_plan load(std::istream& stream)
	{
		expect(stream, "BITREVERSE_AFFINE");
		if (read_number(stream, std::numeric_limits<word>::max()) != 1)
			throw std::runtime_error("Unsupported affine inverse plan version");
		affine_plan plan;
		plan.input_count_ = static_cast<size_t>(read_number(stream, max_dimension));
		plan.output_count_ = static_cast<size_t>(read_number(stream, max_dimension));
		const size_t rank = static_cast<size_t>(read_number(stream, std::min(plan.input_count_, plan.output_count_)));
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
		for (auto& value : plan.pivots_) value = static_cast<size_t>(read_number(stream, plan.input_count_));
		for (auto& value : plan.constants_) value = static_cast<std::uint8_t>(read_number(stream, 1));
		for (auto& value : plan.reduced_) value = read_number(stream, std::numeric_limits<word>::max());
		for (auto& value : plan.transform_) value = read_number(stream, std::numeric_limits<word>::max());
		expect(stream, "END_AFFINE");
		plan.validate();
		return plan;
	}
};

} // namespace dixelu::bitreverse::inversion::detail

#endif // DIXELU_BITREVERSE_INVERSE_AFFINE_PLAN_H
