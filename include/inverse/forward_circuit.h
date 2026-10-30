#ifndef DIXELU_BITREVERSE_INVERSE_FORWARD_CIRCUIT_H
#define DIXELU_BITREVERSE_INVERSE_FORWARD_CIRCUIT_H

#include "../bitreverse.h"
#include "text_io.h"

#include <cstdint>
#include <limits>
#include <memory_resource>
#include <stdexcept>
#include <vector>

namespace dixelu::bitreverse::inversion::detail
{

// The specialized circuit as gates in topological order, for direct forward
// evaluation. An unknown gate holds its input column in `first` and a constant
// its value; other operands are gate indices.
struct forward_circuit
{
	using index = std::uint32_t;
	struct gate { char operation; index first{}, second{}; };

	size_t inputs{};
	std::pmr::vector<gate> gates;
	std::pmr::vector<index> outputs, requirements;

	explicit forward_circuit(std::pmr::memory_resource* memory = std::pmr::get_default_resource())
		: gates(memory), outputs(memory), requirements(memory) {}

	// Linearizes the cones of the output and requirement roots. Work is charged
	// to tick(amount): once for the node count, then once per traversal step.
	template<class Tick>
	void build(const collision_resolution::solver_core::compiled_circuit& circuit,
		const std::vector<size_t>& output_ids, const std::vector<size_t>& variable_ids,
		const std::vector<size_t>& requirement_ids, Tick tick)
	{
		constexpr index absent = std::numeric_limits<index>::max();
		const size_t count = circuit.nodes.size();
		if (count > artifact_node_limit || requirement_ids.size() > artifact_node_limit)
			throw std::invalid_argument("Forward circuit exceeds its size limits");
		inputs = variable_ids.size();
		tick(count);
		auto* memory = gates.get_allocator().resource();
		std::pmr::vector<index> columns(count, absent, memory), remap(count, absent, memory);
		std::pmr::vector<unsigned char> state(count, memory);
		for (size_t column = 0; column < variable_ids.size(); ++column)
		{
			const size_t id = variable_ids[column];
			if (id >= count || circuit.nodes[id]->operation != '*' || columns[id] != absent)
				throw std::invalid_argument("Invalid forward circuit unknown");
			columns[id] = static_cast<index>(column);
		}
		struct frame { size_t id, next; };
		std::pmr::vector<frame> stack(memory);
		auto add_root = [&](size_t root)
		{
			if (root >= count)
				throw std::invalid_argument("Invalid forward circuit root");
			if (state[root] == 2)
				return remap[root];
			state[root] = 1;
			stack.push_back({root, 0});
			while (!stack.empty())
			{
				tick(1);
				auto& current = stack.back();
				const size_t id = current.id;
				const char operation = static_cast<char>(circuit.nodes[id]->operation);
				if (operation != '=' && operation != '*' && operation != '!' &&
					operation != '&' && operation != '|' && operation != '^')
					throw std::invalid_argument("Unsupported forward circuit operation");
				const size_t arity = details::operation_args_count[static_cast<unsigned char>(operation)];
				if (current.next < arity)
				{
					const size_t child = circuit.inputs[id][current.next++];
					if (child >= count)
						throw std::invalid_argument("Invalid forward circuit child");
					if (state[child] == 1)
						throw std::invalid_argument("Cyclic forward circuit");
					if (!state[child])
					{
						state[child] = 1;
						stack.push_back({child, 0});
					}
					continue;
				}
				gate step{operation};
				if (operation == '=')
					step.first = circuit.nodes[id]->state != 0;
				else if (operation == '*')
				{
					if (columns[id] == absent)
						throw std::invalid_argument("Undeclared forward circuit unknown");
					step.first = columns[id];
				}
				else
				{
					step.first = remap[circuit.inputs[id][0]];
					if (arity == 2)
						step.second = remap[circuit.inputs[id][1]];
				}
				remap[id] = static_cast<index>(gates.size());
				gates.push_back(step);
				state[id] = 2;
				stack.pop_back();
			}
			return remap[root];
		};
		for (const size_t id : output_ids)
			outputs.push_back(add_root(id));
		for (const size_t id : requirement_ids)
			requirements.push_back(add_root(id));
	}

	// Evaluates every gate in order. True is `ones`, so one word can carry
	// several independent lanes; unknown(column) supplies the input values.
	template<class Values, class Unknown>
	void evaluate(Values& values, typename Values::value_type ones, Unknown unknown) const
	{
		using value = typename Values::value_type;
		for (size_t id = 0; id < gates.size(); ++id)
		{
			const auto step = gates[id];
			switch (step.operation)
			{
			case '=': values[id] = step.first ? ones : value{}; break;
			case '*': values[id] = unknown(step.first); break;
			case '!': values[id] = values[step.first] ^ ones; break;
			case '&': values[id] = values[step.first] & values[step.second]; break;
			case '|': values[id] = values[step.first] | values[step.second]; break;
			default: values[id] = values[step.first] ^ values[step.second]; break;
			}
		}
	}

	// Whether evaluated gate values meet every requirement and produce target.
	template<class Values>
	bool accepts(const Values& values, const std::vector<bool>& target) const
	{
		for (const index root : requirements)
			if (!values[root])
				return false;
		for (size_t bit = 0; bit < outputs.size(); ++bit)
			if ((values[outputs[bit]] != 0) != target[bit])
				return false;
		return true;
	}
};

} // namespace dixelu::bitreverse::inversion::detail

#endif // DIXELU_BITREVERSE_INVERSE_FORWARD_CIRCUIT_H
