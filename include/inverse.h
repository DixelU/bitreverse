#ifndef DIXELU_BITREVERSE_INVERSE_H
#define DIXELU_BITREVERSE_INVERSE_H

#include <array>
#include <chrono>
#include <functional>
#include <istream>
#include <limits>
#include <optional>
#include <ostream>
#include <streambuf>
#include <string_view>
#include <type_traits>
#include <variant>

#include "bitreverse.h"
#include "inverse/affine_plan.h"
#include "inverse/synthesized_plan.h"
#include "inverse/selector_plan.h"
#include "inverse/cegis_plan.h"
#include "inverse/node_interner.h"
#include "inverse/text_io.h"

namespace dixelu::bitreverse::inversion
{

using bits = std::vector<bit_tracker>;
using values = std::vector<bool>;

// Integer bits are least-significant first; vectors concatenate their integers.
template<size_t N>
bits bits_of(const int_tracker<N>& value)
{
	return bits(value.bits.rbegin(), value.bits.rend());
}

template<size_t N>
bits bits_of(const std::vector<int_tracker<N>>& value)
{
	bits result;
	result.reserve(value.size() * N);
	for (const auto& item : value)
		result.insert(result.end(), item.bits.rbegin(), item.bits.rend());
	return result;
}

// A fixed, specialized relation: outputs = algorithm(inputs), requirements = true.
// Known algorithm parameters must be concrete before constructing the expression.
class program
{
	using core = collision_resolution::solver_core::compiled_circuit;
	using pointer = counted_ptr<dixelu::bitreverse::details::bitstate>;
	using node = dixelu::bitreverse::details::bitstate;
	using reader = detail::text_reader<std::invalid_argument>;
	// At most one direct inverse is active; monostate answers queries by search.
	using backend = std::variant<std::monostate, detail::affine_plan,
		detail::synthesized_plan, detail::selector_plan, detail::cegis_plan>;
	// Saved backend markers, in variant order.
	static constexpr std::array<std::string_view, std::variant_size_v<backend>> backend_names{
		"SEARCH", "AFFINE", "SYNTHESIZED", "SELECTOR", "LEARNED"};
	static constexpr size_t none = collision_resolution::solver_core::no_node;

	std::shared_ptr<const core> circuit_;
	std::vector<size_t> inputs_, outputs_, requirements_, variables_;
	std::vector<size_t> input_columns_;
	backend backend_;

	program() = default;

	static bool valid_operation(char operation)
	{
		return operation == '=' || operation == '*' || operation == '!' ||
			operation == '^' || operation == '&' || operation == '|';
	}

	// Copy and intern the reachable DAG. Canonical children make the existing
	// constant/cancellation/absorption rules useful across duplicate expressions.
	static std::vector<pointer> specialize(const bits& roots)
	{
		std::unordered_map<const node*, pointer> copies;
		std::unordered_map<const node*, unsigned char> color;
		detail::node_interner interned;
		std::vector<pointer> result;
		for (const auto& root : roots)
		{
			if (!root.bit_state)
				throw std::invalid_argument("inverse contains a null bit");
			std::vector<std::pair<pointer, bool>> pending{{root.bit_state, false}};
			while (!pending.empty())
			{
				auto [source, expanded] = std::move(pending.back());
				pending.pop_back();
				if (copies.contains(source.get()))
					continue;
				const char operation = static_cast<char>(source->operation);
				if (!valid_operation(operation))
					throw std::invalid_argument("inverse contains an invalid gate");
				const size_t arity = dixelu::bitreverse::details::operation_args_count[
					static_cast<unsigned char>(operation)];
				if (!expanded)
				{
					if (color[source.get()] == 1)
						throw std::invalid_argument("inverse contains a cycle");
					color[source.get()] = 1;
					pending.emplace_back(source, true);
					if (arity > 1)
					{
						if (!source->_2)
							throw std::invalid_argument("inverse gate is missing an input");
						pending.emplace_back(source->_2, false);
					}
					if (arity > 0)
					{
						if (!source->_1)
							throw std::invalid_argument("inverse gate is missing an input");
						pending.emplace_back(source->_1, false);
					}
					continue;
				}

				pointer lhs = arity > 0 ? copies.at(source->_1.get()) : pointer{};
				pointer rhs = arity > 1 ? copies.at(source->_2.get()) : pointer{};
				if (arity == 2 && interned.identity(lhs) > interned.identity(rhs))
					std::swap(lhs, rhs);
				pointer copy = operation == '='
					? dixelu::bitreverse::details::make_boolean_constant(source->state)
					: dixelu::bitreverse::details::make_bitstate_operation(operation, lhs, rhs);
				if (auto existing = interned.find(copy))
					copy = std::move(existing);
				else
					interned.insert(copy);
				copies.emplace(source.get(), std::move(copy));
				color[source.get()] = 2;
			}
			result.push_back(copies.at(root.bit_state.get()));
		}
		return result;
	}

	void set_ports(const std::vector<pointer>& roots,
		size_t input_size, size_t output_size)
	{
		circuit_ = std::make_shared<core>(roots);
		for (size_t i = 0; i < roots.size(); ++i)
		{
			const size_t id = circuit_->node_ids.at(roots[i].get());
			if (i < input_size)
				inputs_.push_back(id);
			else if (i < input_size + output_size)
				outputs_.push_back(id);
			else
				requirements_.push_back(id);
		}
		std::unordered_map<size_t, size_t> columns;
		for (const size_t id : inputs_)
		{
			const auto operation = circuit_->nodes[id]->operation;
			if (operation != '*' && operation != '=')
				throw std::invalid_argument("inverse input ports must be constants or unknown leaves");
			if (operation == '*')
			{
				auto [position, inserted] = columns.emplace(id, variables_.size());
				if (inserted)
					variables_.push_back(id);
				input_columns_.push_back(position->second);
			}
			else
				input_columns_.push_back(none);
		}
		for (const size_t id : circuit_->variables)
			if (!columns.contains(id))
				throw std::invalid_argument("inverse has an unknown that is not a declared input");
	}

	values full_input(const values& assignment) const
	{
		values result;
		result.reserve(inputs_.size());
		for (size_t i = 0; i < inputs_.size(); ++i)
			result.push_back(input_columns_[i] == none ?
				static_cast<bool>(circuit_->nodes[inputs_[i]]->state) :
				assignment[input_columns_[i]]);
		return result;
	}

	std::optional<values> complete(const std::optional<values>& assignment) const
	{
		if (!assignment)
			return std::nullopt;
		return full_input(*assignment);
	}

	// The affine plan solves requirements as extra outputs that must be true.
	values affine_target(const values& target) const
	{
		values bound = target;
		bound.insert(bound.end(), requirements_.size(), true);
		return bound;
	}

	void check_target(const values& target) const
	{
		if (target.size() != output_count())
			throw std::invalid_argument("inverse target has the wrong bit width");
	}

	// The active plan, or logic_error naming the backend a query needs.
	template<class Plan>
	const Plan& require() const
	{
		if (const auto* plan = std::get_if<Plan>(&backend_))
			return *plan;
		if constexpr (std::is_same_v<Plan, detail::affine_plan>)
			throw std::logic_error("free-bit parameterization requires a direct affine inverse");
		else if constexpr (std::is_same_v<Plan, detail::synthesized_plan>)
			throw std::logic_error("program has no synthesized inverse");
		else if constexpr (std::is_same_v<Plan, detail::selector_plan>)
			throw std::logic_error("program has no compiled selector");
		else
			throw std::logic_error("program has no learned inverse");
	}

	// A copy of this program that answers queries with plan.
	template<class Plan>
	program with_backend(Plan plan) const
	{
		program result = *this;
		result.backend_ = std::move(plan);
		return result;
	}

	// Enumerates unknown assignments with the active direct backend.
	size_t enumerate(const values& target,
		const std::function<bool(const values&)>& emit, size_t maximum) const
	{
		return std::visit([&]<class Plan>(const Plan& plan) -> size_t
		{
			if constexpr (std::is_same_v<Plan, std::monostate>)
				throw std::logic_error("nonlinear inverse requires search");
			else if constexpr (std::is_same_v<Plan, detail::affine_plan>)
				return plan.enumerate(affine_target(target), emit, maximum);
			else
				return plan.enumerate(target, emit, maximum);
		}, backend_);
	}

	// The node count a direct query reports: the structure it traverses.
	size_t query_node_count() const
	{
		if (const auto* plan = std::get_if<detail::selector_plan>(&backend_))
			return plan->node_count() + plan->forward_node_count();
		if (const auto* plan = std::get_if<detail::synthesized_plan>(&backend_))
			return plan->node_count();
		return node_count();
	}

	size_t search(const values& target, const std::function<bool(const values&)>& callback,
		size_t max_solutions, const solver_options& options, solver_statistics* statistics) const
	{
		std::vector<std::pair<size_t, bool>> bindings;
		for (size_t bit = 0; bit < target.size(); ++bit)
			bindings.emplace_back(outputs_[bit], target[bit]);
		for (const size_t id : requirements_)
			bindings.emplace_back(id, true);
		size_t count = 0;
		return collision_resolution::solve_compiled_stream(circuit_, bindings,
			[&](const collision_resolution::solution& solution)
			{
				values assignment;
				assignment.reserve(variables_.size());
				for (const size_t id : variables_)
					assignment.push_back(solution.assignments.at(circuit_->nodes[id]));
				++count;
				return callback(full_input(assignment)) &&
					(!max_solutions || count < max_solutions);
			}, options, statistics);
	}

public:
	static program compile(const bits& inputs, const bits& outputs,
		const bits& requirements = {})
	{
		bits roots = inputs;
		roots.insert(roots.end(), outputs.begin(), outputs.end());
		roots.insert(roots.end(), requirements.begin(), requirements.end());
		program result;
		result.set_ports(specialize(roots), inputs.size(), outputs.size());
		auto all_outputs = result.outputs_;
		all_outputs.insert(all_outputs.end(), result.requirements_.begin(), result.requirements_.end());
		if (auto affine = detail::affine_plan::compile(*result.circuit_, all_outputs, result.variables_))
			result.backend_ = std::move(*affine);
		return result;
	}

	size_t input_count() const { return inputs_.size(); }
	size_t output_count() const { return outputs_.size(); }
	size_t unknown_count() const { return variables_.size(); }
	size_t node_count() const { return circuit_->nodes.size(); }
	bool is_affine() const { return std::holds_alternative<detail::affine_plan>(backend_); }
	bool is_synthesized() const { return std::holds_alternative<detail::synthesized_plan>(backend_); }
	bool is_selected() const { return std::holds_alternative<detail::selector_plan>(backend_); }
	bool is_learned() const { return std::holds_alternative<detail::cegis_plan>(backend_); }

	// All expensive reasoning happens in these builders, and failure leaves
	// this program intact. Explicit synthesis never falls back to search.
	program synthesized(const synthesis_options& options = {},
		synthesis_statistics* statistics = nullptr) const
	{
		return with_backend(detail::synthesized_plan::compile(
			*circuit_, outputs_, variables_, requirements_, options, statistics));
	}

	// Exhaustively compile the bounded input domain into a selector tree.
	// Targets are checked with one forward evaluation; no image BDD is built.
	program selected(const selector_options& options = {},
		selector_statistics* statistics = nullptr) const
	{
		return with_backend(detail::selector_plan::compile(
			*circuit_, outputs_, variables_, requirements_, options, statistics));
	}

	// Learn a polynomial selector from counterexamples, then certify it with
	// the relational UNSAT miter. No incomplete candidate is returned.
	program learned(const cegis_options& options = {}, cegis_statistics* statistics = nullptr) const
	{
		return with_backend(detail::cegis_plan::compile(
			*circuit_, outputs_, variables_, requirements_, options, statistics));
	}

	size_t synthesized_node_count() const { return require<detail::synthesized_plan>().node_count(); }
	size_t synthesized_relation_node_count() const { return require<detail::synthesized_plan>().relation_node_count(); }
	size_t synthesized_function_node_count() const { return require<detail::synthesized_plan>().function_node_count(); }
	size_t synthesized_selector_node_count() const { return require<detail::synthesized_plan>().selector_node_count(); }
	size_t synthesized_validity_node_count() const { return require<detail::synthesized_plan>().validity_node_count(); }

	size_t selector_node_count() const { return require<detail::selector_plan>().node_count(); }
	size_t selector_leaf_count() const { return require<detail::selector_plan>().leaf_count(); }
	size_t selector_assignment_count() const { return require<detail::selector_plan>().assignment_count(); }
	size_t selector_forward_node_count() const { return require<detail::selector_plan>().forward_node_count(); }

	size_t learned_term_count() const { return require<detail::cegis_plan>().term_count(); }
	size_t learned_coefficient_count() const { return require<detail::cegis_plan>().coefficient_count(); }

	size_t free_bit_count() const { return require<detail::affine_plan>().free_bit_count(); }

	// Writes a standalone header defining bitreverse_inverse(target) for a
	// synthesized, selector, or learned inverse. It needs only the standard
	// library: no tracking, decision-diagram construction, or SAT.
	void export_cpp(std::ostream& stream) const
	{
		values constants;
		constants.reserve(inputs_.size());
		for (const size_t id : inputs_)
			constants.push_back(circuit_->nodes[id]->state != 0);
		const detail::cpp_signature signature{input_columns_, constants, unknown_count(), output_count()};
		std::visit([&]<class Plan>(const Plan& plan)
		{
			if constexpr (requires { plan.export_cpp(stream, signature); })
				plan.export_cpp(stream, signature);
			else
				throw std::logic_error("synthesize the inverse before exporting C++");
		}, backend_);
		if (!stream)
			throw std::runtime_error("could not export C++ inverse");
	}

	std::optional<values> evaluate(const values& target, const values& free_bits) const
	{
		check_target(target);
		return complete(std::visit([&]<class Plan>(const Plan& plan) -> std::optional<values>
		{
			if constexpr (std::is_same_v<Plan, std::monostate>)
				throw std::logic_error("nonlinear inverse requires search");
			else if constexpr (std::is_same_v<Plan, detail::affine_plan>)
				return plan.evaluate(affine_target(target), free_bits);
			else
			{
				if (!free_bits.empty())
					throw std::invalid_argument("nonlinear inverse selects one input and takes no free bits");
				return plan.evaluate(target);
			}
		}, backend_));
	}

	// Selector queries report tree visits and the single forward verification pass.
	std::optional<values> evaluate_selected(const values& target,
		selector_evaluation_statistics* statistics = nullptr) const
	{
		if (statistics) *statistics = {};
		check_target(target);
		return complete(require<detail::selector_plan>().evaluate(target, statistics));
	}

	std::optional<values> evaluate_profiled(const values& target,
		evaluation_statistics* statistics) const
	{
		if (statistics) *statistics = {};
		check_target(target);
		return complete(require<detail::synthesized_plan>().evaluate(target, statistics));
	}

	std::optional<values> evaluate_schedule(const values& target,
		evaluation_statistics* statistics = nullptr) const
	{
		if (statistics) *statistics = {};
		check_target(target);
		return complete(require<detail::synthesized_plan>().evaluate_schedule(target, statistics));
	}

	// Zero max_solutions means exhaustive enumeration. Returning false stops early.
	// An impossible target returns zero, not an exception.
	size_t solve(const values& target,
		const std::function<bool(const values&)>& callback,
		size_t max_solutions = 1, const solver_options& options = {},
		solver_statistics* statistics = nullptr) const
	{
		check_target(target);
		if (!callback)
			throw std::invalid_argument("inverse requires a solution callback");
		if (std::holds_alternative<std::monostate>(backend_))
			return search(target, callback, max_solutions, options, statistics);
		if (is_learned() && max_solutions != 1)
			throw std::logic_error("learned inverse provides one witness; enumeration requires another backend");
		const auto started = std::chrono::steady_clock::now();
		if (statistics)
		{
			statistics->reset();
			statistics->nodes = query_node_count();
			statistics->variables = unknown_count();
			statistics->affine_enabled = is_affine();
		}
		const size_t count = enumerate(target,
			[&](const values& assignment) { return callback(full_input(assignment)); }, max_solutions);
		if (statistics)
		{
			statistics->solutions = count;
			statistics->elapsed = std::chrono::steady_clock::now() - started;
		}
		return count;
	}

private:
	void write_payload(std::ostream& stream) const
	{
		const size_t port_count = input_count() + output_count() + requirements_.size();
		if (node_count() > detail::artifact_node_limit || port_count > detail::artifact_node_limit)
			throw std::length_error("inverse artifact exceeds format limits");
		stream << "BITREVERSE_INVERSE 1\n";
		detail::write_fields(stream, {node_count(), input_count(), output_count(), requirements_.size()});
		stream.put('\n');
		for (size_t id = 0; id < node_count(); ++id)
		{
			const auto& current = circuit_->nodes[id];
			stream.put(static_cast<char>(current->operation));
			stream.put(' ');
			stream << detail::decimal(current->state) << ' ';
			for (const size_t child : circuit_->inputs[id])
				stream << detail::decimal(child == none ? node_count() : child) << ' ';
			stream.put('\n');
		}
		for (const auto* ports : {&inputs_, &outputs_, &requirements_})
		{
			detail::write_fields(stream, *ports);
			stream.put('\n');
		}
		stream << backend_names[backend_.index()] << '\n';
		std::visit([&]<class Plan>(const Plan& plan)
		{
			if constexpr (!std::is_same_v<Plan, std::monostate>)
				plan.save(stream);
		}, backend_);
		if (!stream)
			throw std::runtime_error("could not write inverse artifact");
	}

	std::uint64_t checksum() const
	{
		// Hash the canonical representation without allocating a second archive.
		// This detects accidental damage to either the graph or its cached inverse.
		struct sink : std::streambuf
		{
			std::uint64_t hash = 14695981039346656037ULL;
			int_type overflow(int_type character) override
			{
				if (!traits_type::eq_int_type(character, traits_type::eof()))
				{
					hash ^= static_cast<unsigned char>(traits_type::to_char_type(character));
					hash *= 1099511628211ULL;
				}
				return traits_type::not_eof(character);
			}
			std::streamsize xsputn(const char* data, std::streamsize length) override
			{
				for (std::streamsize i = 0; i < length; ++i)
					overflow(traits_type::to_int_type(data[i]));
				return length;
			}
		} buffer;
		std::ostream stream(&buffer);
		write_payload(stream);
		return buffer.hash;
	}

	struct record
	{
		char operation;
		bool state;
		std::array<size_t, 2> children;
	};

	// Gate records; a child index equal to count marks an absent operand.
	static std::vector<record> read_records(reader& input, size_t count)
	{
		std::vector<record> records;
		records.reserve(count);
		for (size_t id = 0; id < count; ++id)
		{
			const auto operation = input.token();
			if (operation.size() != 1 || !valid_operation(operation[0]))
				throw std::invalid_argument("invalid inverse gate");
			record current{operation[0], input.number(1) != 0, {input.number(count), input.number(count)}};
			const size_t arity = dixelu::bitreverse::details::operation_args_count[
				static_cast<unsigned char>(current.operation)];
			if (current.operation != '=' && current.state)
				throw std::invalid_argument("invalid inverse gate state");
			for (size_t child = 0; child < 2; ++child)
				if ((child < arity) != (current.children[child] < count))
					throw std::invalid_argument("invalid inverse gate arity");
			records.push_back(current);
		}
		return records;
	}

	// Validates acyclicity before connecting reference-counted objects.
	static void check_acyclic(const std::vector<record>& records)
	{
		const size_t count = records.size();
		std::vector<unsigned char> color(count, 0);
		for (size_t root = 0; root < count; ++root)
		{
			std::vector<std::pair<size_t, bool>> pending{{root, false}};
			while (!pending.empty())
			{
				const auto [id, expanded] = pending.back();
				pending.pop_back();
				if (color[id] == 2)
					continue;
				if (expanded)
				{
					color[id] = 2;
					continue;
				}
				if (color[id] == 1)
					throw std::invalid_argument("cyclic inverse artifact");
				color[id] = 1;
				pending.emplace_back(id, true);
				for (const size_t child : records[id].children)
					if (child < count)
						pending.emplace_back(child, false);
			}
		}
	}

	static std::vector<pointer> connect(const std::vector<record>& records)
	{
		std::vector<pointer> nodes;
		for (const auto& current : records)
		{
			auto value = make_counted<node>();
			value->operation = current.operation;
			value->state = current.state;
			nodes.push_back(std::move(value));
		}
		for (size_t id = 0; id < records.size(); ++id)
		{
			if (records[id].children[0] < records.size())
				nodes[id]->_1 = nodes[records[id].children[0]];
			if (records[id].children[1] < records.size())
				nodes[id]->_2 = nodes[records[id].children[1]];
		}
		return nodes;
	}

	static program read_circuit(reader& input)
	{
		input.expect("BITREVERSE_INVERSE");
		if (input.number(detail::artifact_node_limit) != 1)
			throw std::invalid_argument("unsupported inverse artifact version");
		const size_t count = input.number(detail::artifact_node_limit);
		const size_t input_size = input.number(detail::artifact_node_limit);
		const size_t output_size = input.number(detail::artifact_node_limit);
		const size_t requirement_size = input.number(detail::artifact_node_limit);
		const size_t port_count = input_size + output_size + requirement_size;
		if (port_count > detail::artifact_node_limit)
			throw std::invalid_argument("too many inverse ports");
		const auto records = read_records(input, count);
		std::vector<size_t> ports;
		for (size_t i = 0; i < port_count; ++i)
		{
			if (!count)
				throw std::invalid_argument("inverse port has no node");
			ports.push_back(input.number(count - 1));
		}
		check_acyclic(records);
		const auto nodes = connect(records);
		std::vector<pointer> roots;
		for (const size_t id : ports)
			roots.push_back(nodes[id]);
		program result;
		result.set_ports(roots, input_size, output_size);
		if (result.node_count() != count)
			throw std::invalid_argument("inverse artifact contains unreachable nodes");
		return result;
	}

	void read_backend(reader& input, std::istream& stream)
	{
		const auto name = input.token();
		if (name == "LEARNED")
			backend_ = detail::cegis_plan::load(stream, *circuit_, outputs_, variables_, requirements_);
		else if (name == "SELECTOR")
			backend_ = detail::selector_plan::load(stream, *circuit_, outputs_, variables_, requirements_);
		else if (name == "SYNTHESIZED")
		{
			auto plan = detail::synthesized_plan::load(stream);
			if (plan.input_count() != unknown_count() || plan.output_count() != output_count())
				throw std::invalid_argument("synthesized dimensions do not match its ports");
			backend_ = std::move(plan);
		}
		else if (name == "AFFINE")
		{
			auto plan = detail::affine_plan::load(stream);
			if (plan.input_count() != unknown_count() ||
				plan.output_count() != output_count() + requirements_.size())
				throw std::invalid_argument("inverse affine dimensions do not match its ports");
			backend_ = std::move(plan);
		}
		else if (name.empty())
			throw std::invalid_argument("missing inverse backend");
		else if (name != "SEARCH")
			throw std::invalid_argument("unknown inverse backend");
	}

public:
	void save(std::ostream& stream) const
	{
		write_payload(stream);
		stream << "CHECKSUM " << detail::decimal(checksum()) << " \nEND_INVERSE\n";
		if (!stream)
			throw std::runtime_error("could not write inverse artifact");
	}

	static program load(std::istream& stream)
	{
		reader input(stream, "inverse artifact");
		auto result = read_circuit(input);
		result.read_backend(input, stream);
		input.expect("CHECKSUM");
		if (input.number(std::numeric_limits<std::uint64_t>::max()) != result.checksum())
			throw std::invalid_argument("inverse artifact checksum mismatch");
		input.expect("END_INVERSE");
		stream >> std::ws;
		if (!stream.eof())
			throw std::invalid_argument("unexpected trailing inverse data");
		return result;
	}
};

} // namespace dixelu::bitreverse::inversion

#endif // DIXELU_BITREVERSE_INVERSE_H
