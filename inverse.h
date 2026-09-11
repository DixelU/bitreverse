#ifndef DIXELU_BITREVERSE_INVERSE_H
#define DIXELU_BITREVERSE_INVERSE_H

#include <charconv>
#include <istream>
#include <ostream>
#include <streambuf>
#include <tuple>

#include "bitreverse.h"
#include "inverse/affine_plan.h"
#include "inverse/synthesized_plan.h"
#include "inverse/selector_plan.h"

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
	static constexpr size_t none = collision_resolution::solver_core::no_node;
	static constexpr size_t file_limit = 1'000'000;

	std::shared_ptr<const core> circuit_;
	std::vector<size_t> inputs_, outputs_, requirements_, variables_;
	std::vector<size_t> input_columns_;
	std::optional<detail::affine_plan> affine_;
	std::optional<detail::synthesized_plan> synthesized_;
	std::optional<detail::selector_plan> selected_;

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
		std::unordered_map<const node*, size_t> canonical_ids;
		using key = std::tuple<char, bool, size_t, size_t>;
		std::map<key, pointer> interned;
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
				if (arity == 2 && canonical_ids.at(lhs.get()) > canonical_ids.at(rhs.get()))
					std::swap(lhs, rhs);
				pointer copy = dixelu::bitreverse::details::make_bitstate_operation(
					static_cast<std::uint8_t>(operation) |
						(operation == '=' && source->state ? 0x80 : 0), lhs, rhs);
				if (!canonical_ids.contains(copy.get()))
				{
					// Unknowns retain identity, even though they share an opcode.
					if (copy->operation != '*')
					{
						const key signature{
							static_cast<char>(copy->operation), static_cast<bool>(copy->state),
							copy->_1 ? canonical_ids.at(copy->_1.get()) : none,
							copy->_2 ? canonical_ids.at(copy->_2.get()) : none};
						auto [entry, inserted] = interned.emplace(signature, copy);
						if (!inserted)
							copy = entry->second;
					}
					if (!canonical_ids.contains(copy.get()))
						canonical_ids.emplace(copy.get(), canonical_ids.size());
				}
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
		auto compiled = std::make_shared<core>(roots);
		circuit_ = compiled;
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

	void check_target(const values& target) const
	{
		if (target.size() != output_count())
			throw std::invalid_argument("inverse target has the wrong bit width");
	}

	static size_t read_size(std::istream& stream, size_t maximum = file_limit)
	{
		stream >> std::ws;
		size_t value = 0;
		size_t digits = 0;
		while (stream.peek() >= '0' && stream.peek() <= '9')
		{
			const size_t digit = static_cast<size_t>(stream.get() - '0');
			if (++digits > 20 || digit > maximum || value > (maximum - digit) / 10)
				throw std::invalid_argument("inverse artifact integer exceeds its limit");
			value = value * 10 + digit;
		}
		const int next = stream.peek();
		if (!digits || (next != std::char_traits<char>::eof() &&
			next != ' ' && next != '\t' && next != '\r' && next != '\n' &&
			next != '\f' && next != '\v'))
			throw std::invalid_argument("invalid inverse artifact integer");
		return value;
	}

	static void write_size(std::ostream& stream, std::uint64_t value)
	{
		char buffer[20];
		const auto written = std::to_chars(buffer, buffer + sizeof(buffer), value);
		stream.write(buffer, written.ptr - buffer);
		stream.put(' ');
	}

	static void expect(std::istream& stream, const char* expected)
	{
		std::string token;
		if (!(stream >> token) || token != expected)
			throw std::invalid_argument("invalid inverse artifact marker");
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
		result.affine_ = detail::affine_plan::compile(
			*result.circuit_, all_outputs, result.variables_);
		return result;
	}

	size_t input_count() const { return inputs_.size(); }
	size_t output_count() const { return outputs_.size(); }
	size_t unknown_count() const { return variables_.size(); }
	size_t node_count() const { return circuit_->nodes.size(); }
	bool is_affine() const { return affine_.has_value(); }
	bool is_synthesized() const { return synthesized_.has_value(); }
	bool is_selected() const { return selected_.has_value(); }

	// All expensive reasoning occurs here. Failure leaves this program intact.
	// Explicit synthesis never silently falls back to query-time search.
	program synthesized(const synthesis_options& options = {},
		synthesis_statistics* statistics = nullptr) const
	{
		auto plan = detail::synthesized_plan::compile(
			*circuit_, outputs_, variables_, requirements_, options, statistics);
		program result = *this;
		result.synthesized_ = std::move(plan);
		result.affine_.reset();
		result.selected_.reset();
		return result;
	}

	// Exhaustively compile the bounded input domain into a selector tree.
	// Targets are checked with one forward evaluation; no image BDD is built.
	program selected(const selector_options& options = {},
		selector_statistics* statistics = nullptr) const
	{
		auto plan = detail::selector_plan::compile(
			*circuit_, outputs_, variables_, requirements_, options, statistics);
		program result = *this;
		result.selected_ = std::move(plan);
		result.affine_.reset();
		result.synthesized_.reset();
		return result;
	}

	size_t selector_node_count() const
	{
		if (!selected_) throw std::logic_error("program has no compiled selector");
		return selected_->node_count();
	}
	size_t selector_leaf_count() const
	{
		if (!selected_) throw std::logic_error("program has no compiled selector");
		return selected_->leaf_count();
	}
	size_t selector_assignment_count() const
	{
		if (!selected_) throw std::logic_error("program has no compiled selector");
		return selected_->assignment_count();
	}
	size_t selector_forward_node_count() const
	{
		if (!selected_) throw std::logic_error("program has no compiled selector");
		return selected_->forward_node_count();
	}

	size_t synthesized_node_count() const
	{
		if (!synthesized_)
			throw std::logic_error("program has no synthesized inverse");
		return synthesized_->node_count();
	}

	size_t synthesized_relation_node_count() const
	{
		if (!synthesized_)
			throw std::logic_error("program has no synthesized inverse");
		return synthesized_->relation_node_count();
	}

	size_t synthesized_function_node_count() const
	{
		if (!synthesized_)
			throw std::logic_error("program has no synthesized inverse");
		return synthesized_->function_node_count();
	}

	size_t synthesized_selector_node_count() const
	{
		if (!synthesized_)
			throw std::logic_error("program has no synthesized inverse");
		return synthesized_->selector_node_count();
	}

	size_t synthesized_validity_node_count() const
	{
		if (!synthesized_)
			throw std::logic_error("program has no synthesized inverse");
		return synthesized_->validity_node_count();
	}

	// Emits only the canonical inverse and its validity check. The generated
	// header has no dependency on tracking, decision-diagram construction, or SAT.
	void export_cpp(std::ostream& stream) const
	{
		if (!synthesized_ && !selected_)
			throw std::logic_error("synthesize the inverse before exporting C++");
		values constants;
		constants.reserve(inputs_.size());
		for (const size_t id : inputs_)
			constants.push_back(circuit_->nodes[id]->state != 0);
		if (selected_) selected_->export_cpp(stream, input_columns_, constants);
		else synthesized_->export_cpp(stream, input_columns_, constants);
	}

	size_t free_bit_count() const
	{
		if (!affine_)
			throw std::logic_error("free-bit parameterization requires a direct affine inverse");
		return affine_->free_bit_count();
	}

	std::optional<values> evaluate(const values& target, const values& free_bits) const
	{
		check_target(target);
		if (selected_)
		{
			if (!free_bits.empty())
				throw std::invalid_argument("selector evaluate selects one input; use solve to enumerate");
			return evaluate_selected(target);
		}
		if (synthesized_)
		{
			if (!free_bits.empty())
				throw std::invalid_argument("synthesized evaluate selects one input; use solve to enumerate");
			const auto assignment = synthesized_->evaluate(target);
			if (!assignment)
				return std::nullopt;
			return full_input(*assignment);
		}
		if (!affine_)
			throw std::logic_error("nonlinear inverse requires search");
		values bound = target;
		bound.insert(bound.end(), requirements_.size(), true);
		auto assignment = affine_->evaluate(bound, free_bits);
		if (!assignment)
			return std::nullopt;
		return full_input(*assignment);
	}

	// Selector queries report tree visits and the single forward verification pass.
	std::optional<values> evaluate_selected(const values& target,
		selector_evaluation_statistics* statistics = nullptr) const
	{
		if (statistics) *statistics = {};
		check_target(target);
		if (!selected_) throw std::logic_error("selector evaluation requires a compiled selector");
		const auto assignment = selected_->evaluate(target, statistics);
		if (!assignment) return std::nullopt;
		return full_input(*assignment);
	}

	std::optional<values> evaluate_profiled(const values& target,
		evaluation_statistics* statistics) const
	{
		if (statistics) *statistics = {};
		check_target(target);
		if (!synthesized_) throw std::logic_error("evaluation profiling requires a synthesized inverse");
		const auto assignment = synthesized_->evaluate(target, statistics);
		if (!assignment) return std::nullopt;
		return full_input(*assignment);
	}

	std::optional<values> evaluate_schedule(const values& target,
		evaluation_statistics* statistics = nullptr) const
	{
		if (statistics) *statistics = {};
		check_target(target);
		if (!synthesized_) throw std::logic_error("schedule evaluation requires a synthesized inverse");
		const auto assignment = synthesized_->evaluate_schedule(target, statistics);
		if (!assignment) return std::nullopt;
		return full_input(*assignment);
	}

	// Zero max_solutions means exhaustive enumeration. Returning false stops early.
	// Unlike assert_equality, an impossible target returns zero, not an exception.
	size_t solve(const values& target,
		const std::function<bool(const values&)>& callback,
		size_t max_solutions = 1, const solver_options& options = {},
		solver_statistics* statistics = nullptr) const
	{
		check_target(target);
		if (!callback)
			throw std::invalid_argument("inverse requires a solution callback");
		if (selected_)
		{
			const auto started = std::chrono::steady_clock::now();
			if (statistics)
			{
				statistics->reset();
				statistics->nodes = selected_->node_count() + selected_->forward_node_count();
				statistics->variables = unknown_count();
			}
			const size_t count = selected_->enumerate(target,
				[&](const values& assignment) { return callback(full_input(assignment)); }, max_solutions);
			if (statistics)
			{
				statistics->solutions = count;
				statistics->elapsed = std::chrono::steady_clock::now() - started;
			}
			return count;
		}
		if (synthesized_)
		{
			const auto started = std::chrono::steady_clock::now();
			if (statistics)
			{
				statistics->reset();
				statistics->nodes = synthesized_->node_count();
				statistics->variables = unknown_count();
			}
			const size_t count = synthesized_->enumerate(target,
				[&](const values& assignment) { return callback(full_input(assignment)); },
				max_solutions);
			if (statistics)
			{
				statistics->solutions = count;
				statistics->elapsed = std::chrono::steady_clock::now() - started;
			}
			return count;
		}
		if (affine_)
		{
			const auto started = std::chrono::steady_clock::now();
			if (statistics)
			{
				statistics->reset();
				statistics->nodes = node_count();
				statistics->variables = unknown_count();
				statistics->affine_enabled = true;
			}
			size_t count = 0;
			values free_bits(free_bit_count(), false);
			while (true)
			{
				const auto solution = evaluate(target, free_bits);
				if (!solution)
					break;
				++count;
				if (!callback(*solution) || (max_solutions && count >= max_solutions))
					break;
				size_t bit = 0;
				while (bit < free_bits.size() && free_bits[bit])
					free_bits[bit++] = false;
				if (bit == free_bits.size())
					break;
				free_bits[bit] = true;
			}
			if (statistics)
			{
				statistics->solutions = count;
				statistics->elapsed = std::chrono::steady_clock::now() - started;
			}
			return count;
		}

		std::vector<std::pair<size_t, bool>> bindings;
		for (size_t bit = 0; bit < target.size(); ++bit)
			bindings.emplace_back(outputs_[bit], target[bit]);
		for (const size_t id : requirements_)
			bindings.emplace_back(id, true);
		size_t count = 0;
		return collision_resolution::solve_compiled_stream(circuit_, bindings,
			[&](const collision_resolution::crs_state& solution)
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

private:
	void write_payload(std::ostream& stream) const
	{
		if (node_count() > file_limit || input_count() > file_limit ||
			output_count() > file_limit || requirements_.size() > file_limit ||
			input_count() + output_count() + requirements_.size() > file_limit)
			throw std::length_error("inverse artifact exceeds format limits");
		stream << "BITREVERSE_INVERSE 1\n";
		for (const size_t count : {node_count(), input_count(), output_count(), requirements_.size()})
			write_size(stream, count);
		stream.put('\n');
		for (size_t id = 0; id < node_count(); ++id)
		{
			const auto& current = circuit_->nodes[id];
			stream.put(static_cast<char>(current->operation));
			stream.put(' ');
			write_size(stream, current->state);
			for (const size_t child : circuit_->inputs[id])
				write_size(stream, child == none ? node_count() : child);
			stream.put('\n');
		}
		for (const auto* ports : {&inputs_, &outputs_, &requirements_})
		{
			for (const size_t id : *ports)
				write_size(stream, id);
			stream.put('\n');
		}
		stream << (selected_ ? "SELECTOR\n" : synthesized_ ? "SYNTHESIZED\n" : affine_ ? "AFFINE\n" : "SEARCH\n");
		if (selected_)
			selected_->save(stream);
		else if (synthesized_)
			synthesized_->save(stream);
		else if (affine_)
			affine_->save(stream);
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

public:
	void save(std::ostream& stream) const
	{
		write_payload(stream);
		stream << "CHECKSUM ";
		write_size(stream, checksum());
		stream << "\nEND_INVERSE\n";
		if (!stream)
			throw std::runtime_error("could not write inverse artifact");
	}

	static program load(std::istream& stream)
	{
		expect(stream, "BITREVERSE_INVERSE");
		if (read_size(stream) != 1)
			throw std::invalid_argument("unsupported inverse artifact version");
		const size_t count = read_size(stream);
		const size_t input_size = read_size(stream);
		const size_t output_size = read_size(stream);
		const size_t requirement_size = read_size(stream);
		const size_t port_count = input_size + output_size + requirement_size;
		if (port_count > file_limit)
			throw std::invalid_argument("too many inverse ports");
		struct record { char operation; bool state; std::array<size_t, 2> children; };
		std::vector<record> records;
		records.reserve(count);
		for (size_t id = 0; id < count; ++id)
		{
			std::string operation;
			if (!(stream >> operation) || operation.size() != 1 || !valid_operation(operation[0]))
				throw std::invalid_argument("invalid inverse gate");
			record current{operation[0], read_size(stream, 1) != 0,
				{read_size(stream, count), read_size(stream, count)}};
			const size_t arity = dixelu::bitreverse::details::operation_args_count[
				static_cast<unsigned char>(current.operation)];
			if (current.operation != '=' && current.state)
				throw std::invalid_argument("invalid inverse gate state");
			for (size_t child = 0; child < 2; ++child)
				if ((child < arity) != (current.children[child] < count))
					throw std::invalid_argument("invalid inverse gate arity");
			records.push_back(current);
		}
		std::vector<size_t> ports;
		for (size_t i = 0; i < port_count; ++i)
		{
			if (!count)
				throw std::invalid_argument("inverse port has no node");
			ports.push_back(read_size(stream, count - 1));
		}

		// Validate acyclicity before connecting reference-counted objects.
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
		std::vector<pointer> nodes;
		for (const auto& current : records)
		{
			auto value = make_counted<node>();
			value->operation = current.operation;
			value->state = current.state;
			nodes.push_back(std::move(value));
		}
		for (size_t id = 0; id < count; ++id)
		{
			if (records[id].children[0] < count)
				nodes[id]->_1 = nodes[records[id].children[0]];
			if (records[id].children[1] < count)
				nodes[id]->_2 = nodes[records[id].children[1]];
		}
		std::vector<pointer> roots;
		for (const size_t id : ports)
			roots.push_back(nodes[id]);
		program result;
		result.set_ports(roots, input_size, output_size);
		if (result.node_count() != count)
			throw std::invalid_argument("inverse artifact contains unreachable nodes");
		std::string backend;
		if (!(stream >> backend))
			throw std::invalid_argument("missing inverse backend");
		if (backend == "SELECTOR")
		{
			result.selected_ = detail::selector_plan::load(stream,
				*result.circuit_, result.outputs_, result.variables_, result.requirements_);
			if (result.selected_->input_count() != result.unknown_count() ||
				result.selected_->output_count() != output_size)
				throw std::invalid_argument("selector dimensions do not match its ports");
		}
		else if (backend == "SYNTHESIZED")
		{
			result.synthesized_ = detail::synthesized_plan::load(stream);
			if (result.synthesized_->input_count() != result.unknown_count() ||
				result.synthesized_->output_count() != output_size)
				throw std::invalid_argument("synthesized dimensions do not match its ports");
		}
		else if (backend == "AFFINE")
		{
			result.affine_ = detail::affine_plan::load(stream);
			if (result.affine_->input_count() != result.unknown_count() ||
				result.affine_->output_count() != output_size + requirement_size)
				throw std::invalid_argument("inverse affine dimensions do not match its ports");
		}
		else if (backend != "SEARCH")
			throw std::invalid_argument("unknown inverse backend");
		expect(stream, "CHECKSUM");
		std::string checksum_token;
		stream >> checksum_token;
		std::uint64_t saved_checksum = 0;
		const auto parsed = std::from_chars(checksum_token.data(),
			checksum_token.data() + checksum_token.size(), saved_checksum);
		if (checksum_token.size() > 20 || parsed.ec != std::errc{} ||
			parsed.ptr != checksum_token.data() + checksum_token.size() ||
			saved_checksum != result.checksum())
			throw std::invalid_argument("inverse artifact checksum mismatch");
		expect(stream, "END_INVERSE");
		stream >> std::ws;
		if (!stream.eof())
			throw std::invalid_argument("unexpected trailing inverse data");
		return result;
	}
};

} // namespace dixelu::bitreverse::inversion

#endif // DIXELU_BITREVERSE_INVERSE_H
