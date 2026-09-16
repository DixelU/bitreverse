#ifndef DIXELU_BITREVERSE_INVERSE_CEGIS_PLAN_H
#define DIXELU_BITREVERSE_INVERSE_CEGIS_PLAN_H

#include "../bitreverse.h"
#include <charconv>
#include <istream>
#include <ostream>
#include <tuple>

namespace dixelu::bitreverse::inversion
{
struct cegis_options
{
	size_t max_degree = 2;
	size_t max_features = 4096;
	size_t max_counterexamples = 256;
	// Total solver work across all verification queries. Always positive.
	size_t max_solver_steps = 2000000;
	size_t max_nodes = 100000;
};

struct cegis_statistics
{
	size_t features{}, counterexamples{}, solver_calls{}, solver_steps{};
	size_t candidate_terms{}, candidate_coefficients{};
	bool certified{};
	std::chrono::nanoseconds elapsed{}, verification_elapsed{};
	std::string_view failed_phase = "none";
};

class cegis_limit : public std::runtime_error
{
public:
	explicit cegis_limit(const std::string& message) : std::runtime_error(message) {}
};

namespace detail
{
// An experimental, deliberately limited learner: each unknown input is an XOR
// of constants, target bits, and pairwise target products. Only a completed
// UNSAT check of the relational miter permits a plan to escape the builder.
class cegis_plan
{
	using circuit_type = collision_resolution::solver_core::compiled_circuit;
	using pointer = counted_ptr<dixelu::bitreverse::details::bitstate>;
	using bits = std::vector<bit_tracker>;
	using values = std::vector<bool>;
	using words = std::vector<std::uint64_t>;
	static constexpr size_t absent = size_t(-1);
	static constexpr size_t dimension_limit = 256;
	static constexpr size_t feature_limit = 16384;
	static constexpr size_t node_limit = 1000000;
	struct gate { char operation; size_t first{}, second{}; };
	// first == target_bits denotes 1; second == target_bits denotes a linear term.
	using term = std::pair<size_t, size_t>;
	size_t inputs_{}, target_bits_{};
	std::vector<gate> forward_;
	std::vector<size_t> outputs_, requirements_;
	std::vector<term> terms_;
	std::vector<std::vector<size_t>> coefficients_;

	static bool get(const words& row, size_t column)
	{ return ((row[column / 64] >> (column % 64)) & 1U) != 0; }
	static void set(words& row, size_t column)
	{ row[column / 64] |= std::uint64_t{1} << (column % 64); }
	static void add(words& lhs, const words& rhs)
	{ for (size_t i = 0; i < lhs.size(); ++i) lhs[i] ^= rhs[i]; }
	static bool nonzero(const words& row)
	{ return std::any_of(row.begin(), row.end(), [](auto word) { return word != 0; }); }

	void prepare(const circuit_type& circuit, const std::vector<size_t>& output_ids,
		const std::vector<size_t>& variables, const std::vector<size_t>& requirement_ids,
		size_t max_nodes)
	{
		inputs_ = variables.size(); target_bits_ = output_ids.size();
		if (inputs_ > dimension_limit || target_bits_ > dimension_limit ||
			circuit.nodes.size() > max_nodes || requirement_ids.size() > node_limit)
			throw cegis_limit("CEGIS source dimensions or node budget exceeded");
		std::vector<size_t> columns(circuit.nodes.size(), absent), remap(columns);
		for (size_t i = 0; i < variables.size(); ++i) columns.at(variables[i]) = i;
		std::vector<unsigned char> color(columns.size());
		auto root = [&](size_t id)
		{
			std::vector<std::pair<size_t, bool>> stack{{id, false}};
			while (!stack.empty())
			{
				auto [current, expanded] = stack.back(); stack.pop_back();
				if (color.at(current) == 2) continue;
				const auto& source = circuit.nodes[current];
				const char op = static_cast<char>(source->operation);
				const size_t arity = dixelu::bitreverse::details::operation_args_count[
					static_cast<unsigned char>(op)];
				if (!expanded)
				{
					if (color[current]) throw std::invalid_argument("Cyclic CEGIS source");
					color[current] = 1; stack.emplace_back(current, true);
					for (size_t i = arity; i > 0; --i)
						stack.emplace_back(circuit.inputs[current][i - 1], false);
					continue;
				}
				gate step{op};
				if (op == '=') step.first = source->state != 0;
				else if (op == '*')
				{
					step.first = columns[current];
					if (step.first == absent) throw std::invalid_argument("Undeclared CEGIS input");
				}
				else
				{
					step.first = remap.at(circuit.inputs[current][0]);
					if (arity == 2) step.second = remap.at(circuit.inputs[current][1]);
				}
				remap[current] = forward_.size(); forward_.push_back(step); color[current] = 2;
			}
			return remap[id];
		};
		for (size_t id : output_ids) outputs_.push_back(root(id));
		for (size_t id : requirement_ids) requirements_.push_back(root(id));
	}

	// Intern both forward copies and candidate terms together, preserving the
	// identity of repeated operands for simplification and affine propagation.
	struct symbolic
	{
		size_t limit;
		std::map<std::tuple<char, bool, const void*, const void*>, pointer> cache;
		std::vector<pointer> retained;
		std::unordered_map<const void*, size_t> depths;
		std::unordered_map<const void*, size_t> identities;
		bit_tracker intern(pointer result)
		{
			if (result->operation == '*') return bit_tracker(std::move(result));
			const auto key = std::make_tuple(static_cast<char>(result->operation),
				static_cast<bool>(result->state), static_cast<const void*>(result->_1.get()),
				static_cast<const void*>(result->_2.get()));
			if (auto found = cache.find(key); found != cache.end()) return bit_tracker(pointer(found->second));
			if (cache.size() + retained.size() >= limit) throw cegis_limit("CEGIS miter node budget exceeded");
			const size_t depth = result->_1 ? 1 + std::max(depths.at(result->_1.get()),
				result->_2 ? depths.at(result->_2.get()) : size_t{0}) : 0;
			// Affine preprocessing currently traverses expressions recursively.
			if (depth > 512) throw cegis_limit("CEGIS miter depth exceeds the 512-gate safety cap");
			identities.emplace(result.get(), identities.size());
			depths.emplace(result.get(), depth);
			cache.emplace(key, result); return bit_tracker(std::move(result));
		}
		bit_tracker constant(bool value)
		{ return intern(dixelu::bitreverse::details::make_boolean_constant(value)); }
		bit_tracker variable()
		{
			if (cache.size() + retained.size() >= limit) throw cegis_limit("CEGIS miter node budget exceeded");
			auto result = dixelu::bitreverse::details::make_bitstate_operation('*');
			identities.emplace(result.get(), identities.size());
			depths.emplace(result.get(), 0);
			retained.push_back(result); return bit_tracker(std::move(result));
		}
		bit_tracker op(char operation, const bit_tracker& lhs, const bit_tracker& rhs = {})
		{
			if (operation == '^')
			{
				// Normalize associative XOR and complemented terms. In particular,
				// (x XOR h) XOR h reduces to x before asking SAT to prove it.
				// AND/OR atoms remain shared; this never distributes products.
				std::vector<pointer> pending{lhs.bit_state, rhs.bit_state};
				std::map<size_t, pointer> atoms;
				bool parity = false;
				size_t visits = 0;
				while (!pending.empty())
				{
					if (++visits > limit) throw cegis_limit("CEGIS XOR normalization budget exceeded");
					auto current = std::move(pending.back()); pending.pop_back();
					if (current->operation == '=') parity ^= current->state != 0;
					else if (current->operation == '^')
					{ pending.push_back(current->_1); pending.push_back(current->_2); }
					else if (current->operation == '!')
					{ parity = !parity; pending.push_back(current->_1); }
					else
					{
						const size_t identity = identities.at(current.get());
						if (!atoms.erase(identity)) atoms.emplace(identity, std::move(current));
					}
				}
				auto value = constant(false);
				for (const auto& [key, atom] : atoms)
				{
					(void)key;
					value = intern(dixelu::bitreverse::details::make_bitstate_operation('^', value.bit_state, atom));
				}
				return parity ? intern(dixelu::bitreverse::details::make_bitstate_operation('!', value.bit_state)) : value;
			}
			pointer a = lhs.bit_state, b = operation == '!' ? pointer{} : rhs.bit_state;
			if (b && identities.at(b.get()) < identities.at(a.get())) std::swap(a, b);
			return intern(dixelu::bitreverse::details::make_bitstate_operation(operation, a, b));
		}
	};

	bits forward_symbolic(const bits& input, symbolic& builder) const
	{
		bits nodes; nodes.reserve(forward_.size());
		for (const auto step : forward_)
		{
			if (step.operation == '=') nodes.push_back(builder.constant(step.first != 0));
			else if (step.operation == '*') nodes.push_back(input[step.first]);
			else nodes.push_back(builder.op(step.operation, nodes[step.first],
				step.operation == '!' ? bit_tracker{} : nodes[step.second]));
		}
		return nodes;
	}

	values forward_values(const values& input) const
	{
		values nodes; nodes.reserve(forward_.size());
		for (const auto step : forward_)
		{
			bool value = false;
			switch (step.operation)
			{
			case '=': value = step.first != 0; break;
			case '*': value = input[step.first]; break;
			case '!': value = !nodes[step.first]; break;
			case '&': value = nodes[step.first] && nodes[step.second]; break;
			case '|': value = nodes[step.first] || nodes[step.second]; break;
			default: value = nodes[step.first] != nodes[step.second]; break;
			}
			nodes.push_back(value);
		}
		return nodes;
	}

	bits candidate_symbolic(const bits& target, symbolic& builder) const
	{
		bits terms;
		for (auto [first, second] : terms_)
			terms.push_back(first == target_bits_ ? builder.constant(true) : second == target_bits_ ?
				target[first] : builder.op('&', target[first], target[second]));
		bits result;
		for (const auto& coefficients : coefficients_)
		{
			auto value = builder.constant(false);
			for (size_t term_id : coefficients) value = builder.op('^', value, terms[term_id]);
			result.push_back(std::move(value));
		}
		return result;
	}

	std::optional<values> counterexample(const cegis_options& options, cegis_statistics& stats) const
	{
		const auto started = std::chrono::steady_clock::now();
		struct timer
		{
			cegis_statistics& stats; std::chrono::steady_clock::time_point started;
			~timer() { stats.verification_elapsed += std::chrono::steady_clock::now() - started; }
		} timing{stats, started};
		symbolic builder{options.max_nodes, {}, {}, {}, {}};
		bits x;
		for (size_t i = 0; i < inputs_; ++i) x.push_back(builder.variable());
		const auto first = forward_symbolic(x, builder);
		bits target; for (size_t root : outputs_) target.push_back(first[root]);
		const auto candidate = candidate_symbolic(target, builder);
		const auto second = forward_symbolic(candidate, builder);
		auto allowed = builder.constant(true), bad = builder.constant(false);
		for (size_t root : requirements_)
		{
			allowed = builder.op('&', allowed, first[root]);
			bad = builder.op('|', bad, builder.op('!', second[root]));
		}
		for (size_t i = 0; i < outputs_.size(); ++i)
			bad = builder.op('|', bad, builder.op('^', second[outputs_[i]], target[i]));
		bad = builder.op('&', allowed, bad);
		// Keep unused x variables explicit so a SAT model supplies complete training inputs.
		std::vector<pointer> roots{bad.bit_state};
		for (const auto& variable : x) roots.push_back(variable.bit_state);
		auto circuit = std::make_shared<const circuit_type>(std::move(roots));
		if (stats.solver_steps >= options.max_solver_steps) throw cegis_limit("CEGIS solver step budget exhausted");
		solver_options solver;
		solver.max_search_steps = options.max_solver_steps - stats.solver_steps;
		solver_statistics query;
		std::optional<values> witness;
		++stats.solver_calls;
		try
		{
			collision_resolution::solve_compiled_stream(circuit, {{circuit->root_id, true}},
				[&](const collision_resolution::crs_state& model)
				{
					values input;
					for (const auto& variable : x) input.push_back(model.assignments.at(variable.bit_state));
					witness = std::move(input); return false;
				}, solver, &query);
		}
		catch (const solver_limit& error)
		{
			stats.solver_steps += query.search_steps;
			throw cegis_limit(error.what());
		}
		stats.solver_steps += query.search_steps;
		return witness; // Empty only after an exhaustive, normally completed UNSAT query.
	}

	void compact()
	{
		std::vector<size_t> remap(terms_.size(), absent);
		for (const auto& output : coefficients_) for (size_t id : output) remap[id] = 0;
		std::vector<term> retained;
		for (size_t i = 0; i < terms_.size(); ++i)
			if (remap[i] != absent) { remap[i] = retained.size(); retained.push_back(terms_[i]); }
		for (auto& output : coefficients_) for (auto& id : output) id = remap[id];
		terms_ = std::move(retained);
	}

	static size_t read(std::istream& stream, size_t maximum)
	{
		std::string token;
		// Bound token length as well as its numerical value.
		stream >> std::ws;
		while (stream.peek() >= '0' && stream.peek() <= '9' && token.size() <= 20)
			token.push_back(static_cast<char>(stream.get()));
		size_t value{};
		const auto parsed = std::from_chars(token.data(), token.data() + token.size(), value);
		const int next = stream.peek();
		if (token.empty() || token.size() > 20 || parsed.ec != std::errc{} ||
			parsed.ptr != token.data() + token.size() || value > maximum ||
			(next != std::char_traits<char>::eof() && next != ' ' && next != '\n' &&
			 next != '\r' && next != '\t' && next != '\f' && next != '\v'))
			throw std::invalid_argument("Invalid CEGIS artifact integer");
		return value;
	}

	static void write(std::ostream& stream, size_t value)
	{
		char buffer[32];
		const auto result = std::to_chars(buffer, buffer + sizeof(buffer), value);
		stream.write(buffer, result.ptr - buffer);
	}

public:
	static cegis_plan compile(const circuit_type& circuit, const std::vector<size_t>& outputs,
		const std::vector<size_t>& variables, const std::vector<size_t>& requirements,
		const cegis_options& options, cegis_statistics* statistics)
	{
		cegis_statistics local;
		auto& stats = statistics ? *statistics : local; stats = {};
		const auto started = std::chrono::steady_clock::now();
		struct timer
		{
			cegis_statistics& stats; std::chrono::steady_clock::time_point started;
			~timer() { stats.elapsed = std::chrono::steady_clock::now() - started; }
		} timing{stats, started};
		stats.failed_phase = "preparation";
		if ((options.max_degree != 1 && options.max_degree != 2) || !options.max_features ||
			!options.max_counterexamples || !options.max_solver_steps || !options.max_nodes)
			throw std::invalid_argument("CEGIS requires degree 1 or 2 and positive resource budgets");
		if (options.max_features > feature_limit || options.max_nodes > node_limit ||
			options.max_counterexamples > 4096)
			throw cegis_limit("CEGIS options exceed implementation safety caps");
		cegis_plan plan;
		stats.failed_phase = "preparation";
		plan.prepare(circuit, outputs, variables, requirements, options.max_nodes);
		const size_t width = outputs.size();
		std::vector<term> basis{{width, width}};
		for (size_t i = 0; i < width; ++i) basis.emplace_back(i, width);
		if (options.max_degree == 2)
			for (size_t i = 0; i < width; ++i)
				for (size_t j = i + 1; j < width; ++j) basis.emplace_back(i, j);
		stats.features = basis.size();
		if (basis.size() > options.max_features) throw cegis_limit("CEGIS feature budget exceeded");
		struct equation { words features, input; };
		std::map<size_t, equation> rows;
		plan.coefficients_.resize(variables.size());
		while (true)
		{
			stats.failed_phase = "verification";
			stats.candidate_terms = plan.term_count();
			stats.candidate_coefficients = plan.coefficient_count();
			const auto witness = plan.counterexample(options, stats);
			if (!witness)
			{
				stats.certified = true; stats.failed_phase = "none"; return plan;
			}
			stats.failed_phase = "learning";
			if (stats.counterexamples >= options.max_counterexamples)
				throw cegis_limit("CEGIS counterexample budget exceeded");
			++stats.counterexamples;
			const auto nodes = plan.forward_values(*witness);
			values target; for (size_t root : plan.outputs_) target.push_back(nodes[root]);
			for (size_t root : plan.requirements_)
				if (!nodes[root]) throw std::logic_error("CEGIS solver returned a disallowed witness");
			if (plan.evaluate(target)) throw std::logic_error("CEGIS solver returned a spurious counterexample");
			equation row{words((basis.size() + 63) / 64), words((variables.size() + 63) / 64)};
			for (size_t i = 0; i < basis.size(); ++i)
			{
				auto [first, second] = basis[i];
				if (first == width || (target[first] && (second == width || target[second]))) set(row.features, i);
			}
			for (size_t i = 0; i < witness->size(); ++i) if ((*witness)[i]) set(row.input, i);
			size_t pivot = absent;
			for (size_t column = 0; column < basis.size(); ++column)
				if (get(row.features, column))
				{
					const auto existing = rows.find(column);
					if (existing == rows.end()) { pivot = column; break; }
					add(row.features, existing->second.features); add(row.input, existing->second.input);
				}
			if (pivot == absent)
			{
				if (nonzero(row.input)) throw cegis_limit("CEGIS chosen witnesses do not fit the requested polynomial family");
				throw std::logic_error("CEGIS repeated a satisfied training constraint");
			}
			rows.emplace(pivot, std::move(row));
			// Free coefficients remain zero. This is interpolation, not minimum-size
			// optimization, and fixing witnesses can miss smaller relational selectors.
			std::vector<words> solution(basis.size(), words((variables.size() + 63) / 64));
			for (auto iter = rows.rbegin(); iter != rows.rend(); ++iter)
			{
				auto value = iter->second.input;
				for (auto next = rows.upper_bound(iter->first); next != rows.end(); ++next)
					if (get(iter->second.features, next->first)) add(value, solution[next->first]);
				solution[iter->first] = std::move(value);
			}
			plan.terms_ = basis; plan.coefficients_.assign(variables.size(), {});
			for (size_t term_id = 0; term_id < basis.size(); ++term_id)
				for (size_t bit = 0; bit < variables.size(); ++bit)
					if (get(solution[term_id], bit)) plan.coefficients_[bit].push_back(term_id);
			plan.compact();
		}
	}

	size_t term_count() const { return terms_.size(); }
	size_t coefficient_count() const
	{ size_t count = 0; for (const auto& output : coefficients_) count += output.size(); return count; }

	std::optional<values> evaluate(const values& target) const
	{
		if (target.size() != target_bits_) throw std::invalid_argument("CEGIS target has wrong width");
		values terms;
		for (auto [first, second] : terms_)
			terms.push_back(first == target_bits_ || (target[first] && (second == target_bits_ || target[second])));
		values input(inputs_);
		for (size_t bit = 0; bit < inputs_; ++bit)
			for (size_t id : coefficients_[bit]) input[bit] = input[bit] != terms[id];
		const auto nodes = forward_values(input);
		for (size_t root : requirements_) if (!nodes[root]) return std::nullopt;
		for (size_t i = 0; i < outputs_.size(); ++i) if (nodes[outputs_[i]] != target[i]) return std::nullopt;
		return input;
	}

	void save(std::ostream& stream) const
	{
		stream << "CEGIS 1\n"; write(stream, terms_.size()); stream.put('\n');
		for (auto [first, second] : terms_)
		{ write(stream, first); stream.put(' '); write(stream, second); stream.put('\n'); }
		for (const auto& output : coefficients_)
		{
			write(stream, output.size());
			for (size_t id : output) { stream.put(' '); write(stream, id); }
			stream.put('\n');
		}
		stream << "END_CEGIS\n";
	}

	static cegis_plan load(std::istream& stream, const circuit_type& circuit,
		const std::vector<size_t>& outputs, const std::vector<size_t>& variables,
		const std::vector<size_t>& requirements)
	{
		std::string marker; stream >> marker;
		if (marker != "CEGIS" || read(stream, 1) != 1) throw std::invalid_argument("Invalid CEGIS version");
		cegis_plan result; result.prepare(circuit, outputs, variables, requirements, node_limit);
		const size_t count = read(stream, feature_limit), width = outputs.size();
		std::set<term> unique;
		for (size_t i = 0; i < count; ++i)
		{
			const size_t first = read(stream, width), second = read(stream, width);
			if ((first == width && second != width) || (second < width && first >= second) ||
				!unique.emplace(first, second).second) throw std::invalid_argument("Invalid CEGIS term");
			result.terms_.emplace_back(first, second);
		}
		std::vector<bool> used(count);
		for (size_t i = 0; i < variables.size(); ++i)
		{
			const size_t size = read(stream, count);
			std::vector<size_t> coefficients;
			for (size_t j = 0; j < size; ++j)
			{
				const size_t id = read(stream, count - 1);
				if (j && id <= coefficients.back()) throw std::invalid_argument("Invalid CEGIS coefficients");
				coefficients.push_back(id); used[id] = true;
			}
			result.coefficients_.push_back(std::move(coefficients));
		}
		if (std::find(used.begin(), used.end(), false) != used.end()) throw std::invalid_argument("Unused CEGIS term");
		stream >> marker;
		if (marker != "END_CEGIS") throw std::invalid_argument("Invalid CEGIS ending");
		return result;
	}

	void export_cpp(std::ostream& stream, const std::vector<size_t>& columns, const values& constants) const
	{
		stream << "#pragma once\n#include <array>\n#include <optional>\n#include <vector>\n\n"
			<< "inline std::optional<std::array<bool, " << columns.size() << ">>\n"
			<< "bitreverse_inverse(const std::array<bool, " << target_bits_ << ">& target)\n{\n"
			<< "  (void)target;\n  std::array<bool, " << terms_.size() << "> terms{};\n";
		for (size_t i = 0; i < terms_.size(); ++i)
		{
			const auto [first, second] = terms_[i]; stream << "  terms[" << i << "] = ";
			if (first == target_bits_) stream << "true";
			else { stream << "target[" << first << ']'; if (second < target_bits_) stream << " && target[" << second << ']'; }
			stream << ";\n";
		}
		stream << "  std::array<bool, " << inputs_ << "> input{};\n";
		for (size_t i = 0; i < inputs_; ++i)
		{
			stream << "  input[" << i << "] = false";
			for (size_t id : coefficients_[i]) stream << " ^ terms[" << id << ']';
			stream << ";\n";
		}
		stream << "  std::vector<unsigned char> nodes(" << forward_.size() << ");\n";
		for (size_t i = 0; i < forward_.size(); ++i)
		{
			const auto step = forward_[i]; stream << "  nodes[" << i << "] = ";
			if (step.operation == '=') stream << (step.first ? "true" : "false");
			else if (step.operation == '*') stream << "input[" << step.first << ']';
			else
			{
				if (step.operation == '!') stream << '!';
				stream << "nodes[" << step.first << ']';
				if (step.operation != '!') stream << ' ' << step.operation << " nodes[" << step.second << ']';
			}
			stream << ";\n";
		}
		for (size_t root : requirements_) stream << "  if (!nodes[" << root << "]) return std::nullopt;\n";
		for (size_t i = 0; i < outputs_.size(); ++i)
			stream << "  if (bool(nodes[" << outputs_[i] << "]) != target[" << i << "]) return std::nullopt;\n";
		stream << "  std::array<bool, " << columns.size() << "> result{};\n";
		for (size_t i = 0; i < columns.size(); ++i)
		{
			stream << "  result[" << i << "] = ";
			if (columns[i] == absent) stream << (constants[i] ? "true" : "false");
			else stream << "input[" << columns[i] << ']';
			stream << ";\n";
		}
		stream << "  return result;\n}\n";
	}
};
}
}
#endif
