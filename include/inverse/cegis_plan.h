#ifndef DIXELU_BITREVERSE_INVERSE_CEGIS_PLAN_H
#define DIXELU_BITREVERSE_INVERSE_CEGIS_PLAN_H

#include "../bitreverse.h"
#include "forward_circuit.h"
#include "node_interner.h"
#include "text_io.h"

#include <algorithm>
#include <chrono>
#include <functional>
#include <istream>
#include <map>
#include <optional>
#include <ostream>
#include <ranges>
#include <set>
#include <string>
#include <unordered_map>
#include <vector>

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

// Learns each unknown input as an XOR of constants, target bits, and pairwise
// target products. Only a completed UNSAT check of the relational miter lets
// a plan leave compile().
class cegis_plan
{
	using circuit_type = collision_resolution::solver_core::compiled_circuit;
	using pointer = counted_ptr<details::bitstate>;
	using bits = std::vector<bit_tracker>;
	using values = std::vector<bool>;
	using words = std::vector<std::uint64_t>;
	using index = forward_circuit::index;
	static constexpr size_t absent = static_cast<size_t>(-1);
	static constexpr size_t dimension_limit = 256;
	static constexpr size_t feature_limit = 16384;
	static constexpr size_t counterexample_limit = 4096;
	// The solver's affine preprocessing recurses through expressions, so the
	// miter depth is capped to bound its stack.
	static constexpr size_t depth_limit = 512;
	// first == target_bits denotes 1; second == target_bits denotes a linear term.
	using term = std::pair<size_t, size_t>;
	// One training constraint: the basis features of a target and the chosen
	// input bits for it.
	struct equation { words features, input; };

	forward_circuit forward_;
	std::vector<term> terms_;
	std::vector<std::vector<size_t>> coefficients_;

	size_t target_bits() const { return forward_.outputs.size(); }

	static bool get(const words& row, size_t column)
	{
		return ((row[column / 64] >> (column % 64)) & 1U) != 0;
	}

	static void set(words& row, size_t column)
	{
		row[column / 64] |= std::uint64_t{1} << (column % 64);
	}

	static void add(words& lhs, const words& rhs)
	{
		for (size_t i = 0; i < lhs.size(); ++i)
			lhs[i] ^= rhs[i];
	}

	static bool nonzero(const words& row)
	{
		return std::ranges::any_of(row, [](std::uint64_t word) { return word != 0; });
	}

	// Adds this object's lifetime to total, including during unwinding.
	class scoped_timer
	{
		std::chrono::nanoseconds& total_;
		std::chrono::steady_clock::time_point started_ = std::chrono::steady_clock::now();
	public:
		explicit scoped_timer(std::chrono::nanoseconds& total) : total_(total) {}
		~scoped_timer() { total_ += std::chrono::steady_clock::now() - started_; }
	};

	void prepare(const circuit_type& circuit, const std::vector<size_t>& output_ids,
		const std::vector<size_t>& variables, const std::vector<size_t>& requirement_ids,
		size_t max_nodes)
	{
		if (variables.size() > dimension_limit || output_ids.size() > dimension_limit ||
			circuit.nodes.size() > max_nodes || requirement_ids.size() > artifact_node_limit)
			throw cegis_limit("CEGIS source dimensions or node budget exceeded");
		forward_.build(circuit, output_ids, variables, requirement_ids, [](size_t) {});
	}

	// Builds miter expressions. Forward copies and candidate terms are interned
	// together, so repeated operands keep one identity for simplification and
	// affine propagation.
	struct symbolic
	{
		size_t limit;
		node_interner nodes;
		std::unordered_map<const void*, size_t> depths;

		explicit symbolic(size_t node_limit) : limit(node_limit) {}

		bit_tracker intern(pointer result)
		{
			if (auto existing = nodes.find(result))
				return bit_tracker(std::move(existing));
			if (nodes.size() >= limit)
				throw cegis_limit("CEGIS miter node budget exceeded");
			const size_t depth = result->_1 ? 1 + std::max(depths.at(result->_1.get()),
				result->_2 ? depths.at(result->_2.get()) : size_t{0}) : 0;
			if (depth > depth_limit)
				throw cegis_limit("CEGIS miter depth exceeds the " + std::to_string(depth_limit) +
					"-gate safety cap");
			depths.emplace(result.get(), depth);
			nodes.insert(result);
			return bit_tracker(std::move(result));
		}

		bit_tracker constant(bool value)
		{
			return intern(details::make_boolean_constant(value));
		}

		bit_tracker variable()
		{
			if (nodes.size() >= limit)
				throw cegis_limit("CEGIS miter node budget exceeded");
			auto result = details::make_bitstate_operation('*');
			depths.emplace(result.get(), 0);
			nodes.insert(result);
			return bit_tracker(std::move(result));
		}

		// Flattens nested XOR and complements into a parity and a set of
		// atoms, so (x XOR h) XOR h reduces to x before SAT has to prove it.
		// AND/OR atoms stay shared; products are never distributed.
		bit_tracker exclusive_or(const bit_tracker& lhs, const bit_tracker& rhs)
		{
			std::vector<pointer> pending{lhs.bit_state, rhs.bit_state};
			std::map<size_t, pointer> atoms;
			bool parity = false;
			size_t visits = 0;
			while (!pending.empty())
			{
				if (++visits > limit)
					throw cegis_limit("CEGIS XOR normalization budget exceeded");
				auto current = std::move(pending.back());
				pending.pop_back();
				if (current->operation == '=')
					parity ^= current->state != 0;
				else if (current->operation == '^')
				{
					pending.push_back(current->_1);
					pending.push_back(current->_2);
				}
				else if (current->operation == '!')
				{
					parity = !parity;
					pending.push_back(current->_1);
				}
				else
				{
					const size_t identity = nodes.identity(current);
					if (!atoms.erase(identity))
						atoms.emplace(identity, std::move(current));
				}
			}
			auto value = constant(false);
			for (const auto& atom : atoms | std::views::values)
				value = intern(details::make_bitstate_operation('^', value.bit_state, atom));
			return parity ? intern(details::make_bitstate_operation('!', value.bit_state)) : value;
		}

		bit_tracker op(char operation, const bit_tracker& lhs, const bit_tracker& rhs = {})
		{
			if (operation == '^')
				return exclusive_or(lhs, rhs);
			pointer a = lhs.bit_state, b = operation == '!' ? pointer{} : rhs.bit_state;
			if (b && nodes.identity(b) < nodes.identity(a))
				std::swap(a, b);
			return intern(details::make_bitstate_operation(operation, a, b));
		}
	};

	bits forward_symbolic(const bits& input, symbolic& builder) const
	{
		bits nodes;
		nodes.reserve(forward_.gates.size());
		for (const auto step : forward_.gates)
		{
			if (step.operation == '=')
				nodes.push_back(builder.constant(step.first != 0));
			else if (step.operation == '*')
				nodes.push_back(input[step.first]);
			else
				nodes.push_back(builder.op(step.operation, nodes[step.first],
					step.operation == '!' ? bit_tracker{} : nodes[step.second]));
		}
		return nodes;
	}

	std::vector<unsigned char> forward_values(const values& input) const
	{
		std::vector<unsigned char> nodes(forward_.gates.size());
		forward_.evaluate(nodes, 1, [&](index column) { return input[column]; });
		return nodes;
	}

	bits candidate_symbolic(const bits& target, symbolic& builder) const
	{
		bits terms;
		for (const auto& [first, second] : terms_)
			terms.push_back(first == target_bits() ? builder.constant(true) : second == target_bits() ?
				target[first] : builder.op('&', target[first], target[second]));
		bits result;
		for (const auto& coefficients : coefficients_)
		{
			auto value = builder.constant(false);
			for (const size_t term_id : coefficients)
				value = builder.op('^', value, terms[term_id]);
			result.push_back(std::move(value));
		}
		return result;
	}

	// The relational miter over unknowns x: true when x is allowed but the
	// candidate's input for target(x) is disallowed or produces another target.
	std::shared_ptr<const circuit_type> miter(const bits& x, symbolic& builder) const
	{
		const auto first = forward_symbolic(x, builder);
		bits target;
		for (const index root : forward_.outputs)
			target.push_back(first[root]);
		const auto candidate = candidate_symbolic(target, builder);
		const auto second = forward_symbolic(candidate, builder);
		auto allowed = builder.constant(true), bad = builder.constant(false);
		for (const index root : forward_.requirements)
		{
			allowed = builder.op('&', allowed, first[root]);
			bad = builder.op('|', bad, builder.op('!', second[root]));
		}
		for (size_t i = 0; i < forward_.outputs.size(); ++i)
			bad = builder.op('|', bad, builder.op('^', second[forward_.outputs[i]], target[i]));
		bad = builder.op('&', allowed, bad);
		// Unused x variables stay explicit so a SAT model supplies complete training inputs.
		std::vector<pointer> roots{bad.bit_state};
		for (const auto& variable : x)
			roots.push_back(variable.bit_state);
		return std::make_shared<const circuit_type>(std::move(roots));
	}

	// An input the candidate mishandles, or nothing once an exhaustive solver
	// query has proven the miter unsatisfiable.
	std::optional<values> counterexample(const cegis_options& options, cegis_statistics& stats) const
	{
		const scoped_timer timing(stats.verification_elapsed);
		symbolic builder(options.max_nodes);
		bits x;
		for (size_t i = 0; i < forward_.inputs; ++i)
			x.push_back(builder.variable());
		const auto circuit = miter(x, builder);
		if (stats.solver_steps >= options.max_solver_steps)
			throw cegis_limit("CEGIS solver step budget exhausted");
		solver_options solver;
		solver.max_search_steps = options.max_solver_steps - stats.solver_steps;
		solver_statistics query;
		std::optional<values> witness;
		++stats.solver_calls;
		try
		{
			collision_resolution::solve_compiled_stream(circuit, {{circuit->root_id, true}},
				[&](const collision_resolution::solution& model)
				{
					values input;
					for (const auto& variable : x)
						input.push_back(model.assignments.at(variable.bit_state));
					witness = std::move(input);
					return false;
				}, solver, &query);
		}
		catch (const solver_limit& error)
		{
			stats.solver_steps += query.search_steps;
			throw cegis_limit(error.what());
		}
		stats.solver_steps += query.search_steps;
		return witness;
	}

	static void check_options(const cegis_options& options)
	{
		if ((options.max_degree != 1 && options.max_degree != 2) || !options.max_features ||
			!options.max_counterexamples || !options.max_solver_steps || !options.max_nodes)
			throw std::invalid_argument("CEGIS requires degree 1 or 2 and positive resource budgets");
		if (options.max_features > feature_limit || options.max_nodes > artifact_node_limit ||
			options.max_counterexamples > counterexample_limit)
			throw cegis_limit("CEGIS options exceed implementation safety caps");
	}

	// The constant 1, every target bit and, for degree 2, every product of two
	// distinct target bits.
	static std::vector<term> feature_basis(size_t width, size_t degree)
	{
		std::vector<term> basis{{width, width}};
		for (size_t i = 0; i < width; ++i)
			basis.emplace_back(i, width);
		if (degree == 2)
			for (size_t i = 0; i < width; ++i)
				for (size_t j = i + 1; j < width; ++j)
					basis.emplace_back(i, j);
		return basis;
	}

	static equation training_equation(const std::vector<term>& basis,
		const values& target, const values& witness)
	{
		const size_t width = target.size();
		equation row{words((basis.size() + 63) / 64), words((witness.size() + 63) / 64)};
		for (size_t i = 0; i < basis.size(); ++i)
		{
			const auto [first, second] = basis[i];
			if (first == width || (target[first] && (second == width || target[second])))
				set(row.features, i);
		}
		for (size_t i = 0; i < witness.size(); ++i)
			if (witness[i])
				set(row.input, i);
		return row;
	}

	// Reduces row by the stored equations and stores it under its new pivot.
	// A row without a pivot either repeats a satisfied constraint or shows
	// that no polynomial in the family fits the chosen witnesses.
	static void add_equation(std::map<size_t, equation>& rows, equation row, size_t features)
	{
		size_t pivot = absent;
		for (size_t column = 0; column < features; ++column)
			if (get(row.features, column))
			{
				const auto existing = rows.find(column);
				if (existing == rows.end())
				{
					pivot = column;
					break;
				}
				add(row.features, existing->second.features);
				add(row.input, existing->second.input);
			}
		if (pivot == absent)
		{
			if (nonzero(row.input))
				throw cegis_limit("CEGIS chosen witnesses do not fit the requested polynomial family");
			throw std::logic_error("CEGIS repeated a satisfied training constraint");
		}
		rows.emplace(pivot, std::move(row));
	}

	// Back-substitutes the triangular equations into coefficients. Free
	// coefficients stay zero: this interpolates the witnesses rather than
	// minimizing the selector, and fixed witnesses can miss smaller ones.
	void interpolate(const std::map<size_t, equation>& rows, const std::vector<term>& basis)
	{
		const size_t inputs = forward_.inputs;
		std::vector<words> solution(basis.size(), words((inputs + 63) / 64));
		for (auto iter = rows.rbegin(); iter != rows.rend(); ++iter)
		{
			auto value = iter->second.input;
			for (auto next = rows.upper_bound(iter->first); next != rows.end(); ++next)
				if (get(iter->second.features, next->first))
					add(value, solution[next->first]);
			solution[iter->first] = std::move(value);
		}
		terms_ = basis;
		coefficients_.assign(inputs, {});
		for (size_t term_id = 0; term_id < basis.size(); ++term_id)
			for (size_t bit = 0; bit < inputs; ++bit)
				if (get(solution[term_id], bit))
					coefficients_[bit].push_back(term_id);
		compact();
	}

	// Drops the terms no coefficient uses, renumbering the rest in order.
	void compact()
	{
		std::vector<size_t> remap(terms_.size(), absent);
		for (const auto& output : coefficients_)
			for (const size_t id : output)
				remap[id] = 0;
		std::vector<term> retained;
		for (size_t i = 0; i < terms_.size(); ++i)
			if (remap[i] != absent)
			{
				remap[i] = retained.size();
				retained.push_back(terms_[i]);
			}
		for (auto& output : coefficients_)
			for (auto& id : output)
				id = remap[id];
		terms_ = std::move(retained);
	}

	// Trains on a counterexample: its target must be one the candidate fails.
	void learn(const values& witness, const std::vector<term>& basis, std::map<size_t, equation>& rows)
	{
		const auto nodes = forward_values(witness);
		values target;
		for (const index root : forward_.outputs)
			target.push_back(nodes[root] != 0);
		for (const index root : forward_.requirements)
			if (!nodes[root])
				throw std::logic_error("CEGIS solver returned a disallowed witness");
		if (evaluate(target))
			throw std::logic_error("CEGIS solver returned a spurious counterexample");
		add_equation(rows, training_equation(basis, target, witness), basis.size());
		interpolate(rows, basis);
	}

public:
	static cegis_plan compile(const circuit_type& circuit, const std::vector<size_t>& outputs,
		const std::vector<size_t>& variables, const std::vector<size_t>& requirements,
		const cegis_options& options, cegis_statistics* statistics)
	{
		cegis_statistics local;
		auto& stats = statistics ? *statistics : local;
		stats = {};
		const scoped_timer timing(stats.elapsed);
		stats.failed_phase = "preparation";
		check_options(options);
		cegis_plan plan;
		plan.prepare(circuit, outputs, variables, requirements, options.max_nodes);
		const auto basis = feature_basis(outputs.size(), options.max_degree);
		stats.features = basis.size();
		if (basis.size() > options.max_features)
			throw cegis_limit("CEGIS feature budget exceeded");
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
				stats.certified = true;
				stats.failed_phase = "none";
				return plan;
			}
			stats.failed_phase = "learning";
			if (stats.counterexamples >= options.max_counterexamples)
				throw cegis_limit("CEGIS counterexample budget exceeded");
			++stats.counterexamples;
			plan.learn(*witness, basis, rows);
		}
	}

	size_t term_count() const { return terms_.size(); }

	size_t coefficient_count() const
	{
		size_t count = 0;
		for (const auto& output : coefficients_)
			count += output.size();
		return count;
	}

	std::optional<values> evaluate(const values& target) const
	{
		if (target.size() != target_bits())
			throw std::invalid_argument("CEGIS target has wrong width");
		values terms;
		for (const auto& [first, second] : terms_)
			terms.push_back(first == target_bits() ||
				(target[first] && (second == target_bits() || target[second])));
		values input(forward_.inputs);
		for (size_t bit = 0; bit < input.size(); ++bit)
			for (const size_t id : coefficients_[bit])
				input[bit] = input[bit] != terms[id];
		if (!forward_.accepts(forward_values(input), target))
			return std::nullopt;
		return input;
	}

	// A learned plan holds one witness per target, so it reports at most one.
	size_t enumerate(const values& target, const std::function<bool(const values&)>& callback, size_t) const
	{
		const auto input = evaluate(target);
		if (input)
			callback(*input);
		return input.has_value();
	}

	void save(std::ostream& stream) const
	{
		stream << "CEGIS 1\n" << decimal(terms_.size()) << '\n';
		for (const auto& [first, second] : terms_)
			stream << decimal(first) << ' ' << decimal(second) << '\n';
		for (const auto& output : coefficients_)
		{
			stream << decimal(output.size());
			for (const size_t id : output)
				stream << ' ' << decimal(id);
			stream << '\n';
		}
		stream << "END_CEGIS\n";
	}

	static cegis_plan load(std::istream& stream, const circuit_type& circuit,
		const std::vector<size_t>& outputs, const std::vector<size_t>& variables,
		const std::vector<size_t>& requirements)
	{
		text_reader<std::invalid_argument> reader(stream, "CEGIS plan");
		reader.expect("CEGIS");
		if (reader.number(1) != 1)
			throw std::invalid_argument("Unsupported CEGIS plan version");
		cegis_plan result;
		result.prepare(circuit, outputs, variables, requirements, artifact_node_limit);
		const size_t count = reader.number(feature_limit), width = outputs.size();
		std::set<term> unique;
		for (size_t i = 0; i < count; ++i)
		{
			const size_t first = reader.number(width), second = reader.number(width);
			if ((first == width && second != width) || (second < width && first >= second) ||
				!unique.emplace(first, second).second)
				throw std::invalid_argument("Invalid CEGIS term");
			result.terms_.emplace_back(first, second);
		}
		std::vector<bool> used(count);
		for (size_t i = 0; i < variables.size(); ++i)
		{
			const size_t size = reader.number(count);
			std::vector<size_t> coefficients;
			for (size_t j = 0; j < size; ++j)
			{
				const size_t id = reader.number(count - 1);
				if (j && id <= coefficients.back())
					throw std::invalid_argument("Invalid CEGIS coefficients");
				coefficients.push_back(id);
				used[id] = true;
			}
			result.coefficients_.push_back(std::move(coefficients));
		}
		if (std::ranges::find(used, false) != used.end())
			throw std::invalid_argument("Unused CEGIS term");
		reader.expect("END_CEGIS");
		return result;
	}

	void export_cpp(std::ostream& stream, const cpp_signature& signature) const
	{
		signature.begin(stream, "Generated counterexample-learned inverse with an exact forward check.",
			{"array", "optional", "vector"});
		stream << "  (void)target;\n  std::array<bool, " << decimal(terms_.size()) << "> terms{};\n";
		for (size_t i = 0; i < terms_.size(); ++i)
		{
			const auto [first, second] = terms_[i];
			stream << "  terms[" << decimal(i) << "] = ";
			if (first == target_bits())
				stream << "true";
			else
			{
				stream << "target[" << decimal(first) << ']';
				if (second < target_bits())
					stream << " && target[" << decimal(second) << ']';
			}
			stream << ";\n";
		}
		stream << "  std::array<bool, " << decimal(forward_.inputs) << "> input{};\n";
		for (size_t i = 0; i < forward_.inputs; ++i)
		{
			stream << "  input[" << decimal(i) << "] = false";
			for (const size_t id : coefficients_[i])
				stream << " ^ terms[" << decimal(id) << ']';
			stream << ";\n";
		}
		stream << "  std::vector<unsigned char> nodes(" << decimal(forward_.gates.size()) << ");\n";
		for (size_t i = 0; i < forward_.gates.size(); ++i)
		{
			const auto step = forward_.gates[i];
			stream << "  nodes[" << decimal(i) << "] = ";
			if (step.operation == '=')
				stream << (step.first ? "true" : "false");
			else if (step.operation == '*')
				stream << "input[" << decimal(step.first) << ']';
			else if (step.operation == '!')
				stream << "!nodes[" << decimal(step.first) << ']';
			else
				stream << "nodes[" << decimal(step.first) << "] " << step.operation
					<< " nodes[" << decimal(step.second) << ']';
			stream << ";\n";
		}
		for (const index root : forward_.requirements)
			stream << "  if (!nodes[" << decimal(root) << "]) return std::nullopt;\n";
		for (size_t i = 0; i < forward_.outputs.size(); ++i)
			stream << "  if (bool(nodes[" << decimal(forward_.outputs[i]) << "]) != target["
				<< decimal(i) << "]) return std::nullopt;\n";
		signature.end(stream, [&](size_t column) { stream << "input[" << decimal(column) << ']'; });
	}
};

} // namespace detail
} // namespace dixelu::bitreverse::inversion

#endif // DIXELU_BITREVERSE_INVERSE_CEGIS_PLAN_H
