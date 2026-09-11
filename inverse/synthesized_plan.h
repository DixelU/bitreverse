#ifndef DIXELU_BITREVERSE_INVERSE_SYNTHESIZED_PLAN_H
#define DIXELU_BITREVERSE_INVERSE_SYNTHESIZED_PLAN_H

#include "../bitreverse.h"

#include <charconv>
#include <chrono>
#include <functional>
#include <istream>
#include <ostream>
#include <string_view>
#include <unordered_map>
#include <unordered_set>

namespace dixelu::bitreverse::inversion
{

struct synthesis_options
{
	size_t max_nodes = 750000;
	size_t max_operations = 10000000;
};

struct synthesis_statistics
{
	size_t created_nodes{};
	size_t operations{};
	size_t relation_nodes{};
	size_t function_nodes{};
	std::chrono::nanoseconds elapsed{};
};

class synthesis_limit : public std::runtime_error
{
public:
	explicit synthesis_limit(const std::string& message) : std::runtime_error(message) {}
};

namespace detail
{

// Reduced ordered decision diagrams use target variables first, followed by
// unknown input variables. IDs 0 and 1 are false and true; all other IDs name
// a decision (variable ? high : low), with children preceding their parent.
// Compilation constructs an exact relation, then eliminates its input
// variables to produce a validity predicate and a canonical witness circuit.
// Evaluation of that circuit never invokes a solver or traverses input choices.
class synthesized_plan
{
	static constexpr size_t absent = static_cast<size_t>(-1);
	static constexpr size_t max_dimension = 512;
	static constexpr size_t storage_node_limit = 1000000;
	struct decision
	{
		size_t variable, low, high;
		bool operator==(const decision&) const = default;
	};
	struct decision_hash
	{
		size_t operator()(const decision& value) const
		{
			size_t seed = value.variable;
			seed ^= value.low + size_t{0x9e3779b9} + (seed << 6) + (seed >> 2);
			return seed ^ (value.high + size_t{0x9e3779b9} + (seed << 6) + (seed >> 2));
		}
	};
	struct pair_key
	{
		size_t first, second;
		bool operator==(const pair_key&) const = default;
	};
	struct pair_hash
	{
		size_t operator()(const pair_key& key) const
		{
			return key.first ^ (key.second + size_t{0x9e3779b9} +
				(key.first << 6) + (key.first >> 2));
		}
	};

	class manager
	{
		const synthesis_options& options_;
		std::unordered_map<decision, size_t, decision_hash> unique_;
		std::unordered_map<decision, size_t, decision_hash> apply_cache_;
	public:
		std::vector<decision> nodes{{absent, 0, 0}, {absent, 1, 1}};
		size_t operations{};

		explicit manager(const synthesis_options& options) : options_(options) {}
		void tick(size_t amount = 1)
		{
			if (amount > options_.max_operations - operations)
				throw synthesis_limit("Boolean synthesis operation budget exceeded");
			operations += amount;
		}
		size_t make(size_t variable, size_t low, size_t high)
		{
			tick();
			if (low == high) return low;
			const decision key{variable, low, high};
			if (const auto found = unique_.find(key); found != unique_.end())
				return found->second;
			if (nodes.size() - 2 >= options_.max_nodes || nodes.size() - 2 >= storage_node_limit)
				throw synthesis_limit("Boolean synthesis node budget exceeded");
			const size_t result = nodes.size();
			nodes.push_back(key);
			unique_.emplace(key, result);
			return result;
		}
		size_t apply(char operation, size_t lhs, size_t rhs)
		{
			tick();
			if (lhs > rhs) std::swap(lhs, rhs);
			if (operation == '&')
			{
				if (lhs == 0) return 0;
				if (lhs == 1 || lhs == rhs) return rhs;
			}
			else if (operation == '|')
			{
				if (lhs == 0 || lhs == rhs) return rhs;
				if (lhs == 1) return 1;
			}
			else
			{
				if (lhs == 0) return rhs;
				if (lhs == rhs) return 0;
			}
			if (rhs < 2) // The nontrivial terminal case is 1 XOR 0/1.
				return lhs ^ rhs;
			const decision key{static_cast<size_t>(operation), lhs, rhs};
			if (const auto found = apply_cache_.find(key); found != apply_cache_.end())
				return found->second;
			const auto a = nodes[lhs], b = nodes[rhs];
			const size_t variable = std::min(a.variable, b.variable);
			const size_t low = apply(operation,
				a.variable == variable ? a.low : lhs,
				b.variable == variable ? b.low : rhs);
			const size_t high = apply(operation,
				a.variable == variable ? a.high : lhs,
				b.variable == variable ? b.high : rhs);
			const size_t result = make(variable, low, high);
			// Cache eviction changes only compilation time. Its memory is bounded
			// independently of the total number of attempted operations.
			if (apply_cache_.size() >= options_.max_nodes) apply_cache_.clear();
			apply_cache_.emplace(key, result);
			return result;
		}
		size_t negate(size_t value) { return apply('^', value, 1); }
	};

	size_t input_count_{};
	size_t output_count_{};
	std::vector<decision> nodes_{{absent, 0, 0}, {absent, 1, 1}};
	size_t relation_{};
	size_t valid_{};
	std::vector<size_t> functions_;
	std::vector<size_t> function_order_;
	std::vector<decision> function_steps_;
	std::vector<size_t> function_results_;
	size_t function_valid_{};
	size_t relation_nodes_{};
	size_t selector_nodes_{};
	size_t validity_nodes_{};

	static std::vector<unsigned char> reachable(
		const std::vector<decision>& nodes, const std::vector<size_t>& roots,
		manager* budget = nullptr)
	{
		if (budget) budget->tick(nodes.size());
		std::vector<unsigned char> seen(nodes.size());
		std::vector<size_t> pending;
		for (const size_t root : roots)
		{
			if (!seen[root]) { seen[root] = 1; pending.push_back(root); }
		}
		while (!pending.empty())
		{
			if (budget) budget->tick();
			const size_t current = pending.back();
			pending.pop_back();
			if (current < 2) continue;
			for (const size_t child : {nodes[current].low, nodes[current].high})
				if (!seen[child]) { seen[child] = 1; pending.push_back(child); }
		}
		return seen;
	}

	void index_functions(manager* budget = nullptr)
	{
		auto roots = functions_;
		roots.push_back(valid_);
		const auto seen = reachable(nodes_, roots, budget);
		if (budget) budget->tick(nodes_.size());
		function_order_.clear();
		for (size_t id = 2; id < nodes_.size(); ++id)
			if (seen[id])
			{
				if (nodes_[id].variable >= output_count_)
					throw std::runtime_error("Synthesized function depends on an input variable");
				function_order_.push_back(id);
			}
		std::vector<size_t> remap(nodes_.size());
		remap[1] = 1;
		function_steps_.clear();
		for (const size_t id : function_order_)
		{
			const auto current = nodes_[id];
			remap[id] = function_steps_.size() + 2;
			function_steps_.push_back({current.variable, remap[current.low], remap[current.high]});
		}
		function_valid_ = remap[valid_];
		function_results_.clear();
		for (const size_t root : functions_) function_results_.push_back(remap[root]);
		const auto relation_seen = reachable(nodes_, {relation_}, budget);
		relation_nodes_ = static_cast<size_t>(std::count(
			relation_seen.begin() + 2, relation_seen.end(), static_cast<unsigned char>(1)));
		const auto selector_seen = reachable(nodes_, functions_, budget);
		selector_nodes_ = static_cast<size_t>(std::count(
			selector_seen.begin() + 2, selector_seen.end(), static_cast<unsigned char>(1)));
		const auto validity_seen = reachable(nodes_, {valid_}, budget);
		validity_nodes_ = static_cast<size_t>(std::count(
			validity_seen.begin() + 2, validity_seen.end(), static_cast<unsigned char>(1)));
	}

	static bool whitespace(int c)
	{
		return c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f' || c == '\v';
	}
	static void skip_space(std::istream& stream)
	{
		while (whitespace(stream.peek())) stream.get();
	}
	static void expect(std::istream& stream, std::string_view token)
	{
		skip_space(stream);
		for (const char c : token)
			if (stream.get() != c) throw std::runtime_error("Invalid synthesized plan marker");
		if (stream.peek() != std::char_traits<char>::eof() && !whitespace(stream.peek()))
			throw std::runtime_error("Invalid synthesized plan token");
	}
	static size_t read_number(std::istream& stream, size_t limit)
	{
		skip_space(stream);
		size_t value = 0, digits = 0;
		while (stream.peek() >= '0' && stream.peek() <= '9')
		{
			const size_t digit = static_cast<size_t>(stream.get() - '0');
			if (++digits > 20 || digit > limit || value > (limit - digit) / 10)
				throw std::runtime_error("Synthesized plan number exceeds its limit");
			value = value * 10 + digit;
		}
		if (!digits || (stream.peek() != std::char_traits<char>::eof() && !whitespace(stream.peek())))
			throw std::runtime_error("Invalid synthesized plan number");
		return value;
	}
	static void write_number(std::ostream& stream, size_t value)
	{
		char buffer[32];
		const auto result = std::to_chars(buffer, buffer + sizeof(buffer), value);
		stream.write(buffer, result.ptr - buffer);
		stream.put(' ');
	}

public:
	static synthesized_plan compile(
		const collision_resolution::solver_core::compiled_circuit& circuit,
		const std::vector<size_t>& output_ids,
		const std::vector<size_t>& variable_ids,
		const std::vector<size_t>& requirement_ids,
		const synthesis_options& options = {},
		synthesis_statistics* statistics = nullptr)
	{
		const auto started = std::chrono::steady_clock::now();
		manager bdd(options);
		if (statistics) *statistics = {};
		auto report = [&]
		{
			if (!statistics) return;
			statistics->created_nodes = bdd.nodes.size() - 2;
			statistics->operations = bdd.operations;
			statistics->elapsed = std::chrono::steady_clock::now() - started;
		};
		try
		{
			if (!options.max_nodes || !options.max_operations)
				throw synthesis_limit("Boolean synthesis budgets must be positive");
			const size_t inputs = variable_ids.size(), outputs = output_ids.size();
			if (outputs > max_dimension || inputs > max_dimension - outputs)
				throw synthesis_limit("Boolean synthesis supports at most 512 total input and output bits");
			const size_t count = circuit.nodes.size();
			if (circuit.inputs.size() != count)
				throw std::invalid_argument("Invalid synthesis circuit");
			bdd.tick(count);
			std::vector<size_t> columns(count, absent), forward(count);
			for (size_t column = 0; column < inputs; ++column)
			{
				bdd.tick();
				const size_t id = variable_ids[column];
				if (id >= count || !circuit.nodes[id] || circuit.nodes[id]->operation != '*' ||
					columns[id] != absent)
					throw std::invalid_argument("Invalid synthesis input variable");
				columns[id] = column;
			}
			std::vector<size_t> roots = output_ids;
			bdd.tick(requirement_ids.size());
			roots.insert(roots.end(), requirement_ids.begin(), requirement_ids.end());
			std::vector<unsigned char> state(count);
			struct frame { size_t id, next; };
			std::vector<frame> stack;
			for (const size_t root : roots)
			{
				bdd.tick();
				if (root >= count) throw std::invalid_argument("Invalid synthesis root");
				if (state[root] == 2) continue;
				stack.push_back({root, 0});
				state[root] = 1;
				while (!stack.empty())
				{
					bdd.tick();
					auto& current = stack.back();
					const size_t id = current.id;
					if (!circuit.nodes[id]) throw std::invalid_argument("Null synthesis gate");
					const char operation = static_cast<char>(circuit.nodes[id]->operation);
					if (operation != '=' && operation != '*' && operation != '!' &&
						operation != '&' && operation != '|' && operation != '^')
						throw std::invalid_argument("Unsupported synthesis gate");
					const size_t arity = dixelu::bitreverse::details::operation_args_count[
						static_cast<unsigned char>(operation)];
					if (current.next < arity)
					{
						const size_t child = circuit.inputs[id][current.next++];
						if (child >= count) throw std::invalid_argument("Invalid synthesis child");
						if (state[child] == 1) throw std::invalid_argument("Cyclic synthesis circuit");
						if (state[child] == 0) { state[child] = 1; stack.push_back({child, 0}); }
						continue;
					}
					if (operation == '=') forward[id] = circuit.nodes[id]->state != 0;
					else if (operation == '*')
					{
						if (columns[id] == absent)
							throw std::invalid_argument("Undeclared synthesis input variable");
						forward[id] = bdd.make(outputs + columns[id], 0, 1);
					}
					else if (operation == '!') forward[id] = bdd.negate(forward[circuit.inputs[id][0]]);
					else forward[id] = bdd.apply(operation,
						forward[circuit.inputs[id][0]], forward[circuit.inputs[id][1]]);
					state[id] = 2;
					stack.pop_back();
				}
			}
			size_t domain = 1;
			for (const size_t id : requirement_ids) domain = bdd.apply('&', domain, forward[id]);
			std::vector<size_t> complements;
			for (const size_t id : output_ids) complements.push_back(bdd.negate(forward[id]));
			std::unordered_map<pair_key, size_t, pair_hash> relation_cache;
			std::function<size_t(size_t, size_t)> relation = [&](size_t index, size_t subset) -> size_t
			{
				bdd.tick();
				if (subset == 0 || index == outputs) return subset;
				const pair_key key{index, subset};
				if (const auto found = relation_cache.find(key); found != relation_cache.end())
					return found->second;
				const size_t low_domain = bdd.apply('&', subset, complements[index]);
				const size_t high_domain = bdd.apply('&', subset, forward[output_ids[index]]);
				const size_t low = relation(index + 1, low_domain);
				const size_t high = relation(index + 1, high_domain);
				const size_t result = bdd.make(index, low, high);
				if (relation_cache.size() >= options.max_nodes) relation_cache.clear();
				relation_cache.emplace(key, result);
				return result;
			};
			synthesized_plan plan;
			plan.input_count_ = inputs;
			plan.output_count_ = outputs;
			plan.relation_ = relation(0, domain);

			struct witness { size_t valid; std::vector<size_t> bits; };
			std::unordered_map<size_t, witness> witness_cache;
			std::function<witness(size_t)> synthesize = [&](size_t id) -> witness
			{
				bdd.tick(inputs + 1);
				if (const auto found = witness_cache.find(id); found != witness_cache.end())
					return found->second;
				witness result{static_cast<size_t>(id != 0), std::vector<size_t>(inputs)};
				if (id >= 2 && bdd.nodes[id].variable < outputs)
				{
					const auto current = bdd.nodes[id];
					const auto low = synthesize(current.low), high = synthesize(current.high);
					result.valid = bdd.make(current.variable, low.valid, high.valid);
					for (size_t bit = 0; bit < inputs; ++bit)
					{
						bdd.tick();
						if (low.valid == 0) result.bits[bit] = high.bits[bit];
						else if (high.valid == 0) result.bits[bit] = low.bits[bit];
						else result.bits[bit] = bdd.make(current.variable, low.bits[bit], high.bits[bit]);
					}
				}
				else
				{
					// A nonzero input-domain BDD always has a satisfying path.
					// Prefer low edges and leave skipped variables at zero.
					for (size_t cursor = id; cursor >= 2;)
					{
						bdd.tick();
						const auto current = bdd.nodes[cursor];
						const bool high = current.low == 0;
						result.bits[current.variable - outputs] = high;
						cursor = high ? current.high : current.low;
					}
				}
				witness_cache.emplace(id, result);
				return result;
			};
			const auto canonical = synthesize(plan.relation_);
			plan.valid_ = canonical.valid;
			plan.functions_ = canonical.bits;
			auto plan_roots = plan.functions_;
			plan_roots.push_back(plan.valid_);
			plan_roots.push_back(plan.relation_);
			const auto used = reachable(bdd.nodes, plan_roots, &bdd);
			bdd.tick(bdd.nodes.size());
			std::vector<size_t> remap(bdd.nodes.size());
			remap[1] = 1;
			for (size_t id = 2; id < bdd.nodes.size(); ++id)
				if (used[id])
				{
					const auto current = bdd.nodes[id];
					remap[id] = plan.nodes_.size();
					plan.nodes_.push_back({current.variable, remap[current.low], remap[current.high]});
				}
			plan.relation_ = remap[plan.relation_];
			plan.valid_ = remap[plan.valid_];
			for (auto& function : plan.functions_) function = remap[function];
			plan.index_functions(&bdd);
			if (statistics)
			{
				statistics->relation_nodes = plan.relation_node_count();
				statistics->function_nodes = plan.function_node_count();
			}
			report();
			return plan;
		}
		catch (...) { report(); throw; }
	}

	size_t input_count() const { return input_count_; }
	size_t output_count() const { return output_count_; }
	size_t node_count() const { return nodes_.size() - 2; }
	size_t relation_node_count() const { return relation_nodes_; }
	size_t function_node_count() const { return function_order_.size(); }
	size_t selector_node_count() const { return selector_nodes_; }
	size_t validity_node_count() const { return validity_nodes_; }

	std::optional<std::vector<bool>> evaluate(const std::vector<bool>& target) const
	{
		if (target.size() != output_count_) throw std::invalid_argument("Wrong synthesized target width");
		std::vector<unsigned char> values(function_steps_.size() + 2);
		values[1] = 1;
		for (size_t index = 0; index < function_steps_.size(); ++index)
		{
			const auto current = function_steps_[index];
			values[index + 2] = values[target[current.variable] ? current.high : current.low];
		}
		if (!values[function_valid_]) return std::nullopt;
		std::vector<bool> result(input_count_);
		for (size_t bit = 0; bit < input_count_; ++bit) result[bit] = values[function_results_[bit]] != 0;
		return result;
	}

	size_t enumerate(const std::vector<bool>& target,
		const std::function<bool(const std::vector<bool>&)>& callback, size_t maximum = 1) const
	{
		if (target.size() != output_count_) throw std::invalid_argument("Wrong synthesized target width");
		if (!callback) throw std::invalid_argument("Missing synthesized enumeration callback");
		size_t root = relation_;
		while (root >= 2 && nodes_[root].variable < output_count_)
		{
			const auto current = nodes_[root];
			root = target[current.variable] ? current.high : current.low;
		}
		size_t found = 0;
		std::vector<bool> input(input_count_);
		std::function<bool(size_t, size_t)> visit = [&](size_t id, size_t bit) -> bool
		{
			if (id == 0) return true;
			if (bit == input_count_)
			{
				++found;
				const bool more = callback(input);
				return more && (!maximum || found < maximum);
			}
			const bool decision_here = id >= 2 && nodes_[id].variable == output_count_ + bit;
			input[bit] = false;
			if (!visit(decision_here ? nodes_[id].low : id, bit + 1)) return false;
			input[bit] = true;
			return visit(decision_here ? nodes_[id].high : id, bit + 1);
		};
		visit(root, 0);
		return found;
	}

	void save(std::ostream& stream) const
	{
		stream << "SYNTHESIZED_PLAN 1 ";
		for (const size_t value : {input_count_, output_count_, node_count(), relation_, valid_})
			write_number(stream, value);
		stream.put('\n');
		for (size_t id = 2; id < nodes_.size(); ++id)
		{
			for (const size_t value : {nodes_[id].variable, nodes_[id].low, nodes_[id].high})
				write_number(stream, value);
			stream.put('\n');
		}
		for (const size_t value : functions_) write_number(stream, value);
		stream << "\nEND_SYNTHESIZED_PLAN\n";
		if (!stream) throw std::runtime_error("Failed to write synthesized plan");
	}

	static synthesized_plan load(std::istream& stream)
	{
		expect(stream, "SYNTHESIZED_PLAN");
		if (read_number(stream, 1) != 1) throw std::runtime_error("Unsupported synthesized plan version");
		synthesized_plan plan;
		plan.input_count_ = read_number(stream, max_dimension);
		plan.output_count_ = read_number(stream, max_dimension - plan.input_count_);
		const size_t count = read_number(stream, storage_node_limit);
		plan.relation_ = read_number(stream, count + 1);
		plan.valid_ = read_number(stream, count + 1);
		const size_t dimensions = plan.input_count_ + plan.output_count_;
		if (count && !dimensions) throw std::runtime_error("Decision nodes without synthesis variables");
		std::unordered_set<decision, decision_hash> unique;
		plan.nodes_.reserve(count + 2);
		for (size_t index = 0; index < count; ++index)
		{
			const size_t id = index + 2;
			const size_t variable = read_number(stream, dimensions - 1);
			const size_t low = read_number(stream, id - 1);
			const size_t high = read_number(stream, id - 1);
			const decision current{variable, low, high};
			if (low == high || plan.nodes_[low].variable <= variable ||
				plan.nodes_[high].variable <= variable || !unique.insert(current).second)
				throw std::runtime_error("Invalid reduced ordered synthesized decision");
			plan.nodes_.push_back(current);
		}
		for (size_t bit = 0; bit < plan.input_count_; ++bit)
			plan.functions_.push_back(read_number(stream, count + 1));
		expect(stream, "END_SYNTHESIZED_PLAN");
		auto roots = plan.functions_;
		roots.push_back(plan.valid_);
		roots.push_back(plan.relation_);
		const auto used = reachable(plan.nodes_, roots);
		if (std::find(used.begin() + 2, used.end(), 0) != used.end())
			throw std::runtime_error("Unreachable synthesized decision node");
		plan.index_functions();
		return plan;
	}

	void export_cpp(std::ostream& stream, const std::vector<size_t>& input_columns,
		const std::vector<bool>& input_constants) const
	{
		if (input_columns.size() != input_constants.size())
			throw std::invalid_argument("Invalid synthesized export input layout");
		for (const size_t column : input_columns)
			if (column != absent && column >= input_count_)
				throw std::invalid_argument("Invalid synthesized export input column");
		// Only the canonical witness and validity DAG are emitted. The larger
		// relation used to enumerate alternative witnesses is deliberately absent.
		std::vector<size_t> remap(nodes_.size());
		remap[1] = 1;
		for (size_t index = 0; index < function_order_.size(); ++index)
			remap[function_order_[index]] = index + 2;
		// to_chars keeps generated C++ independent of the destination stream's
		// base, locale, boolalpha, precision and other numeric formatting flags.
		auto number = [](size_t value)
		{
			char buffer[32];
			const auto converted = std::to_chars(buffer, buffer + sizeof(buffer), value);
			return std::string(buffer, converted.ptr);
		};
		stream << "// Generated Boolean inverse; input/target bits retain their declared order.\n"
			<< "#pragma once\n#include <array>\n#include <cstddef>\n#include <optional>\n\n"
			<< "inline std::optional<std::array<bool, " << number(input_columns.size()) << ">>\n"
			<< "bitreverse_inverse(const std::array<bool, " << number(output_count_) << ">& target)\n{\n"
			<< "  struct decision { std::size_t variable, low, high; };\n"
			<< "  // A fixed topological Boolean circuit, evaluated once per target.\n"
			<< "  static constexpr std::array<decision, " << number(function_order_.size()) << "> schedule{{\n";
		for (const size_t id : function_order_)
		{
			const auto current = nodes_[id];
			stream << "    {" << number(current.variable) << ", " << number(remap[current.low])
				<< ", " << number(remap[current.high]) << "},\n";
		}
		stream << "  }};\n  std::array<bool, " << number(function_order_.size() + 2) << "> v{};\n"
			<< "  v[1] = true;\n"
			<< "  for (std::size_t i = 0; i < schedule.size(); ++i) {\n"
			<< "    const auto step = schedule[i];\n"
			<< "    v[i + 2] = v[target[step.variable] ? step.high : step.low];\n  }\n"
			<< "  if (!v[" << number(remap[valid_]) << "]) return std::nullopt;\n"
			<< "  return std::array<bool, " << number(input_columns.size()) << ">{{";
		for (size_t index = 0; index < input_columns.size(); ++index)
		{
			if (index) stream << ", ";
			if (input_columns[index] == absent) stream << (input_constants[index] ? "true" : "false");
			else stream << "v[" << number(remap[functions_[input_columns[index]]]) << "]";
		}
		stream << "}};\n}\n";
		if (!stream) throw std::runtime_error("Failed to export synthesized inverse");
	}
};

} // namespace detail
} // namespace dixelu::bitreverse::inversion

#endif // DIXELU_BITREVERSE_INVERSE_SYNTHESIZED_PLAN_H
