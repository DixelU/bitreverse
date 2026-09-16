#ifndef DIXELU_BITREVERSE_INVERSE_SELECTOR_PLAN_H
#define DIXELU_BITREVERSE_INVERSE_SELECTOR_PLAN_H

#include "../bitreverse.h"

#include <array>
#include <charconv>
#include <chrono>
#include <cstdint>
#include <functional>
#include <istream>
#include <limits>
#include <memory_resource>
#include <ostream>
#include <string_view>

namespace dixelu::bitreverse::inversion
{

struct selector_options
{
	size_t max_assignments = 65536;
	size_t max_nodes = 131071;
	size_t max_operations = 250000000;
	size_t max_bytes = 268435456;
};

struct selector_statistics
{
	size_t domain_assignments{}, allowed_assignments{}, distinct_outputs{};
	size_t decision_nodes{}, leaves{}, operations{}, peak_bytes{};
	std::chrono::nanoseconds enumeration_elapsed{}, tree_elapsed{}, validation_elapsed{}, elapsed{};
	std::string_view failed_phase = "none";
};

struct selector_evaluation_statistics
{
	size_t decision_visits{}, forward_gate_evaluations{};
};

class selector_limit : public std::runtime_error
{
public:
	explicit selector_limit(const std::string& message) : std::runtime_error(message) {}
};

namespace detail
{

// A bounded-domain baseline: enumerate the forward circuit in 64-bit batches,
// group equal outputs, and learn a target-bit decision tree. The tree needs no
// separate validity diagram: one forward evaluation checks its chosen input.
class selector_plan
{
	using circuit_type = collision_resolution::solver_core::compiled_circuit;
	using index = std::uint32_t;
	static constexpr index leaf_flag = index{1} << 31;
	static constexpr index absent = std::numeric_limits<index>::max();
	static constexpr size_t max_domain = size_t{1} << 20;
	static constexpr size_t max_tree = max_domain * 2 - 1;
	static constexpr size_t max_circuit = 1000000;
	static constexpr size_t max_dimensions = 512;
	static constexpr size_t load_memory_limit = 268435456;
	struct gate { char operation; index first{}, second{}; };
	struct decision { index variable, low, high; };
	struct group { index begin, count; };

	class bounded_resource final : public std::pmr::memory_resource
	{
		size_t limit_, current_{}, peak_{};
		void* do_allocate(size_t bytes, size_t alignment) override
		{
			if (bytes > limit_ - current_) throw selector_limit("Selector byte budget exceeded");
			void* result = std::pmr::new_delete_resource()->allocate(bytes, alignment);
			current_ += bytes;
			peak_ = std::max(peak_, current_);
			return result;
		}
		void do_deallocate(void* pointer, size_t bytes, size_t alignment) override
		{
			std::pmr::new_delete_resource()->deallocate(pointer, bytes, alignment);
			current_ -= bytes;
		}
		bool do_is_equal(const std::pmr::memory_resource& other) const noexcept override
		{
			return this == &other;
		}
	public:
		explicit bounded_resource(size_t limit) : limit_(limit) {}
		size_t current() const { return current_; }
		size_t peak() const { return peak_; }
	};

	struct storage
	{
		// The resource outlives every container that refers to it, including in
		// returned plans. max_bytes counts actual container allocation requests;
		// fixed objects, recursion frames and allocator overhead are excluded.
		bounded_resource memory;
		std::pmr::vector<gate> gates;
		std::pmr::vector<index> outputs, requirements;
		std::pmr::vector<decision> decisions;
		std::pmr::vector<group> groups;
		std::pmr::vector<index> assignments;
		size_t inputs{}, target_bits{};
		index root = absent;
		explicit storage(size_t limit)
			: memory(limit), gates(&memory), outputs(&memory), requirements(&memory),
			  decisions(&memory), groups(&memory), assignments(&memory) {}
	};
	std::shared_ptr<const storage> data_;
	explicit selector_plan(std::shared_ptr<const storage> data) : data_(std::move(data)) {}

	struct work_budget
	{
		size_t limit, operations{};
		void tick(size_t amount = 1)
		{
			if (amount > limit - operations) throw selector_limit("Selector operation budget exceeded");
			operations += amount;
		}
	};

	static void dimensions(size_t inputs, size_t outputs)
	{
		if (inputs > 20 || outputs > max_dimensions - inputs)
			throw selector_limit("Selector supports at most 20 unknown bits and 512 total input/output bits");
	}

	static void prepare(storage& result, const circuit_type& circuit,
		const std::vector<size_t>& output_ids, const std::vector<size_t>& variable_ids,
		const std::vector<size_t>& requirement_ids, work_budget& budget)
	{
		dimensions(variable_ids.size(), output_ids.size());
		result.inputs = variable_ids.size();
		result.target_bits = output_ids.size();
		const size_t count = circuit.nodes.size();
		if (count > max_circuit || circuit.inputs.size() != count || requirement_ids.size() > max_circuit)
			throw std::invalid_argument("Invalid or oversized selector forward circuit");
		budget.tick(count);
		std::pmr::vector<index> columns(count, absent, &result.memory), remap(count, absent, &result.memory);
		std::pmr::vector<unsigned char> state(count, &result.memory);
		for (size_t column = 0; column < variable_ids.size(); ++column)
		{
			const size_t id = variable_ids[column];
			if (id >= count || !circuit.nodes[id] || circuit.nodes[id]->operation != '*' || columns[id] != absent)
				throw std::invalid_argument("Invalid selector unknown variable");
			columns[id] = static_cast<index>(column);
		}
		struct frame { size_t id, next; };
		std::pmr::vector<frame> stack(&result.memory);
		auto add_root = [&](size_t root)
		{
			if (root >= count) throw std::invalid_argument("Invalid selector forward root");
			if (state[root] == 2) return remap[root];
			state[root] = 1;
			stack.push_back({root, 0});
			while (!stack.empty())
			{
				budget.tick();
				auto& current = stack.back();
				const size_t id = current.id;
				if (!circuit.nodes[id]) throw std::invalid_argument("Null selector forward gate");
				const char operation = static_cast<char>(circuit.nodes[id]->operation);
				if (operation != '=' && operation != '*' && operation != '!' &&
					operation != '&' && operation != '|' && operation != '^')
					throw std::invalid_argument("Unsupported selector forward operation");
				const size_t arity = dixelu::bitreverse::details::operation_args_count[static_cast<unsigned char>(operation)];
				if (current.next < arity)
				{
					const size_t child = circuit.inputs[id][current.next++];
					if (child >= count) throw std::invalid_argument("Invalid selector forward child");
					if (state[child] == 1) throw std::invalid_argument("Cyclic selector forward circuit");
					if (!state[child]) { state[child] = 1; stack.push_back({child, 0}); }
					continue;
				}
				gate step{operation};
				if (operation == '=') step.first = circuit.nodes[id]->state != 0;
				else if (operation == '*')
				{
					if (columns[id] == absent) throw std::invalid_argument("Undeclared selector input variable");
					step.first = columns[id];
				}
				else
				{
					step.first = remap[circuit.inputs[id][0]];
					if (arity == 2) step.second = remap[circuit.inputs[id][1]];
				}
				remap[id] = static_cast<index>(result.gates.size());
				result.gates.push_back(step);
				state[id] = 2;
				stack.pop_back();
			}
			return remap[root];
		};
		for (const size_t id : output_ids) result.outputs.push_back(add_root(id));
		for (const size_t id : requirement_ids) result.requirements.push_back(add_root(id));
	}

	static std::uint64_t input_word(size_t base, size_t exponent)
	{
		constexpr std::array<std::uint64_t, 6> patterns{
			0xaaaaaaaaaaaaaaaaULL, 0xccccccccccccccccULL, 0xf0f0f0f0f0f0f0f0ULL,
			0xff00ff00ff00ff00ULL, 0xffff0000ffff0000ULL, 0xffffffff00000000ULL};
		if (exponent < patterns.size()) return patterns[exponent];
		return ((base >> exponent) & 1) ? ~std::uint64_t{} : 0;
	}

	template<class Values>
	static void evaluate_batch(const storage& data, size_t base, Values& values)
	{
		for (size_t id = 0; id < data.gates.size(); ++id)
		{
			const auto step = data.gates[id];
			switch (step.operation)
			{
			case '=': values[id] = step.first ? ~std::uint64_t{} : 0; break;
			case '*': values[id] = input_word(base, data.inputs - 1 - step.first); break;
			case '!': values[id] = ~values[step.first]; break;
			case '&': values[id] = values[step.first] & values[step.second]; break;
			case '|': values[id] = values[step.first] | values[step.second]; break;
			default: values[id] = values[step.first] ^ values[step.second]; break;
			}
		}
	}

	static index descend(const storage& data, const std::vector<bool>& target,
		selector_evaluation_statistics* statistics = nullptr)
	{
		index root = data.root;
		while (root != absent && !(root & leaf_flag))
		{
			const auto current = data.decisions[root];
			root = target[current.variable] ? current.high : current.low;
			if (statistics) ++statistics->decision_visits;
		}
		return root == absent ? absent : root & ~leaf_flag;
	}

	static bool verify(const storage& data, index rank, const std::vector<bool>& target,
		selector_evaluation_statistics* statistics)
	{
		std::vector<unsigned char> values(data.gates.size());
		for (size_t id = 0; id < data.gates.size(); ++id)
		{
			const auto step = data.gates[id];
			switch (step.operation)
			{
			case '=': values[id] = step.first != 0; break;
			case '*': values[id] = (rank >> (data.inputs - 1 - step.first)) & 1; break;
			case '!': values[id] = !values[step.first]; break;
			case '&': values[id] = values[step.first] & values[step.second]; break;
			case '|': values[id] = values[step.first] | values[step.second]; break;
			default: values[id] = values[step.first] ^ values[step.second]; break;
			}
		}
		if (statistics) statistics->forward_gate_evaluations = data.gates.size();
		for (const index root : data.requirements) if (!values[root]) return false;
		for (size_t bit = 0; bit < data.outputs.size(); ++bit)
			if ((values[data.outputs[bit]] != 0) != target[bit]) return false;
		return true;
	}

	static std::vector<bool> unpack(const storage& data, index rank)
	{
		std::vector<bool> result(data.inputs);
		for (size_t bit = 0; bit < data.inputs; ++bit)
			result[bit] = ((rank >> (data.inputs - 1 - bit)) & 1) != 0;
		return result;
	}

	static std::string number(size_t value)
	{
		char buffer[32];
		const auto result = std::to_chars(buffer, buffer + sizeof(buffer), value);
		return std::string(buffer, result.ptr);
	}
	static bool whitespace(int c)
	{
		return c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f' || c == '\v';
	}
	static void skip_space(std::istream& stream) { while (whitespace(stream.peek())) stream.get(); }
	static void expect(std::istream& stream, std::string_view token)
	{
		skip_space(stream);
		for (const char c : token)
			if (stream.get() != c) throw std::runtime_error("Invalid selector plan marker");
		if (stream.peek() != std::char_traits<char>::eof() && !whitespace(stream.peek()))
			throw std::runtime_error("Invalid selector plan token");
	}
	static size_t read_number(std::istream& stream, size_t limit)
	{
		skip_space(stream);
		size_t value = 0, digits = 0;
		while (stream.peek() >= '0' && stream.peek() <= '9')
		{
			const size_t digit = static_cast<size_t>(stream.get() - '0');
			if (++digits > 20 || digit > limit || value > (limit - digit) / 10)
				throw std::runtime_error("Selector plan number exceeds its limit");
			value = value * 10 + digit;
		}
		if (!digits || (stream.peek() != std::char_traits<char>::eof() && !whitespace(stream.peek())))
			throw std::runtime_error("Invalid selector plan number");
		return value;
	}

public:
	static selector_plan compile(const circuit_type& circuit,
		const std::vector<size_t>& output_ids, const std::vector<size_t>& variable_ids,
		const std::vector<size_t>& requirement_ids, const selector_options& options = {},
		selector_statistics* statistics = nullptr)
	{
		const auto started = std::chrono::steady_clock::now();
		auto phase_started = started;
		std::string_view phase = "enumeration";
		if (statistics) *statistics = {};
		std::shared_ptr<storage> data;
		work_budget budget{options.max_operations};
		auto report = [&]
		{
			if (!statistics) return;
			statistics->operations = budget.operations;
			statistics->elapsed = std::chrono::steady_clock::now() - started;
			if (data)
			{
				statistics->peak_bytes = data->memory.peak();
				statistics->decision_nodes = data->decisions.size();
				statistics->leaves = data->groups.size();
			}
		};
		auto finish_phase = [&]
		{
			if (!statistics) return;
			const auto elapsed = std::chrono::steady_clock::now() - phase_started;
			if (phase == "enumeration") statistics->enumeration_elapsed = elapsed;
			else if (phase == "tree") statistics->tree_elapsed = elapsed;
			else statistics->validation_elapsed = elapsed;
		};
		try
		{
			if (!options.max_assignments || !options.max_nodes || !options.max_operations || !options.max_bytes)
				throw selector_limit("Selector budgets must be positive");
			dimensions(variable_ids.size(), output_ids.size());
			const size_t domain = size_t{1} << variable_ids.size();
			if (statistics) statistics->domain_assignments = domain;
			if (domain > options.max_assignments) throw selector_limit("Selector assignment budget exceeded");
			data = std::make_shared<storage>(options.max_bytes);
			prepare(*data, circuit, output_ids, variable_ids, requirement_ids, budget);
			const size_t words = (output_ids.size() + 63) / 64;
			std::pmr::vector<std::uint64_t> targets(&data->memory), values(data->gates.size(), &data->memory);
			std::pmr::vector<index> ranks(&data->memory);
			targets.reserve(domain * words);
			ranks.reserve(domain);
			for (size_t base = 0; base < domain; base += 64)
			{
				budget.tick(data->gates.size() + data->requirements.size());
				evaluate_batch(*data, base, values);
				std::uint64_t allowed = ~std::uint64_t{};
				for (const index root : data->requirements) allowed &= values[root];
				for (size_t lane = 0; lane < std::min(size_t{64}, domain - base); ++lane)
				{
					budget.tick();
					if (!((allowed >> lane) & 1)) continue;
					const size_t row = ranks.size();
					ranks.push_back(static_cast<index>(base + lane));
					targets.resize(targets.size() + words);
					budget.tick(data->outputs.size());
					for (size_t bit = 0; bit < data->outputs.size(); ++bit)
						if ((values[data->outputs[bit]] >> lane) & 1)
							targets[row * words + bit / 64] |= std::uint64_t{1} << (bit % 64);
				}
			}
			if (statistics) statistics->allowed_assignments = ranks.size();
			finish_phase();
			phase = "tree";
			phase_started = std::chrono::steady_clock::now();
			std::pmr::vector<index> order(ranks.size(), &data->memory);
			for (size_t row = 0; row < ranks.size(); ++row) order[row] = static_cast<index>(row);
			auto compare_targets = [&](index left, index right)
			{
				for (size_t word = 0; word < words; ++word)
				{
					budget.tick();
					const auto a = targets[left * words + word], b = targets[right * words + word];
					if (a != b) return a < b ? -1 : 1;
				}
				return 0;
			};
			std::sort(order.begin(), order.end(), [&](index left, index right)
			{
				budget.tick();
				const int compared = compare_targets(left, right);
				return compared ? compared < 0 : ranks[left] < ranks[right];
			});
			std::pmr::vector<index> keys(&data->memory), leaf_order(&data->memory);
			data->assignments.reserve(ranks.size());
			for (size_t begin = 0; begin < order.size();)
			{
				size_t end = begin + 1;
				while (end < order.size() && !compare_targets(order[begin], order[end])) ++end;
				if (data->groups.size() >= std::min(options.max_nodes, max_tree))
					throw selector_limit("Selector node budget exceeded");
				keys.push_back(order[begin]);
				leaf_order.push_back(static_cast<index>(data->groups.size()));
				data->groups.push_back({static_cast<index>(begin), static_cast<index>(end - begin)});
				budget.tick(end - begin);
				for (size_t row = begin; row < end; ++row) data->assignments.push_back(ranks[order[row]]);
				begin = end;
			}
			if (statistics) statistics->distinct_outputs = data->groups.size();
			auto target_bit = [&](index leaf, size_t bit)
			{
				return ((targets[keys[leaf] * words + bit / 64] >> (bit % 64)) & 1) != 0;
			};
			std::function<index(size_t, size_t)> build = [&](size_t begin, size_t end) -> index
			{
				budget.tick();
				if (end - begin == 1) return leaf_flag | leaf_order[begin];
				size_t selected = output_ids.size(), best = end - begin;
				for (size_t bit = 0; bit < output_ids.size(); ++bit)
				{
					size_t high = 0;
					budget.tick(end - begin);
					for (size_t row = begin; row < end; ++row) high += target_bit(leaf_order[row], bit);
					const size_t imbalance = std::max(high, end - begin - high);
					if (imbalance < best) { selected = bit; best = imbalance; }
					if (best == (end - begin + 1) / 2) break;
				}
				if (selected == output_ids.size()) throw std::logic_error("Distinct selector targets cannot be separated");
				budget.tick(end - begin);
				const auto middle = std::partition(leaf_order.begin() + begin, leaf_order.begin() + end,
					[&](index leaf) { return !target_bit(leaf, selected); });
				const size_t split = static_cast<size_t>(middle - leaf_order.begin());
				const index low = build(begin, split), high = build(split, end);
				if (data->decisions.size() + data->groups.size() >= std::min(options.max_nodes, max_tree))
					throw selector_limit("Selector node budget exceeded");
				const index result = static_cast<index>(data->decisions.size());
				data->decisions.push_back({static_cast<index>(selected), low, high});
				return result;
			};
			if (!data->groups.empty()) data->root = build(0, data->groups.size());
			finish_phase();
			phase = "validation";
			phase_started = std::chrono::steady_clock::now();
			// Check every distinct reachable target against the completed tree.
			// Enumeration established each full group and its false-first witness.
			for (size_t leaf = 0; leaf < data->groups.size(); ++leaf)
			{
				index cursor = data->root;
				while (!(cursor & leaf_flag))
				{
					budget.tick();
					const auto current = data->decisions[cursor];
					cursor = target_bit(static_cast<index>(leaf), current.variable) ? current.high : current.low;
				}
				if ((cursor & ~leaf_flag) != leaf) throw std::logic_error("Selector exhaustive certification failed");
			}
			finish_phase();
			report();
			return selector_plan(std::move(data));
		}
		catch (...)
		{
			finish_phase();
			if (statistics) statistics->failed_phase = phase;
			report();
			throw;
		}
	}

	size_t input_count() const { return data_->inputs; }
	size_t output_count() const { return data_->target_bits; }
	size_t node_count() const { return data_->decisions.size() + data_->groups.size(); }
	size_t leaf_count() const { return data_->groups.size(); }
	size_t assignment_count() const { return data_->assignments.size(); }
	size_t forward_node_count() const { return data_->gates.size(); }
	size_t storage_bytes() const { return data_->memory.current(); }

	std::optional<std::vector<bool>> evaluate(const std::vector<bool>& target,
		selector_evaluation_statistics* statistics = nullptr) const
	{
		if (statistics) *statistics = {};
		if (target.size() != output_count()) throw std::invalid_argument("Wrong selector target width");
		const index leaf = descend(*data_, target, statistics);
		if (leaf == absent) return std::nullopt;
		const index rank = data_->assignments[data_->groups[leaf].begin];
		if (!verify(*data_, rank, target, statistics)) return std::nullopt;
		return unpack(*data_, rank);
	}

	size_t enumerate(const std::vector<bool>& target,
		const std::function<bool(const std::vector<bool>&)>& callback, size_t maximum = 1) const
	{
		if (target.size() != output_count()) throw std::invalid_argument("Wrong selector target width");
		if (!callback) throw std::invalid_argument("Missing selector enumeration callback");
		const index leaf = descend(*data_, target);
		if (leaf == absent) return 0;
		const auto selected = data_->groups[leaf];
		if (!verify(*data_, data_->assignments[selected.begin], target, nullptr)) return 0;
		size_t found = 0;
		for (size_t row = selected.begin; row < selected.begin + selected.count; ++row)
		{
			++found;
			if (!callback(unpack(*data_, data_->assignments[row])) || (maximum && found >= maximum)) break;
		}
		return found;
	}

	void save(std::ostream& stream) const
	{
		stream << "SELECTOR_PLAN 1 " << number(input_count()) << ' ' << number(output_count()) << ' '
			<< number(data_->decisions.size()) << ' ' << number(leaf_count()) << ' '
			<< number(assignment_count()) << ' ' << number(data_->root) << '\n';
		for (const auto node : data_->decisions)
			stream << number(node.variable) << ' ' << number(node.low) << ' ' << number(node.high) << '\n';
		for (const auto leaf : data_->groups)
			stream << number(leaf.begin) << ' ' << number(leaf.count) << '\n';
		for (const index rank : data_->assignments) stream << number(rank) << ' ';
		stream << "\nEND_SELECTOR_PLAN\n";
		if (!stream) throw std::runtime_error("Failed to write selector plan");
	}

	static selector_plan load(std::istream& stream, const circuit_type& circuit,
		const std::vector<size_t>& output_ids, const std::vector<size_t>& variable_ids,
		const std::vector<size_t>& requirement_ids)
	{
		expect(stream, "SELECTOR_PLAN");
		if (read_number(stream, 1) != 1) throw std::runtime_error("Unsupported selector plan version");
		const size_t inputs = read_number(stream, 20), outputs = read_number(stream, max_dimensions - inputs);
		if (inputs != variable_ids.size() || outputs != output_ids.size())
			throw std::runtime_error("Selector plan dimensions do not match its circuit");
		const size_t branches = read_number(stream, max_domain - 1);
		const size_t leaves = read_number(stream, max_domain);
		const size_t assignments = read_number(stream, size_t{1} << inputs);
		const index root = static_cast<index>(read_number(stream, absent));
		if ((leaves && branches + 1 != leaves) || (!leaves && (branches || assignments)) || assignments < leaves)
			throw std::runtime_error("Invalid selector tree dimensions");
		auto data = std::make_shared<storage>(load_memory_limit);
		work_budget budget{std::numeric_limits<size_t>::max()};
		prepare(*data, circuit, output_ids, variable_ids, requirement_ids, budget);
		data->root = root;
		data->decisions.reserve(branches);
		data->groups.reserve(leaves);
		data->assignments.reserve(assignments);
		auto valid_child = [&](index child, size_t preceding)
		{
			return child != absent && ((child & leaf_flag) ? (child & ~leaf_flag) < leaves : child < preceding);
		};
		for (size_t id = 0; id < branches; ++id)
		{
			if (!outputs) throw std::runtime_error("Selector decisions without target bits");
			const index variable = static_cast<index>(read_number(stream, outputs - 1));
			const index low = static_cast<index>(read_number(stream, absent));
			const index high = static_cast<index>(read_number(stream, absent));
			if (low == high || !valid_child(low, id) || !valid_child(high, id))
				throw std::runtime_error("Invalid selector decision");
			data->decisions.push_back({variable, low, high});
		}
		size_t next = 0;
		for (size_t id = 0; id < leaves; ++id)
		{
			const size_t begin = read_number(stream, assignments), count = read_number(stream, assignments);
			if (begin != next || !count || count > assignments - next)
				throw std::runtime_error("Invalid selector assignment group");
			data->groups.push_back({static_cast<index>(begin), static_cast<index>(count)});
			next += count;
		}
		if (next != assignments) throw std::runtime_error("Incomplete selector assignment groups");
		std::pmr::vector<unsigned char> seen_rank(size_t{1} << inputs, &data->memory);
		for (size_t row = 0; row < assignments; ++row)
		{
			const index rank = static_cast<index>(read_number(stream, (size_t{1} << inputs) - 1));
			if (seen_rank[rank]++) throw std::runtime_error("Duplicate selector assignment");
			data->assignments.push_back(rank);
		}
		for (const auto leaf : data->groups)
			for (size_t row = leaf.begin + 1; row < leaf.begin + leaf.count; ++row)
				if (data->assignments[row - 1] >= data->assignments[row])
					throw std::runtime_error("Selector group is not in canonical input order");
		expect(stream, "END_SELECTOR_PLAN");
		if ((!leaves && root != absent) || (leaves && !valid_child(root, branches)))
			throw std::runtime_error("Invalid selector root");
		std::pmr::vector<unsigned char> seen_node(branches + leaves, &data->memory);
		struct visit { index node; bool exit; };
		std::pmr::vector<visit> pending(&data->memory);
		std::array<bool, max_dimensions> path_bits{};
		if (leaves) pending.push_back({root, false});
		while (!pending.empty())
		{
			const auto next_visit = pending.back();
			pending.pop_back();
			const index current = next_visit.node;
			if (next_visit.exit)
			{
				path_bits[data->decisions[current].variable] = false;
				continue;
			}
			const size_t key = (current & leaf_flag) ? branches + (current & ~leaf_flag) : current;
			if (seen_node[key]++) throw std::runtime_error("Selector tree contains a shared child");
			if (!(current & leaf_flag))
			{
				const auto decision = data->decisions[current];
				if (path_bits[decision.variable]) throw std::runtime_error("Selector tree repeats a target bit along a path");
				path_bits[decision.variable] = true;
				pending.push_back({current, true});
				pending.push_back({decision.high, false});
				pending.push_back({decision.low, false});
			}
		}
		if (std::find(seen_node.begin(), seen_node.end(), 0) != seen_node.end())
			throw std::runtime_error("Selector tree contains unreachable nodes");
		return selector_plan(std::move(data));
	}

	void export_cpp(std::ostream& stream, const std::vector<size_t>& input_columns,
		const std::vector<bool>& input_constants) const
	{
		const size_t no_column = static_cast<size_t>(-1);
		if (input_columns.size() != input_constants.size()) throw std::invalid_argument("Invalid selector export input layout");
		for (const size_t column : input_columns)
			if (column != no_column && column >= input_count()) throw std::invalid_argument("Invalid selector export input column");
		stream << "// Generated bounded-domain selector with an exact forward check.\n"
			<< "#pragma once\n#include <array>\n#include <cstddef>\n#include <cstdint>\n#include <optional>\n#include <vector>\n\n"
			<< "inline std::optional<std::array<bool, " << number(input_columns.size()) << ">>\n"
			<< "bitreverse_inverse(const std::array<bool, " << number(output_count()) << ">& target)\n{\n";
		if (!leaf_count())
		{
			stream << "  (void)target;\n  return std::nullopt;\n}\n";
			if (!stream) throw std::runtime_error("Failed to export selector plan");
			return;
		}
		stream << "  struct decision { std::uint32_t variable, low, high; };\n"
			<< "  static constexpr std::array<decision, " << number(data_->decisions.size()) << "> decisions{{\n";
		for (const auto node : data_->decisions)
			stream << "    {" << number(node.variable) << "u, " << number(node.low) << "u, " << number(node.high) << "u},\n";
		stream << "  }};\n  static constexpr std::array<std::uint32_t, " << number(leaf_count()) << "> representatives{{";
		for (size_t leaf = 0; leaf < leaf_count(); ++leaf)
		{
			if (leaf) stream << ", ";
			stream << number(data_->assignments[data_->groups[leaf].begin]) << 'u';
		}
		stream << "}};\n  std::uint32_t cursor = " << number(data_->root) << "u;\n"
			<< "  while (!(cursor & 2147483648u)) {\n    const auto node = decisions[cursor];\n"
			<< "    cursor = target[node.variable] ? node.high : node.low;\n  }\n"
			<< "  const auto rank = representatives[cursor & 2147483647u];\n"
			<< "  struct gate { char operation; std::uint32_t first, second; };\n"
			<< "  static constexpr std::array<gate, " << number(data_->gates.size()) << "> gates{{\n";
		for (const auto step : data_->gates)
			stream << "    {'" << step.operation << "', " << number(step.first) << "u, " << number(step.second) << "u},\n";
		stream << "  }};\n  std::vector<unsigned char> values(" << number(data_->gates.size()) << ");\n"
			<< "  for (std::size_t id = 0; id < gates.size(); ++id) {\n    const auto step = gates[id];\n"
			<< "    switch (step.operation) {\n    case '=': values[id] = step.first != 0; break;\n"
			<< "    case '*': values[id] = (rank >> (" << number(input_count()) << "u - 1u - step.first)) & 1u; break;\n"
			<< "    case '!': values[id] = !values[step.first]; break;\n"
			<< "    case '&': values[id] = values[step.first] & values[step.second]; break;\n"
			<< "    case '|': values[id] = values[step.first] | values[step.second]; break;\n"
			<< "    default: values[id] = values[step.first] ^ values[step.second]; break;\n    }\n  }\n"
			<< "  static constexpr std::array<std::uint32_t, " << number(data_->requirements.size()) << "> requirements{{";
		for (size_t bit = 0; bit < data_->requirements.size(); ++bit)
		{
			if (bit) stream << ", ";
			stream << number(data_->requirements[bit]) << 'u';
		}
		stream << "}};\n  for (const auto root : requirements) if (!values[root]) return std::nullopt;\n"
			<< "  static constexpr std::array<std::uint32_t, " << number(output_count()) << "> outputs{{";
		for (size_t bit = 0; bit < output_count(); ++bit)
		{
			if (bit) stream << ", ";
			stream << number(data_->outputs[bit]) << 'u';
		}
		stream << "}};\n  for (std::size_t bit = 0; bit < outputs.size(); ++bit)\n"
			<< "    if (values[outputs[bit]] != target[bit]) return std::nullopt;\n"
			<< "  return std::array<bool, " << number(input_columns.size()) << ">{{";
		for (size_t bit = 0; bit < input_columns.size(); ++bit)
		{
			if (bit) stream << ", ";
			if (input_columns[bit] == no_column) stream << (input_constants[bit] ? "true" : "false");
			else stream << "((rank >> " << number(input_count() - 1 - input_columns[bit]) << "u) & 1u) != 0";
		}
		stream << "}};\n}\n";
		if (!stream) throw std::runtime_error("Failed to export selector plan");
	}
};

} // namespace detail
} // namespace dixelu::bitreverse::inversion

#endif
