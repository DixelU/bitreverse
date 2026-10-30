#ifndef DIXELU_BITREVERSE_INVERSE_SELECTOR_PLAN_H
#define DIXELU_BITREVERSE_INVERSE_SELECTOR_PLAN_H

#include "../bitreverse.h"
#include "counting_resource.h"
#include "forward_circuit.h"
#include "text_io.h"

#include <algorithm>
#include <array>
#include <chrono>
#include <cstdint>
#include <functional>
#include <istream>
#include <limits>
#include <memory_resource>
#include <numeric>
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
	size_t operations{}, peak_bytes{};
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

// Enumerates a bounded input domain through the forward circuit in 64-lane
// batches, groups equal outputs, and learns a target-bit decision tree. The
// tree needs no separate validity diagram: one forward evaluation checks its
// chosen input.
//
// A rank numbers an unknown assignment with column 0 as its most significant
// bit, so ascending ranks list assignments in false-first lexicographic order
// and the first rank of each group is its canonical input.
class selector_plan
{
	using circuit_type = collision_resolution::solver_core::compiled_circuit;
	using index = forward_circuit::index;
	using reader = text_reader<std::runtime_error>;
	static constexpr index leaf_flag = index{1} << 31;
	static constexpr index absent = std::numeric_limits<index>::max();
	static constexpr size_t max_unknowns = 20;
	static constexpr size_t max_domain = size_t{1} << max_unknowns;
	static constexpr size_t max_tree = max_domain * 2 - 1;
	static constexpr size_t max_dimensions = 512;
	static constexpr size_t load_memory_limit = 268435456;
	struct decision { index variable, low, high; };
	struct group { index begin, count; };

	struct storage
	{
		// The resource outlives every container that refers to it, including in
		// returned plans. max_bytes counts actual container allocation requests;
		// fixed objects, recursion frames and allocator overhead are excluded.
		counting_resource<selector_limit> memory;
		forward_circuit forward;
		std::pmr::vector<decision> decisions;
		std::pmr::vector<group> groups;
		std::pmr::vector<index> assignments;
		index root = absent;
		explicit storage(size_t limit)
			: memory(limit), forward(&memory), decisions(&memory), groups(&memory), assignments(&memory) {}
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
		if (inputs > max_unknowns || outputs > max_dimensions - inputs)
			throw selector_limit("Selector supports at most 20 unknown bits and 512 total input/output bits");
	}

	// The 64 lanes of the batch starting at rank base for one rank bit.
	static std::uint64_t input_word(size_t base, size_t exponent)
	{
		constexpr std::array<std::uint64_t, 6> patterns{
			0xaaaaaaaaaaaaaaaaULL, 0xccccccccccccccccULL, 0xf0f0f0f0f0f0f0f0ULL,
			0xff00ff00ff00ff00ULL, 0xffff0000ffff0000ULL, 0xffffffff00000000ULL};
		if (exponent < patterns.size()) return patterns[exponent];
		return ((base >> exponent) & 1) ? ~std::uint64_t{} : 0;
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
		const auto& forward = data.forward;
		std::vector<unsigned char> values(forward.gates.size());
		forward.evaluate(values, 1, [&](index column) { return (rank >> (forward.inputs - 1 - column)) & 1; });
		if (statistics) statistics->forward_gate_evaluations = values.size();
		return forward.accepts(values, target);
	}

	static std::vector<bool> unpack(const storage& data, index rank)
	{
		const size_t inputs = data.forward.inputs;
		std::vector<bool> result(inputs);
		for (size_t bit = 0; bit < inputs; ++bit)
			result[bit] = ((rank >> (inputs - 1 - bit)) & 1) != 0;
		return result;
	}

	// Construction work state. Its containers allocate from the plan's budgeted
	// resource and live until construction ends, so the byte budget and peak
	// cover every phase.
	struct builder
	{
		storage& data;
		work_budget& budget;
		size_t node_limit, words;
		// One packed target row per allowed rank, in rank order.
		std::pmr::vector<std::uint64_t> targets, values;
		std::pmr::vector<index> ranks, order, keys, leaf_order;

		builder(storage& plan, work_budget& work, size_t nodes)
			: data(plan), budget(work), node_limit(nodes),
			  words((plan.forward.outputs.size() + 63) / 64),
			  targets(&plan.memory), values(plan.forward.gates.size(), &plan.memory),
			  ranks(&plan.memory), order(&plan.memory), keys(&plan.memory),
			  leaf_order(&plan.memory) {}

		bool target_bit(index leaf, size_t bit) const
		{
			return ((targets[keys[leaf] * words + bit / 64] >> (bit % 64)) & 1) != 0;
		}

		int compare_targets(index left, index right)
		{
			for (size_t word = 0; word < words; ++word)
			{
				budget.tick();
				const auto a = targets[left * words + word], b = targets[right * words + word];
				if (a != b) return a < b ? -1 : 1;
			}
			return 0;
		}

		// Records every allowed rank and its target, 64 ranks per forward pass.
		void enumerate(size_t domain)
		{
			const auto& forward = data.forward;
			targets.reserve(domain * words);
			ranks.reserve(domain);
			for (size_t base = 0; base < domain; base += 64)
			{
				budget.tick(forward.gates.size() + forward.requirements.size());
				forward.evaluate(values, ~std::uint64_t{},
					[&](index column) { return input_word(base, forward.inputs - 1 - column); });
				std::uint64_t allowed = ~std::uint64_t{};
				for (const index root : forward.requirements) allowed &= values[root];
				for (size_t lane = 0; lane < std::min(size_t{64}, domain - base); ++lane)
				{
					budget.tick();
					if (!((allowed >> lane) & 1)) continue;
					const size_t row = ranks.size();
					ranks.push_back(static_cast<index>(base + lane));
					targets.resize(targets.size() + words);
					budget.tick(forward.outputs.size());
					for (size_t bit = 0; bit < forward.outputs.size(); ++bit)
						if ((values[forward.outputs[bit]] >> lane) & 1)
							targets[row * words + bit / 64] |= std::uint64_t{1} << (bit % 64);
				}
			}
		}

		// Sorts allowed ranks by target, then rank, and makes one leaf per
		// distinct target holding its ranks in ascending order.
		void group()
		{
			order.resize(ranks.size());
			std::iota(order.begin(), order.end(), index{0});
			std::sort(order.begin(), order.end(), [&](index left, index right)
			{
				budget.tick();
				const int compared = compare_targets(left, right);
				return compared ? compared < 0 : ranks[left] < ranks[right];
			});
			data.assignments.reserve(ranks.size());
			for (size_t begin = 0; begin < order.size();)
			{
				size_t end = begin + 1;
				while (end < order.size() && !compare_targets(order[begin], order[end])) ++end;
				if (data.groups.size() >= node_limit) throw selector_limit("Selector node budget exceeded");
				keys.push_back(order[begin]);
				leaf_order.push_back(static_cast<index>(data.groups.size()));
				data.groups.push_back({static_cast<index>(begin), static_cast<index>(end - begin)});
				budget.tick(end - begin);
				for (size_t row = begin; row < end; ++row) data.assignments.push_back(ranks[order[row]]);
				begin = end;
			}
		}

		// Builds the subtree separating leaf_order[begin, end) by the target
		// bit that divides those leaves most evenly.
		index split(size_t begin, size_t end)
		{
			budget.tick();
			if (end - begin == 1) return leaf_flag | leaf_order[begin];
			const size_t bits = data.forward.outputs.size();
			size_t selected = bits, best = end - begin;
			for (size_t bit = 0; bit < bits; ++bit)
			{
				size_t high = 0;
				budget.tick(end - begin);
				for (size_t row = begin; row < end; ++row) high += target_bit(leaf_order[row], bit);
				const size_t imbalance = std::max(high, end - begin - high);
				if (imbalance < best) { selected = bit; best = imbalance; }
				if (best == (end - begin + 1) / 2) break;
			}
			if (selected == bits) throw std::logic_error("Distinct selector targets cannot be separated");
			budget.tick(end - begin);
			const auto middle = std::partition(leaf_order.begin() + begin, leaf_order.begin() + end,
				[&](index leaf) { return !target_bit(leaf, selected); });
			const size_t boundary = static_cast<size_t>(middle - leaf_order.begin());
			const index low = split(begin, boundary), high = split(boundary, end);
			if (data.decisions.size() + data.groups.size() >= node_limit)
				throw selector_limit("Selector node budget exceeded");
			const index result = static_cast<index>(data.decisions.size());
			data.decisions.push_back({static_cast<index>(selected), low, high});
			return result;
		}

		// Checks every distinct reachable target against the completed tree.
		// Enumeration established each full group and its false-first witness.
		void certify()
		{
			for (size_t leaf = 0; leaf < data.groups.size(); ++leaf)
			{
				index cursor = data.root;
				while (!(cursor & leaf_flag))
				{
					budget.tick();
					const auto current = data.decisions[cursor];
					cursor = target_bit(static_cast<index>(leaf), current.variable) ? current.high : current.low;
				}
				if ((cursor & ~leaf_flag) != leaf) throw std::logic_error("Selector exhaustive certification failed");
			}
		}
	};

	// A child is a leaf or a decision stored before its parent.
	static bool valid_child(index child, size_t preceding, size_t leaves)
	{
		return child != absent && ((child & leaf_flag) ? (child & ~leaf_flag) < leaves : child < preceding);
	}

	static void read_decisions(reader& input, storage& data, size_t branches, size_t leaves)
	{
		const size_t outputs = data.forward.outputs.size();
		for (size_t id = 0; id < branches; ++id)
		{
			if (!outputs) throw std::runtime_error("Selector decisions without target bits");
			const index variable = static_cast<index>(input.number(outputs - 1));
			const index low = static_cast<index>(input.number(absent));
			const index high = static_cast<index>(input.number(absent));
			if (low == high || !valid_child(low, id, leaves) || !valid_child(high, id, leaves))
				throw std::runtime_error("Invalid selector decision");
			data.decisions.push_back({variable, low, high});
		}
	}

	static void read_groups(reader& input, storage& data, size_t leaves, size_t assignments)
	{
		size_t next = 0;
		for (size_t id = 0; id < leaves; ++id)
		{
			const size_t begin = input.number(assignments), count = input.number(assignments);
			if (begin != next || !count || count > assignments - next)
				throw std::runtime_error("Invalid selector assignment group");
			data.groups.push_back({static_cast<index>(begin), static_cast<index>(count)});
			next += count;
		}
		if (next != assignments) throw std::runtime_error("Incomplete selector assignment groups");
	}

	static void read_assignments(reader& input, storage& data, size_t assignments)
	{
		const size_t domain = size_t{1} << data.forward.inputs;
		std::pmr::vector<unsigned char> seen(domain, &data.memory);
		for (size_t row = 0; row < assignments; ++row)
		{
			const index rank = static_cast<index>(input.number(domain - 1));
			if (seen[rank]++) throw std::runtime_error("Duplicate selector assignment");
			data.assignments.push_back(rank);
		}
		for (const auto leaf : data.groups)
			for (size_t row = leaf.begin + 1; row < leaf.begin + leaf.count; ++row)
				if (data.assignments[row - 1] >= data.assignments[row])
					throw std::runtime_error("Selector group is not in canonical input order");
	}

	// Every node must be reached exactly once from the root, and no path may
	// test a target bit twice.
	static void check_tree(storage& data, size_t branches, size_t leaves)
	{
		const index root = data.root;
		if ((!leaves && root != absent) || (leaves && !valid_child(root, branches, leaves)))
			throw std::runtime_error("Invalid selector root");
		std::pmr::vector<unsigned char> seen(branches + leaves, &data.memory);
		struct visit { index node; bool exit; };
		std::pmr::vector<visit> pending(&data.memory);
		std::array<bool, max_dimensions> path_bits{};
		if (leaves) pending.push_back({root, false});
		while (!pending.empty())
		{
			const auto next = pending.back();
			pending.pop_back();
			const index current = next.node;
			if (next.exit)
			{
				path_bits[data.decisions[current].variable] = false;
				continue;
			}
			const size_t key = (current & leaf_flag) ? branches + (current & ~leaf_flag) : current;
			if (seen[key]++) throw std::runtime_error("Selector tree contains a shared child");
			if (!(current & leaf_flag))
			{
				const auto decision = data.decisions[current];
				if (path_bits[decision.variable]) throw std::runtime_error("Selector tree repeats a target bit along a path");
				path_bits[decision.variable] = true;
				pending.push_back({current, true});
				pending.push_back({decision.high, false});
				pending.push_back({decision.low, false});
			}
		}
		if (std::find(seen.begin(), seen.end(), 0) != seen.end())
			throw std::runtime_error("Selector tree contains unreachable nodes");
	}

public:
	static selector_plan compile(const circuit_type& circuit,
		const std::vector<size_t>& output_ids, const std::vector<size_t>& variable_ids,
		const std::vector<size_t>& requirement_ids, const selector_options& options,
		selector_statistics* statistics)
	{
		using phase_time = std::chrono::nanoseconds selector_statistics::*;
		const auto started = std::chrono::steady_clock::now();
		if (statistics) *statistics = {};
		work_budget budget{options.max_operations};
		std::shared_ptr<storage> data;
		std::string_view phase = "enumeration";
		phase_time phase_elapsed = &selector_statistics::enumeration_elapsed;
		auto phase_started = started;
		auto finish_phase = [&]
		{
			if (statistics) statistics->*phase_elapsed = std::chrono::steady_clock::now() - phase_started;
		};
		auto begin_phase = [&](std::string_view name, phase_time elapsed)
		{
			finish_phase();
			phase = name;
			phase_elapsed = elapsed;
			phase_started = std::chrono::steady_clock::now();
		};
		auto report = [&]
		{
			finish_phase();
			if (!statistics) return;
			statistics->operations = budget.operations;
			statistics->elapsed = std::chrono::steady_clock::now() - started;
			if (data) statistics->peak_bytes = data->memory.peak();
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
			data->forward.build(circuit, output_ids, variable_ids, requirement_ids,
				[&](size_t amount) { budget.tick(amount); });
			builder work(*data, budget, std::min(options.max_nodes, max_tree));
			work.enumerate(domain);
			if (statistics) statistics->allowed_assignments = work.ranks.size();
			begin_phase("tree", &selector_statistics::tree_elapsed);
			work.group();
			if (statistics) statistics->distinct_outputs = data->groups.size();
			if (!data->groups.empty()) data->root = work.split(0, data->groups.size());
			begin_phase("validation", &selector_statistics::validation_elapsed);
			work.certify();
			report();
			return selector_plan(std::move(data));
		}
		catch (...)
		{
			if (statistics) statistics->failed_phase = phase;
			report();
			throw;
		}
	}

	size_t input_count() const { return data_->forward.inputs; }
	size_t output_count() const { return data_->forward.outputs.size(); }
	size_t node_count() const { return data_->decisions.size() + data_->groups.size(); }
	size_t leaf_count() const { return data_->groups.size(); }
	size_t assignment_count() const { return data_->assignments.size(); }
	size_t forward_node_count() const { return data_->forward.gates.size(); }

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
		const std::function<bool(const std::vector<bool>&)>& callback, size_t maximum) const
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
		stream << "SELECTOR_PLAN 1 " << decimal(input_count()) << ' ' << decimal(output_count()) << ' '
			<< decimal(data_->decisions.size()) << ' ' << decimal(leaf_count()) << ' '
			<< decimal(assignment_count()) << ' ' << decimal(data_->root) << '\n';
		for (const auto node : data_->decisions)
			stream << decimal(node.variable) << ' ' << decimal(node.low) << ' ' << decimal(node.high) << '\n';
		for (const auto leaf : data_->groups)
			stream << decimal(leaf.begin) << ' ' << decimal(leaf.count) << '\n';
		write_fields(stream, data_->assignments);
		stream << "\nEND_SELECTOR_PLAN\n";
		if (!stream) throw std::runtime_error("Failed to write selector plan");
	}

	static selector_plan load(std::istream& stream, const circuit_type& circuit,
		const std::vector<size_t>& output_ids, const std::vector<size_t>& variable_ids,
		const std::vector<size_t>& requirement_ids)
	{
		reader input(stream, "selector plan");
		input.expect("SELECTOR_PLAN");
		if (input.number(1) != 1) throw std::runtime_error("Unsupported selector plan version");
		const size_t inputs = input.number(max_unknowns), outputs = input.number(max_dimensions - inputs);
		if (inputs != variable_ids.size() || outputs != output_ids.size())
			throw std::runtime_error("Selector plan dimensions do not match its circuit");
		const size_t branches = input.number(max_domain - 1);
		const size_t leaves = input.number(max_domain);
		const size_t assignments = input.number(size_t{1} << inputs);
		const index root = static_cast<index>(input.number(absent));
		if ((leaves && branches + 1 != leaves) || (!leaves && (branches || assignments)) || assignments < leaves)
			throw std::runtime_error("Invalid selector tree dimensions");
		auto data = std::make_shared<storage>(load_memory_limit);
		data->forward.build(circuit, output_ids, variable_ids, requirement_ids, [](size_t) {});
		data->root = root;
		data->decisions.reserve(branches);
		data->groups.reserve(leaves);
		data->assignments.reserve(assignments);
		read_decisions(input, *data, branches, leaves);
		read_groups(input, *data, leaves, assignments);
		read_assignments(input, *data, assignments);
		input.expect("END_SELECTOR_PLAN");
		check_tree(*data, branches, leaves);
		return selector_plan(std::move(data));
	}

	void export_cpp(std::ostream& stream, const cpp_signature& signature) const
	{
		signature.begin(stream, "Generated bounded-domain selector with an exact forward check.",
			{"array", "cstddef", "cstdint", "optional", "vector"});
		if (!leaf_count())
		{
			stream << "  (void)target;\n  return std::nullopt;\n}\n";
			return;
		}
		const auto& forward = data_->forward;
		stream << "  struct decision { std::uint32_t variable, low, high; };\n"
			<< "  static constexpr std::array<decision, " << decimal(data_->decisions.size()) << "> decisions{{\n";
		for (const auto node : data_->decisions)
			stream << "    {" << decimal(node.variable) << "u, " << decimal(node.low) << "u, " << decimal(node.high) << "u},\n";
		stream << "  }};\n  static constexpr std::array<std::uint32_t, " << decimal(leaf_count()) << "> representatives{{";
		for (size_t leaf = 0; leaf < leaf_count(); ++leaf)
		{
			if (leaf) stream << ", ";
			stream << decimal(data_->assignments[data_->groups[leaf].begin]) << 'u';
		}
		stream << "}};\n  std::uint32_t cursor = " << decimal(data_->root) << "u;\n"
			<< "  while (!(cursor & 2147483648u)) {\n    const auto node = decisions[cursor];\n"
			<< "    cursor = target[node.variable] ? node.high : node.low;\n  }\n"
			<< "  const auto rank = representatives[cursor & 2147483647u];\n"
			<< "  struct gate { char operation; std::uint32_t first, second; };\n"
			<< "  static constexpr std::array<gate, " << decimal(forward.gates.size()) << "> gates{{\n";
		for (const auto step : forward.gates)
			stream << "    {'" << step.operation << "', " << decimal(step.first) << "u, " << decimal(step.second) << "u},\n";
		stream << "  }};\n  std::vector<unsigned char> values(" << decimal(forward.gates.size()) << ");\n"
			<< "  for (std::size_t id = 0; id < gates.size(); ++id) {\n    const auto step = gates[id];\n"
			<< "    switch (step.operation) {\n    case '=': values[id] = step.first != 0; break;\n"
			<< "    case '*': values[id] = (rank >> (" << decimal(input_count()) << "u - 1u - step.first)) & 1u; break;\n"
			<< "    case '!': values[id] = !values[step.first]; break;\n"
			<< "    case '&': values[id] = values[step.first] & values[step.second]; break;\n"
			<< "    case '|': values[id] = values[step.first] | values[step.second]; break;\n"
			<< "    default: values[id] = values[step.first] ^ values[step.second]; break;\n    }\n  }\n"
			<< "  static constexpr std::array<std::uint32_t, " << decimal(forward.requirements.size()) << "> requirements{{";
		for (size_t bit = 0; bit < forward.requirements.size(); ++bit)
		{
			if (bit) stream << ", ";
			stream << decimal(forward.requirements[bit]) << 'u';
		}
		stream << "}};\n  for (const auto root : requirements) if (!values[root]) return std::nullopt;\n"
			<< "  static constexpr std::array<std::uint32_t, " << decimal(output_count()) << "> outputs{{";
		for (size_t bit = 0; bit < output_count(); ++bit)
		{
			if (bit) stream << ", ";
			stream << decimal(forward.outputs[bit]) << 'u';
		}
		stream << "}};\n  for (std::size_t bit = 0; bit < outputs.size(); ++bit)\n"
			<< "    if (values[outputs[bit]] != target[bit]) return std::nullopt;\n";
		signature.end(stream, [&](size_t column)
		{
			stream << "((rank >> " << decimal(input_count() - 1 - column) << "u) & 1u) != 0";
		});
	}
};

} // namespace detail
} // namespace dixelu::bitreverse::inversion

#endif
