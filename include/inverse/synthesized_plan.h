#ifndef DIXELU_BITREVERSE_INVERSE_SYNTHESIZED_PLAN_H
#define DIXELU_BITREVERSE_INVERSE_SYNTHESIZED_PLAN_H

#include "../bitreverse.h"
#include "counting_resource.h"
#include "synthesis_profile.h"
#include "text_io.h"

#include <algorithm>
#include <chrono>
#include <functional>
#include <istream>
#include <optional>
#include <ostream>
#include <span>
#include <unordered_map>
#include <unordered_set>

namespace dixelu::bitreverse::inversion
{

struct synthesis_options
{
	size_t max_nodes = 750000;
	size_t max_operations = 10000000;
};

struct evaluation_statistics
{
	// Counts executed decisions, not Boolean gates. Lazy evaluation may visit a
	// shared decision through several roots. The schedule evaluates it once,
	// charging shared validity/selector decisions to the validity subtotal.
	size_t decision_visits{};
	size_t validity_visits{};
	size_t selector_visits{};
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
	using circuit_type = collision_resolution::solver_core::compiled_circuit;
	using memory_resource = counting_resource<synthesis_limit>;
	static constexpr size_t absent = static_cast<size_t>(-1);
	static constexpr size_t max_dimension = 512;

	struct decision
	{
		size_t variable, low, high;
		bool operator==(const decision&) const = default;
	};
	struct pair_key
	{
		size_t first, second;
		bool operator==(const pair_key&) const = default;
	};
	static size_t combine(size_t seed, size_t value)
	{
		return seed ^ (value + size_t{0x9e3779b9} + (seed << 6) + (seed >> 2));
	}
	struct decision_hash
	{
		size_t operator()(const decision& value) const
		{
			return combine(combine(value.variable, value.low), value.high);
		}
	};
	struct pair_hash
	{
		size_t operator()(const pair_key& key) const { return combine(key.first, key.second); }
	};

	class manager
	{
		const synthesis_options& options_;
		std::pmr::unordered_map<decision, size_t, decision_hash> unique_;
		std::pmr::unordered_map<decision, size_t, decision_hash> apply_cache_;
	public:
		std::pmr::vector<decision> nodes;
		size_t operations{};

		explicit manager(const synthesis_options& options, std::pmr::memory_resource* memory)
			: options_(options), unique_(memory), apply_cache_(memory),
			  nodes({{absent, 0, 0}, {absent, 1, 1}}, memory) {}
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
			if (nodes.size() - 2 >= options_.max_nodes || nodes.size() - 2 >= artifact_node_limit)
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
	std::vector<decision> function_steps_;
	std::vector<size_t> function_results_;
	size_t function_valid_{};
	size_t relation_nodes_{};
	size_t selector_nodes_{};
	size_t validity_nodes_{};

	static std::vector<unsigned char> reachable(
		std::span<const decision> nodes, const std::vector<size_t>& roots,
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

	// The number of decisions (terminals excluded) reachable from roots.
	static size_t reachable_count(std::span<const decision> nodes,
		const std::vector<size_t>& roots, manager* budget = nullptr)
	{
		const auto seen = reachable(nodes, roots, budget);
		return static_cast<size_t>(std::count(seen.begin() + 2, seen.end(), 1));
	}

	// Renumbers the target-only DAG of the functions and the validity predicate
	// into a children-first schedule, and counts each retained subgraph.
	void index_functions(manager* budget = nullptr)
	{
		auto roots = functions_;
		roots.push_back(valid_);
		const auto seen = reachable(nodes_, roots, budget);
		if (budget) budget->tick(nodes_.size());
		std::vector<size_t> remap(nodes_.size());
		remap[1] = 1;
		function_steps_.clear();
		for (size_t id = 2; id < nodes_.size(); ++id)
		{
			if (!seen[id]) continue;
			const auto current = nodes_[id];
			if (current.variable >= output_count_)
				throw std::runtime_error("Synthesized function depends on an input variable");
			remap[id] = function_steps_.size() + 2;
			function_steps_.push_back({current.variable, remap[current.low], remap[current.high]});
		}
		function_valid_ = remap[valid_];
		function_results_.clear();
		for (const size_t root : functions_) function_results_.push_back(remap[root]);
		relation_nodes_ = reachable_count(nodes_, {relation_}, budget);
		selector_nodes_ = reachable_count(nodes_, functions_, budget);
		validity_nodes_ = reachable_count(nodes_, {valid_}, budget);
	}

	// Records each construction phase's graph growth, work and tracked memory.
	class profiler
	{
		synthesis_statistics* statistics_;
		const manager& bdd_;
		memory_resource& memory_;
		std::chrono::steady_clock::time_point started_ = std::chrono::steady_clock::now();
		std::chrono::steady_clock::time_point phase_started_ = started_;
		synthesis_phase phase_ = synthesis_phase::forward;
		size_t phase_nodes_{}, phase_operations_{};

		void close(bool completed, const std::vector<size_t>& live_roots)
		{
			if (!statistics_) return;
			auto& record = statistics_->phases[static_cast<size_t>(phase_)];
			if (record.completed) return;
			record.elapsed = std::chrono::steady_clock::now() - phase_started_;
			record.created_nodes = bdd_.nodes.size() - 2 - phase_nodes_;
			record.operations = bdd_.operations - phase_operations_;
			record.resident_nodes = bdd_.nodes.size() - 2;
			record.tracked_bytes = memory_.current();
			record.peak_tracked_bytes = memory_.phase_peak();
			if (completed) record.live_nodes = reachable_count(bdd_.nodes, live_roots);
			record.completed = completed;
		}

		void report()
		{
			if (!statistics_) return;
			statistics_->created_nodes = bdd_.nodes.size() - 2;
			statistics_->operations = bdd_.operations;
			statistics_->elapsed = std::chrono::steady_clock::now() - started_;
			statistics_->peak_tracked_bytes = memory_.peak();
		}

	public:
		profiler(synthesis_statistics* statistics, const manager& bdd, memory_resource& memory)
			: statistics_(statistics), bdd_(bdd), memory_(memory)
		{
			if (statistics_)
			{
				*statistics_ = {};
				statistics_->phases[0].started = true;
			}
			memory_.begin_phase();
		}

		// Completes the current phase, whose outputs are live_roots, and starts next.
		void advance(const std::vector<size_t>& live_roots, synthesis_phase next)
		{
			close(true, live_roots);
			phase_ = next;
			phase_started_ = std::chrono::steady_clock::now();
			phase_nodes_ = bdd_.nodes.size() - 2;
			phase_operations_ = bdd_.operations;
			memory_.begin_phase();
			if (statistics_) statistics_->phases[static_cast<size_t>(phase_)].started = true;
		}

		void finish(const std::vector<size_t>& live_roots)
		{
			close(true, live_roots);
			report();
		}

		void fail()
		{
			if (statistics_) statistics_->failed_phase = phase_;
			close(false, {});
			report();
		}
	};

	// Construction work state. Its containers allocate from the tracked
	// resource and live until construction ends, so each phase's memory
	// statistics include the work vectors kept from earlier phases.
	class builder
	{
		struct frame { size_t id, next; };
		struct witness
		{
			size_t valid;
			std::pmr::vector<size_t> bits;
			witness(size_t value, size_t width, std::pmr::memory_resource* resource)
				: valid(value), bits(width, resource) {}
			// A plain pmr copy would allocate from the default resource instead.
			witness(const witness& other)
				: valid(other.valid), bits(other.bits, other.bits.get_allocator()) {}
			witness(witness&&) = default;
		};

		const circuit_type& circuit_;
		const std::vector<size_t>& output_ids_;
		const std::vector<size_t>& variable_ids_;
		const std::vector<size_t>& requirement_ids_;
		const synthesis_options& options_;
		manager& bdd_;
		std::pmr::memory_resource* memory_;
		std::pmr::vector<size_t> columns_, forward_, roots_;
		std::pmr::vector<unsigned char> state_;
		std::pmr::vector<frame> stack_;
		std::pmr::vector<size_t> complements_;
		std::pmr::unordered_map<pair_key, size_t, pair_hash> relation_cache_;
		std::pmr::unordered_map<size_t, witness> witness_cache_;
		std::optional<witness> canonical_;
		std::pmr::vector<size_t> remap_;

		size_t gate(size_t id, char operation)
		{
			const auto& children = circuit_.inputs[id];
			if (operation == '=')
				return circuit_.nodes[id]->state != 0;
			if (operation == '*')
			{
				if (columns_[id] == absent)
					throw std::invalid_argument("Undeclared synthesis input variable");
				return bdd_.make(output_ids_.size() + columns_[id], 0, 1);
			}
			if (operation == '!')
				return bdd_.negate(forward_[children[0]]);
			return bdd_.apply(operation, forward_[children[0]], forward_[children[1]]);
		}

		void build_cone(size_t root)
		{
			const size_t count = circuit_.nodes.size();
			stack_.push_back({root, 0});
			state_[root] = 1;
			while (!stack_.empty())
			{
				bdd_.tick();
				auto& current = stack_.back();
				const size_t id = current.id;
				const char operation = static_cast<char>(circuit_.nodes[id]->operation);
				if (operation != '=' && operation != '*' && operation != '!' &&
					operation != '&' && operation != '|' && operation != '^')
					throw std::invalid_argument("Unsupported synthesis gate");
				const size_t arity = details::operation_args_count[static_cast<unsigned char>(operation)];
				if (current.next < arity)
				{
					const size_t child = circuit_.inputs[id][current.next++];
					if (child >= count) throw std::invalid_argument("Invalid synthesis child");
					if (state_[child] == 1) throw std::invalid_argument("Cyclic synthesis circuit");
					if (state_[child] == 0) { state_[child] = 1; stack_.push_back({child, 0}); }
					continue;
				}
				forward_[id] = gate(id, operation);
				state_[id] = 2;
				stack_.pop_back();
			}
		}

		// Splits subset by each remaining target bit's output value.
		size_t relate(size_t index, size_t subset)
		{
			bdd_.tick();
			if (subset == 0 || index == output_ids_.size()) return subset;
			const pair_key key{index, subset};
			if (const auto found = relation_cache_.find(key); found != relation_cache_.end())
				return found->second;
			const size_t low_domain = bdd_.apply('&', subset, complements_[index]);
			const size_t high_domain = bdd_.apply('&', subset, forward_[output_ids_[index]]);
			const size_t low = relate(index + 1, low_domain);
			const size_t high = relate(index + 1, high_domain);
			const size_t result = bdd_.make(index, low, high);
			if (relation_cache_.size() >= options_.max_nodes) relation_cache_.clear();
			relation_cache_.emplace(key, result);
			return result;
		}

		witness synthesize(size_t id)
		{
			const size_t inputs = variable_ids_.size(), outputs = output_ids_.size();
			bdd_.tick(inputs + 1);
			if (const auto found = witness_cache_.find(id); found != witness_cache_.end())
				return found->second;
			witness result{static_cast<size_t>(id != 0), inputs, memory_};
			if (id >= 2 && bdd_.nodes[id].variable < outputs)
			{
				const auto current = bdd_.nodes[id];
				const auto low = synthesize(current.low), high = synthesize(current.high);
				result.valid = bdd_.make(current.variable, low.valid, high.valid);
				for (size_t bit = 0; bit < inputs; ++bit)
				{
					bdd_.tick();
					if (low.valid == 0) result.bits[bit] = high.bits[bit];
					else if (high.valid == 0) result.bits[bit] = low.bits[bit];
					else result.bits[bit] = bdd_.make(current.variable, low.bits[bit], high.bits[bit]);
				}
			}
			else
			{
				// A nonzero input-domain BDD always has a satisfying path.
				// Prefer low edges and leave skipped variables at zero.
				for (size_t cursor = id; cursor >= 2;)
				{
					bdd_.tick();
					const auto current = bdd_.nodes[cursor];
					const bool high = current.low == 0;
					result.bits[current.variable - outputs] = high;
					cursor = high ? current.high : current.low;
				}
			}
			witness_cache_.emplace(id, result);
			return result;
		}

	public:
		builder(const circuit_type& circuit, const std::vector<size_t>& output_ids,
			const std::vector<size_t>& variable_ids, const std::vector<size_t>& requirement_ids,
			const synthesis_options& options, manager& bdd, std::pmr::memory_resource* memory)
			: circuit_(circuit), output_ids_(output_ids), variable_ids_(variable_ids),
			  requirement_ids_(requirement_ids), options_(options), bdd_(bdd), memory_(memory),
			  columns_(memory), forward_(memory), roots_(memory), state_(memory), stack_(memory),
			  complements_(memory), relation_cache_(memory), witness_cache_(memory), remap_(memory) {}

		// Builds the forward BDD of every node the outputs and requirements
		// reach, and returns the BDDs of those roots.
		std::vector<size_t> forward()
		{
			const size_t count = circuit_.nodes.size();
			bdd_.tick(count);
			columns_.assign(count, absent);
			forward_.resize(count);
			for (size_t column = 0; column < variable_ids_.size(); ++column)
			{
				bdd_.tick();
				const size_t id = variable_ids_[column];
				if (id >= count || circuit_.nodes[id]->operation != '*' || columns_[id] != absent)
					throw std::invalid_argument("Invalid synthesis input variable");
				columns_[id] = column;
			}
			roots_.assign(output_ids_.begin(), output_ids_.end());
			bdd_.tick(requirement_ids_.size());
			roots_.insert(roots_.end(), requirement_ids_.begin(), requirement_ids_.end());
			state_.resize(count);
			for (const size_t root : roots_)
			{
				bdd_.tick();
				if (root >= count) throw std::invalid_argument("Invalid synthesis root");
				if (state_[root] != 2) build_cone(root);
			}
			std::vector<size_t> result;
			for (const size_t id : roots_) result.push_back(forward_[id]);
			return result;
		}

		// The exact relation over target then input variables, restricted to
		// inputs that meet every requirement.
		size_t relation()
		{
			size_t domain = 1;
			for (const size_t id : requirement_ids_) domain = bdd_.apply('&', domain, forward_[id]);
			for (const size_t id : output_ids_) complements_.push_back(bdd_.negate(forward_[id]));
			return relate(0, domain);
		}

		// Eliminates the input variables of a relation: a validity predicate
		// over targets, and one target-only function per input bit.
		const witness& canonical(size_t relation)
		{
			canonical_.emplace(synthesize(relation));
			return *canonical_;
		}

		// Moves the decisions reachable from roots into plan, renumbered in order.
		void compact(synthesized_plan& plan, const std::vector<size_t>& roots)
		{
			const auto used = reachable(bdd_.nodes, roots, &bdd_);
			bdd_.tick(bdd_.nodes.size());
			remap_.resize(bdd_.nodes.size());
			remap_[1] = 1;
			for (size_t id = 2; id < bdd_.nodes.size(); ++id)
				if (used[id])
				{
					const auto current = bdd_.nodes[id];
					remap_[id] = plan.nodes_.size();
					plan.nodes_.push_back({current.variable, remap_[current.low], remap_[current.high]});
				}
			plan.relation_ = remap_[plan.relation_];
			plan.valid_ = remap_[plan.valid_];
			for (auto& function : plan.functions_) function = remap_[function];
		}
	};

public:
	static synthesized_plan compile(
		const circuit_type& circuit,
		const std::vector<size_t>& output_ids,
		const std::vector<size_t>& variable_ids,
		const std::vector<size_t>& requirement_ids,
		const synthesis_options& options,
		synthesis_statistics* statistics)
	{
		memory_resource memory;
		manager bdd(options, &memory);
		profiler profile(statistics, bdd, memory);
		try
		{
			if (!options.max_nodes || !options.max_operations)
				throw synthesis_limit("Boolean synthesis budgets must be positive");
			if (output_ids.size() > max_dimension || variable_ids.size() > max_dimension - output_ids.size())
				throw synthesis_limit("Boolean synthesis supports at most 512 total input and output bits");
			builder work(circuit, output_ids, variable_ids, requirement_ids, options, bdd, &memory);
			profile.advance(work.forward(), synthesis_phase::relation);
			synthesized_plan plan;
			plan.input_count_ = variable_ids.size();
			plan.output_count_ = output_ids.size();
			plan.relation_ = work.relation();
			profile.advance({plan.relation_}, synthesis_phase::witness);
			const auto& canonical = work.canonical(plan.relation_);
			plan.valid_ = canonical.valid;
			plan.functions_.assign(canonical.bits.begin(), canonical.bits.end());
			auto roots = plan.functions_;
			roots.push_back(plan.valid_);
			roots.push_back(plan.relation_);
			profile.advance(roots, synthesis_phase::compaction);
			work.compact(plan, roots);
			plan.index_functions(&bdd);
			profile.finish(roots);
			return plan;
		}
		catch (...)
		{
			profile.fail();
			throw;
		}
	}

	size_t input_count() const { return input_count_; }
	size_t output_count() const { return output_count_; }
	size_t node_count() const { return nodes_.size() - 2; }
	size_t relation_node_count() const { return relation_nodes_; }
	size_t function_node_count() const { return function_steps_.size(); }
	size_t selector_node_count() const { return selector_nodes_; }
	size_t validity_node_count() const { return validity_nodes_; }

	std::optional<std::vector<bool>> evaluate(const std::vector<bool>& target,
		evaluation_statistics* statistics = nullptr) const
	{
		if (statistics) *statistics = {};
		if (target.size() != output_count_) throw std::invalid_argument("Wrong synthesized target width");
		evaluation_statistics visits;
		auto follow = [&](size_t root, size_t& count)
		{
			while (root >= 2)
			{
				const auto current = function_steps_[root - 2];
				root = target[current.variable] ? current.high : current.low;
				++count;
			}
			return root != 0;
		};
		// Every root depends only on the supplied target. Following its selected
		// edges is ordinary function evaluation, with no input choices or search.
		if (!follow(function_valid_, visits.validity_visits))
		{
			visits.decision_visits = visits.validity_visits;
			if (statistics) *statistics = visits;
			return std::nullopt;
		}
		std::vector<bool> result(input_count_);
		for (size_t bit = 0; bit < input_count_; ++bit)
			result[bit] = follow(function_results_[bit], visits.selector_visits);
		visits.decision_visits = visits.validity_visits + visits.selector_visits;
		if (statistics) *statistics = visits;
		return result;
	}

	// Evaluates the whole function DAG in schedule order, including selectors
	// for invalid targets, so the work done does not depend on the target.
	std::optional<std::vector<bool>> evaluate_schedule(const std::vector<bool>& target,
		evaluation_statistics* statistics = nullptr) const
	{
		if (statistics) *statistics = {};
		if (target.size() != output_count_) throw std::invalid_argument("Wrong synthesized target width");
		std::vector<unsigned char> values(function_steps_.size() + 2);
		values[1] = 1;
		for (size_t index = 0; index < function_steps_.size(); ++index)
		{
			const auto current = function_steps_[index];
			values[index + 2] = values[target[current.variable] ? current.high : current.low];
		}
		if (statistics)
		{
			statistics->decision_visits = function_steps_.size();
			statistics->validity_visits = validity_nodes_;
			statistics->selector_visits = function_steps_.size() - validity_nodes_;
		}
		if (!values[function_valid_]) return std::nullopt;
		std::vector<bool> result(input_count_);
		for (size_t bit = 0; bit < input_count_; ++bit) result[bit] = values[function_results_[bit]] != 0;
		return result;
	}

	size_t enumerate(const std::vector<bool>& target,
		const std::function<bool(const std::vector<bool>&)>& callback, size_t maximum) const
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
		write_fields(stream, {input_count_, output_count_, node_count(), relation_, valid_});
		stream.put('\n');
		for (size_t id = 2; id < nodes_.size(); ++id)
		{
			write_fields(stream, {nodes_[id].variable, nodes_[id].low, nodes_[id].high});
			stream.put('\n');
		}
		write_fields(stream, functions_);
		stream << "\nEND_SYNTHESIZED_PLAN\n";
		if (!stream) throw std::runtime_error("Failed to write synthesized plan");
	}

	static synthesized_plan load(std::istream& stream)
	{
		text_reader<std::runtime_error> reader(stream, "synthesized plan");
		reader.expect("SYNTHESIZED_PLAN");
		if (reader.number(1) != 1) throw std::runtime_error("Unsupported synthesized plan version");
		synthesized_plan plan;
		plan.input_count_ = reader.number(max_dimension);
		plan.output_count_ = reader.number(max_dimension - plan.input_count_);
		const size_t count = reader.number(artifact_node_limit);
		plan.relation_ = reader.number(count + 1);
		plan.valid_ = reader.number(count + 1);
		const size_t dimensions = plan.input_count_ + plan.output_count_;
		if (count && !dimensions) throw std::runtime_error("Decision nodes without synthesis variables");
		std::unordered_set<decision, decision_hash> unique;
		plan.nodes_.reserve(count + 2);
		for (size_t index = 0; index < count; ++index)
		{
			const size_t id = index + 2;
			const size_t variable = reader.number(dimensions - 1);
			const size_t low = reader.number(id - 1);
			const size_t high = reader.number(id - 1);
			const decision current{variable, low, high};
			if (low == high || plan.nodes_[low].variable <= variable ||
				plan.nodes_[high].variable <= variable || !unique.insert(current).second)
				throw std::runtime_error("Invalid reduced ordered synthesized decision");
			plan.nodes_.push_back(current);
		}
		for (size_t bit = 0; bit < plan.input_count_; ++bit)
			plan.functions_.push_back(reader.number(count + 1));
		reader.expect("END_SYNTHESIZED_PLAN");
		auto roots = plan.functions_;
		roots.push_back(plan.valid_);
		roots.push_back(plan.relation_);
		if (reachable_count(plan.nodes_, roots) != count)
			throw std::runtime_error("Unreachable synthesized decision node");
		plan.index_functions();
		return plan;
	}

	// Emits only the canonical witness and validity DAG; the larger relation
	// that enumerates alternative witnesses stays out of the header.
	void export_cpp(std::ostream& stream, const cpp_signature& signature) const
	{
		signature.begin(stream, "Generated Boolean inverse; input/target bits retain their declared order.",
			{"array", "cstddef", "optional"});
		stream << "  struct decision { std::size_t variable, low, high; };\n"
			<< "  // Follow only the branches selected by the supplied target.\n"
			<< "  static constexpr std::array<decision, " << decimal(function_steps_.size()) << "> schedule{{\n";
		for (const auto current : function_steps_)
		{
			stream << "    {" << decimal(current.variable) << ", " << decimal(current.low)
				<< ", " << decimal(current.high) << "},\n";
		}
		stream << "  }};\n"
			<< "  const auto evaluate = [&target](std::size_t root) {\n"
			<< "    while (root >= 2) {\n"
			<< "      const auto step = schedule[root - 2];\n"
			<< "      root = target[step.variable] ? step.high : step.low;\n"
			<< "    }\n    return root != 0;\n  };\n"
			<< "  if (!evaluate(" << decimal(function_valid_) << ")) return std::nullopt;\n"
			<< "  static constexpr std::array<std::size_t, " << decimal(input_count_) << "> roots{{";
		for (size_t bit = 0; bit < input_count_; ++bit)
		{
			if (bit) stream << ", ";
			stream << decimal(function_results_[bit]);
		}
		stream << "}};\n  std::array<bool, " << decimal(input_count_) << "> inputs{};\n"
			<< "  for (std::size_t bit = 0; bit < roots.size(); ++bit) inputs[bit] = evaluate(roots[bit]);\n";
		signature.end(stream, [&](size_t column) { stream << "inputs[" << decimal(column) << ']'; });
	}
};

} // namespace detail
} // namespace dixelu::bitreverse::inversion

#endif // DIXELU_BITREVERSE_INVERSE_SYNTHESIZED_PLAN_H
