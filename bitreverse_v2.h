#ifndef DIXELU_BITREVERSE_V2_H
#define DIXELU_BITREVERSE_V2_H

#include <vector>
#include <array>
#include <cstdint>
#include <optional>
#include <map>
#include <set>
#include <memory>
#include <type_traits>
#include <stdexcept>
#include <algorithm>
#include <string>
#include <iostream>

#include "counted_ptr.h"

namespace dixelu
{

namespace bitreverse::v2
{

// ============================================================================
// Minimal CDCL SAT Solver Core
// ============================================================================

namespace sat
{

using literal_t = int32_t;
using clause_t = std::vector<literal_t>;
using assignment_t = std::vector<int8_t>; // -1: unassigned, 0: false, 1: true

inline literal_t make_literal(int32_t var, bool negated = false) { return (var << 1) | (negated ? 1 : 0); }
inline literal_t make_negated(int32_t var) { return make_literal(var, true); }
inline bool is_negated(literal_t lit) { return lit & 1; }
inline int32_t var_index(literal_t lit) { return lit >> 1; }
inline literal_t lit_negate(literal_t lit) { return lit ^ 1; }

class sat_solver
{
	std::vector<clause_t> _clauses;
	assignment_t _assignment;
	std::vector<literal_t> _trail;
	std::vector<size_t> _trail_lim;
	std::vector<int32_t> _decision_stack;
	int32_t _num_vars = 0;

	int8_t value(literal_t lit) const
	{
		int32_t idx = var_index(lit);
		if (idx >= static_cast<int32_t>(_assignment.size())) return -1;
		int8_t val = _assignment[idx];
		return is_negated(lit) ? (val == -1 ? -1 : !val) : val;
	}

	bool is_satisfied(const clause_t& clause) const
	{
		for (literal_t lit : clause)
			if (value(lit) == 1) return true;
		return false;
	}

	bool propagate()
	{
		size_t i = 0;
		while (i < _trail.size())
		{
			//literal_t lit = _trail[i++];
			for (const auto& clause : _clauses)
			{
				if (!is_satisfied(clause))
				{
					int unassigned_count = 0;
					literal_t last_unassigned = 0;
					for (literal_t l : clause)
					{
						int8_t val = value(l);
						if (val == -1)
						{
							++unassigned_count;
							last_unassigned = l;
						}
						else if (val == 1)
							goto next_clause;
					}
					if (unassigned_count == 0)
						return false; // Conflict
					if (unassigned_count == 1)
					{
						int32_t idx = var_index(last_unassigned);
						if (idx >= static_cast<int32_t>(_assignment.size()))
							_assignment.resize(idx + 1, -1);
						_assignment[idx] = is_negated(last_unassigned) ? 0 : 1;
						_trail.push_back(last_unassigned);
					}
				next_clause:;
				}
			}
		}
		return true;
	}

	bool search()
	{
		if (!propagate())
			return false;

		// Check if all variables are assigned
		int32_t decision_var = -1;
		for (int32_t i = 0; i < _num_vars; ++i)
		{
			if (i >= static_cast<int32_t>(_assignment.size()) || _assignment[i] == -1)
			{
				decision_var = i;
				break;
			}
		}

		if (decision_var == -1)
			return true; // All assigned, solution found

		// Save state
		auto saved_assignment = _assignment;
		auto saved_trail = _trail;
		auto saved_trail_lim = _trail_lim;
		auto saved_decisions = _decision_stack;

		_decision_stack.push_back(decision_var);
		_trail_lim.push_back(_trail.size());

		// Try true first, then false
		for (int8_t val : {1, 0})
		{
			_assignment = saved_assignment;
			_trail = saved_trail;
			_trail_lim = saved_trail_lim;
			_decision_stack = saved_decisions;

			int32_t idx = decision_var;
			if (idx >= static_cast<int32_t>(_assignment.size()))
				_assignment.resize(idx + 1, -1);

			_assignment[idx] = val;
			_trail.push_back(make_literal(decision_var, val == 0));

			if (search())
				return true;
		}

		return false;
	}

public:
	int32_t new_variable() { return _num_vars++; }

	void add_clause(const clause_t& clause)
	{
		_clauses.push_back(clause);
	}

	void add_binary_clause(literal_t a, literal_t b)
	{
		_clauses.push_back({a, b});
	}

	void add_ternary_clause(literal_t a, literal_t b, literal_t c)
	{
		_clauses.push_back({a, b, c});
	}

	void add_unit_clause(literal_t lit)
	{
		_clauses.push_back({lit});
	}

	// Encode: out = a AND b
	void encode_and(literal_t a, literal_t b, literal_t out)
	{
		// out => (a AND b)  ==  (!out | a), (!out | b)
		add_binary_clause(lit_negate(out), a);
		add_binary_clause(lit_negate(out), b);
		// (a AND b) => out  ==  (!a | !b | out)
		add_ternary_clause(lit_negate(a), lit_negate(b), out);
	}

	// Encode: out = a OR b
	void encode_or(literal_t a, literal_t b, literal_t out)
	{
		// out => (a OR b)  ==  (!out | a | b)
		add_ternary_clause(lit_negate(out), a, b);
		// (a OR b) => out  ==  (!a | out), (!b | out)
		add_binary_clause(lit_negate(a), out);
		add_binary_clause(lit_negate(b), out);
	}

	// Encode: out = a XOR b
	void encode_xor(literal_t a, literal_t b, literal_t out)
	{
		// XOR truth table encoding:
		// (!a | !b | !out), (a | b | !out), (!a | b | out), (a | !b | out)
		add_ternary_clause(lit_negate(a), lit_negate(b), lit_negate(out));
		add_ternary_clause(a, b, lit_negate(out));
		add_ternary_clause(lit_negate(a), b, out);
		add_ternary_clause(a, lit_negate(b), out);
	}

	// Encode: out = NOT a
	void encode_not(literal_t a, literal_t out)
	{
		// out = !a  ==  (!a | !out), (a | out)
		add_binary_clause(lit_negate(a), lit_negate(out));
		add_binary_clause(a, out);
	}

	// Encode: out = a XNOR b (equality)
	void encode_xnor(literal_t a, literal_t b, literal_t out)
	{
		// out = (a == b)
		// (!a | b | !out), (a | !b | !out), (!a | !b | out), (a | b | out)
		add_ternary_clause(lit_negate(a), b, lit_negate(out));
		add_ternary_clause(a, lit_negate(b), lit_negate(out));
		add_ternary_clause(lit_negate(a), lit_negate(b), out);
		add_ternary_clause(a, b, out);
	}

	bool solve()
	{
		_assignment.assign(_num_vars, -1);
		_trail.clear();
		_trail_lim.clear();
		_decision_stack.clear();
		return search();
	}

	int8_t get_assignment(int32_t var) const
	{
		if (var >= static_cast<int32_t>(_assignment.size()))
			return -1;
		return _assignment[var];
	}

	const assignment_t& get_full_assignment() const { return _assignment; }
};

} // namespace sat

// ============================================================================
// Symbolic Expression Node (simplified, no depth tracking by default)
// ============================================================================

namespace expr
{

struct node;
using node_ptr = counted_ptr<node>;

struct node
{
	enum class op_t : uint8_t
	{
		CONST_FALSE,
		CONST_TRUE,
		UNKNOWN,
		NOT,
		AND,
		OR,
		XOR
	};

	op_t operation;
	node_ptr child1;
	node_ptr child2;
	int32_t sat_var = -1; // SAT variable index

	node(op_t op, node_ptr c1 = {}, node_ptr c2 = {})
		: operation(op), child1(c1), child2(c2) {}

	static node_ptr make_const(bool value)
	{
		static auto false_node = make_counted<node>(op_t::CONST_FALSE);
		static auto true_node = make_counted<node>(op_t::CONST_TRUE);
		return value ? true_node : false_node;
	}

	static node_ptr make_unknown()
	{
		return make_counted<node>(op_t::UNKNOWN);
	}

	static node_ptr make_not(node_ptr child)
	{
		if (child->operation == op_t::CONST_FALSE)
			return make_const(true);
		if (child->operation == op_t::CONST_TRUE)
			return make_const(false);
		if (child->operation == op_t::NOT)
			return child->child1; // Double negation
		return make_counted<node>(op_t::NOT, child);
	}

	static node_ptr make_and(node_ptr left, node_ptr right)
	{
		if (left->operation == op_t::CONST_FALSE || right->operation == op_t::CONST_FALSE)
			return make_const(false);
		if (left->operation == op_t::CONST_TRUE)
			return right;
		if (right->operation == op_t::CONST_TRUE)
			return left;
		if (left == right)
			return left;
		// Canonical ordering for better sharing
		if (left.get() > right.get())
			std::swap(left, right);
		return make_counted<node>(op_t::AND, left, right);
	}

	static node_ptr make_or(node_ptr left, node_ptr right)
	{
		if (left->operation == op_t::CONST_TRUE || right->operation == op_t::CONST_TRUE)
			return make_const(true);
		if (left->operation == op_t::CONST_FALSE)
			return right;
		if (right->operation == op_t::CONST_FALSE)
			return left;
		if (left == right)
			return left;
		if (left.get() > right.get())
			std::swap(left, right);
		return make_counted<node>(op_t::OR, left, right);
	}

	static node_ptr make_xor(node_ptr left, node_ptr right)
	{
		if (left->operation == op_t::CONST_FALSE)
			return right;
		if (right->operation == op_t::CONST_FALSE)
			return left;
		if (left->operation == op_t::CONST_TRUE)
			return make_not(right);
		if (right->operation == op_t::CONST_TRUE)
			return make_not(left);
		if (left == right)
			return make_const(false);
		if (left->operation == op_t::NOT && left->child1 == right)
			return make_const(true);
		if (right->operation == op_t::NOT && right->child1 == left)
			return make_const(true);
		if (left.get() > right.get())
			std::swap(left, right);
		return make_counted<node>(op_t::XOR, left, right);
	}
};

// ============================================================================
// CNF Encoder (Tseitin Transformation)
// ============================================================================

class cnf_encoder
{
	sat::sat_solver& _solver;
	std::map<node_ptr, sat::literal_t> _node_to_lit;
	std::set<node_ptr> _processed;
	std::set<node_ptr> _unknowns;

public:
	cnf_encoder(sat::sat_solver& solver) : _solver(solver) {}

	sat::literal_t encode(node_ptr n)
	{
		if (_processed.contains(n))
			return _node_to_lit[n];

		sat::literal_t lit = sat::make_literal(_solver.new_variable());
		n->sat_var = sat::var_index(lit);
		_node_to_lit[n] = lit;
		_processed.insert(n);

		switch (n->operation)
		{
			case node::op_t::CONST_FALSE:
				_solver.add_unit_clause(sat::lit_negate(lit)); // lit must be false
				break;
			case node::op_t::CONST_TRUE:
				_solver.add_unit_clause(lit); // lit must be true
				break;
			case node::op_t::UNKNOWN:
				_unknowns.insert(n);
				// No constraints - unknown is a free variable
				break;
			case node::op_t::NOT:
			{
				sat::literal_t child_lit = encode(n->child1);
				_solver.encode_not(child_lit, lit);
				break;
			}
			case node::op_t::AND:
			{
				sat::literal_t left_lit = encode(n->child1);
				sat::literal_t right_lit = encode(n->child2);
				_solver.encode_and(left_lit, right_lit, lit);
				break;
			}
			case node::op_t::OR:
			{
				sat::literal_t left_lit = encode(n->child1);
				sat::literal_t right_lit = encode(n->child2);
				_solver.encode_or(left_lit, right_lit, lit);
				break;
			}
			case node::op_t::XOR:
			{
				sat::literal_t left_lit = encode(n->child1);
				sat::literal_t right_lit = encode(n->child2);
				_solver.encode_xor(left_lit, right_lit, lit);
				break;
			}
		}

		return lit;
	}

	const std::set<node_ptr>& get_unknowns() const { return _unknowns; }
};

// ============================================================================
// Solution Representation
// ============================================================================

struct solution
{
	std::map<node_ptr, bool> assignments;

	bool operator<(const solution& other) const
	{
		return assignments < other.assignments;
	}
};

using solutions_t = std::set<solution>;

} // namespace expr

// ============================================================================
// Bit Tracker (SAT-based)
// ============================================================================

struct bit_tracker;
using bit_tracker_ptr = counted_ptr<expr::node>;

struct bit_tracker
{
	bit_tracker_ptr _node;

	bit_tracker() : _node(expr::node::make_const(false)) {}

	explicit bit_tracker(bit_tracker_ptr node) : _node(std::move(node)) {}

	bit_tracker(bool value) : _node(expr::node::make_const(value)) {}

	bit_tracker(const bit_tracker&) = default;
	bit_tracker(bit_tracker&&) = default;

	static bit_tracker unknown()
	{
		return bit_tracker(expr::node::make_unknown());
	}

	bit_tracker& operator=(const bit_tracker&) = default;
	bit_tracker& operator=(bit_tracker&&) = default;

	bit_tracker operator|(const bit_tracker& rhs) const
	{
		return bit_tracker(expr::node::make_or(_node, rhs._node));
	}

	bit_tracker operator&(const bit_tracker& rhs) const
	{
		return bit_tracker(expr::node::make_and(_node, rhs._node));
	}

	bit_tracker operator^(const bit_tracker& rhs) const
	{
		return bit_tracker(expr::node::make_xor(_node, rhs._node));
	}

	bit_tracker operator~() const
	{
		return bit_tracker(expr::node::make_not(_node));
	}

	bit_tracker operator!() const
	{
		return ~(*this);
	}

	bit_tracker& operator|=(const bit_tracker& rhs)
	{
		_node = expr::node::make_or(_node, rhs._node);
		return *this;
	}

	bit_tracker& operator&=(const bit_tracker& rhs)
	{
		_node = expr::node::make_and(_node, rhs._node);
		return *this;
	}

	bit_tracker& operator^=(const bit_tracker& rhs)
	{
		_node = expr::node::make_xor(_node, rhs._node);
		return *this;
	}

	bit_tracker& operator=(bool value)
	{
		_node = expr::node::make_const(value);
		return *this;
	}

	bit_tracker& operator=(std::nullptr_t)
	{
		_node = expr::node::make_unknown();
		return *this;
	}
};

// ============================================================================
// Integer Tracker (SAT-based)
// ============================================================================

template <size_t N>
struct int_tracker
{
	std::array<bit_tracker, N> bits;

	int_tracker()
	{
		for (auto& bit : bits)
			bit = bit_tracker(false);
	}

	int_tracker(std::nullptr_t)
	{
		for (auto& bit : bits)
			bit = bit_tracker::unknown();
	}

	int_tracker(uintmax_t value)
	{
		for (size_t i = 0; i < N; ++i)
			bits[i] = bit_tracker((value >> (N - 1 - i)) & 1);
	}

	template <size_t M>
	int_tracker(const int_tracker<M>& other)
	{
		//size_t start = (N > M) ? (N - M) : 0;
		size_t other_start = (M > N) ? (M - N) : 0;
		for (size_t i = 0; i < N && (other_start + i) < M; ++i)
			bits[i] = other.bits[other_start + i];
	}

	int_tracker(const std::array<bit_tracker, N>& other) : bits(other) {}

	explicit int_tracker(bit_tracker bit)
	{
		for (size_t i = 0; i < N; ++i)
			bits[i] = (i == N - 1) ? bit : bit_tracker(false);
	}

	int_tracker(const int_tracker&) = default;
	int_tracker(int_tracker&&) = default;
	int_tracker& operator=(const int_tracker&) = default;
	int_tracker& operator=(int_tracker&&) = default;

	int_tracker& operator|=(const int_tracker& rhs)
	{
		for (size_t i = 0; i < N; ++i)
			bits[i] = bits[i] | rhs.bits[i];
		return *this;
	}

	int_tracker& operator&=(const int_tracker& rhs)
	{
		for (size_t i = 0; i < N; ++i)
			bits[i] = bits[i] & rhs.bits[i];
		return *this;
	}

	int_tracker& operator^=(const int_tracker& rhs)
	{
		for (size_t i = 0; i < N; ++i)
			bits[i] = bits[i] ^ rhs.bits[i];
		return *this;
	}

	int_tracker operator|(const int_tracker& rhs) const
	{
		int_tracker result = *this;
		result |= rhs;
		return result;
	}

	int_tracker operator&(const int_tracker& rhs) const
	{
		int_tracker result = *this;
		result &= rhs;
		return result;
	}

	int_tracker operator^(const int_tracker& rhs) const
	{
		int_tracker result = *this;
		result ^= rhs;
		return result;
	}

	int_tracker operator~() const
	{
		int_tracker result;
		for (size_t i = 0; i < N; ++i)
			result.bits[i] = ~bits[i];
		return result;
	}

	int_tracker operator<<(size_t shift) const
	{
		if (shift >= N)
			return int_tracker();
		int_tracker result;
		for (size_t i = shift; i < N; ++i)
			result.bits[i] = bits[i - shift];
		return result;
	}

	int_tracker operator>>(size_t shift) const
	{
		if (shift >= N)
			return int_tracker();
		int_tracker result;
		for (size_t i = 0; i < N - shift; ++i)
			result.bits[i] = bits[i + shift];
		return result;
	}

	int_tracker& operator<<=(size_t shift)
	{
		*this = (*this << shift);
		return *this;
	}

	int_tracker& operator>>=(size_t shift)
	{
		*this = (*this >> shift);
		return *this;
	}

	int_tracker operator<<(const int_tracker& shift) const
	{
		int_tracker result = *this;
		result <<= shift;
		return result;
	}

	int_tracker operator>>(const int_tracker& shift) const
	{
		int_tracker result = *this;
		result >>= shift;
		return result;
	}

	int_tracker& operator<<=(const int_tracker& shift)
	{
		int_tracker result = *this;
		for (size_t i = 0; i < N; ++i)
		{
			size_t shift_amount = 1 << i;
			if (shift_amount >= N)
				break;
			int_tracker shifted = (*this) << shift_amount;
			for (size_t j = 0; j < N; ++j)
			{
				// result.bits[j] = (shift.bits[i] & shifted.bits[j]) | (!shift.bits[i] & result.bits[j])
				auto cond = shift.bits[N - 1 - i];
				result.bits[j] = (cond & shifted.bits[j]) | (~cond & result.bits[j]);
			}
		}
		*this = std::move(result);
		return *this;
	}

	int_tracker& operator>>=(const int_tracker& shift)
	{
		int_tracker result = *this;
		for (size_t i = 0; i < N; ++i)
		{
			size_t shift_amount = 1 << i;
			if (shift_amount >= N)
				break;
			int_tracker shifted = (*this) >> shift_amount;
			for (size_t j = 0; j < N; ++j)
			{
				auto cond = shift.bits[N - 1 - i];
				result.bits[j] = (cond & shifted.bits[j]) | (~cond & result.bits[j]);
			}
		}
		*this = std::move(result);
		return *this;
	}

	int_tracker& operator+=(const int_tracker& rhs)
	{
		bit_tracker carry = bit_tracker(false);
		for (size_t i = 0; i < N; ++i)
		{
			size_t idx = N - 1 - i;
			auto& a = bits[idx];
			auto& b = rhs.bits[idx];

			auto sum = a ^ b ^ carry;
			carry = (a & b) | (a & carry) | (b & carry);
			bits[idx] = sum;
		}
		return *this;
	}

	int_tracker operator+(const int_tracker& rhs) const
	{
		int_tracker result = *this;
		result += rhs;
		return result;
	}

	int_tracker operator-() const
	{
		return (~(*this)) + int_tracker(1);
	}

	int_tracker& operator-=(const int_tracker& rhs)
	{
		*this = (*this) + (-rhs);
		return *this;
	}

	int_tracker operator-(const int_tracker& rhs) const
	{
		int_tracker result = *this;
		result -= rhs;
		return result;
	}

	explicit operator bit_tracker()
	{
		bit_tracker result;
		for (size_t i = 0; i < N; ++i)
			result |= bits[i];
		return result;
	}

	bit_tracker operator!() const
	{
		return !static_cast<bit_tracker>(*this);
	}

	std::string to_string() const
	{
		std::string result;
		result.reserve(N);
		for (const auto& bit : bits)
		{
			switch (bit._node->operation)
			{
			case expr::node::op_t::CONST_FALSE:
				result.push_back('0');
				break;
			case expr::node::op_t::CONST_TRUE:
				result.push_back('1');
				break;
			case expr::node::op_t::UNKNOWN:
				result.push_back('*');
				break;
			default:
				result.push_back('?');
				break;
			}
		}
		return result;
	}
};

using itu8 = int_tracker<8>;
using itu16 = int_tracker<16>;
using itu32 = int_tracker<32>;
using itu64 = int_tracker<64>;

// ============================================================================
// Constraint Solver
// ============================================================================

namespace solver
{

/**
 * Find all satisfying assignments for the constraint (lhs == rhs)
 * by encoding the expression tree to CNF and using SAT solver.
 */
inline expr::solutions_t assert_equality(const bit_tracker& lhs, const bit_tracker& rhs)
{
	// Create XOR of lhs and rhs - we want this to be false (they should be equal)
	auto diff = lhs ^ rhs;

	sat::sat_solver solver;
	expr::cnf_encoder encoder(solver);

	// Encode the expression tree to CNF
	sat::literal_t diff_lit = encoder.encode(diff._node);

	// Assert that diff must be false (lhs == rhs)
	solver.add_unit_clause(sat::lit_negate(diff_lit));

	expr::solutions_t solutions;

	// Find all solutions
	while (solver.solve())
	{
		expr::solution sol;
		const auto& assignment = solver.get_full_assignment();

		// Extract assignments for unknown variables only
		for (const auto& unknown : encoder.get_unknowns())
		{
			if (unknown->sat_var >= 0 &&
				unknown->sat_var < static_cast<int32_t>(assignment.size()))
			{
				sol.assignments[unknown] = (assignment[unknown->sat_var] == 1);
			}
		}

		solutions.insert(sol);

		// Add blocking clause to find next solution
		sat::clause_t blocking_clause;
		for (const auto& [node, value] : sol.assignments)
		{
			if (node->sat_var >= 0)
			{
				sat::literal_t lit = sat::make_literal(node->sat_var);
				blocking_clause.push_back(value ? sat::lit_negate(lit) : lit);
			}
		}

		if (!blocking_clause.empty())
			solver.add_clause(blocking_clause);
		else
			break; // No unknowns, only one solution possible
	}

	return solutions;
}

template <size_t N>
expr::solutions_t assert_equality(const int_tracker<N>& lhs, const int_tracker<N>& rhs)
{
	bit_tracker result(false);
	for (size_t i = 0; i < N; ++i)
		result = result | (lhs.bits[i] ^ rhs.bits[i]);

	return assert_equality(result, bit_tracker(false));
}

/**
 * Apply a solution's assignments to a bit_tracker
 */
inline void apply_solution(bit_tracker& bt, const expr::solution& sol)
{
	auto it = sol.assignments.find(bt._node);
	if (it != sol.assignments.end())
		bt = bit_tracker(it->second);
}

template <size_t N>
void apply_solution(int_tracker<N>& it, const expr::solution& sol)
{
	for (auto& bit : it.bits)
		apply_solution(bit, sol);
}

// v1-compatible naming for convenience
inline void assign_assert_result(bit_tracker& value, const expr::solution& sol)
{
	apply_solution(value, sol);
}

template <size_t N>
void assign_assert_result(int_tracker<N>& value, const expr::solution& sol)
{
	apply_solution(value, sol);
}

} // namespace solver

} // namespace bitreverse::v2

} // namespace dixelu

#endif // DIXELU_BITREVERSE_V2_H