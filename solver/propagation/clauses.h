#ifndef DIXELU_BITREVERSE_SOLVER_PROPAGATION_CLAUSES_H
#define DIXELU_BITREVERSE_SOLVER_PROPAGATION_CLAUSES_H

// Internal header: included by bitreverse.h inside
// dixelu::bitreverse::collision_resolution::cdcl.

using literal_t = std::uint32_t;
using clause_id = std::uint32_t;
inline constexpr clause_id no_clause =
	std::numeric_limits<clause_id>::max();

constexpr literal_t make_literal(node_id variable, bool negated = false)
{
	return
		(static_cast<literal_t>(variable) << 1) |
		static_cast<literal_t>(negated);
}

constexpr node_id literal_variable(literal_t literal)
{
	return static_cast<node_id>(literal >> 1);
}

constexpr bool literal_is_negated(literal_t literal)
{
	return (literal & 1U) != 0;
}

constexpr literal_t negate_literal(literal_t literal)
{
	return literal ^ 1U;
}

constexpr literal_t literal_for_value(node_id variable, bool value)
{
	return make_literal(variable, !value);
}

struct clause
{
	std::vector<literal_t> literals;
	std::array<size_t, 2> watched{0, 0};
	bool learned{false};
};

struct clause_database
{
	std::vector<clause> clauses;
	std::vector<std::vector<clause_id>> watches;
	std::vector<clause_id> unit_clauses;
	size_t watched_clause_count{0};
	bool contains_empty_clause{false};

	explicit clause_database(size_t variable_count) :
		watches(variable_count * 2) {}

	clause_id add_clause(
		std::vector<literal_t> literals,
		bool learned = false,
		bool watch_literals = true)
	{
		if (literals.empty())
		{
			contains_empty_clause = true;
			return no_clause;
		}

		const clause_id id =
			static_cast<clause_id>(clauses.size());
		clause stored{
			.literals = std::move(literals),
			.watched = {0, 0},
			.learned = learned};

		if (stored.literals.size() == 1)
			unit_clauses.push_back(id);
		else if (watch_literals)
		{
			stored.watched[1] = 1;
			watches[stored.literals[0]].push_back(id);
			watches[stored.literals[1]].push_back(id);
			++watched_clause_count;
		}

		clauses.push_back(std::move(stored));
		return id;
	}
};

#endif // DIXELU_BITREVERSE_SOLVER_PROPAGATION_CLAUSES_H
