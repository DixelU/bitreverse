#ifndef DIXELU_BITREVERSE_H
#define DIXELU_BITREVERSE_H

#include <cstddef>
#include <functional>
#include <map>
#include <stdexcept>
#include <type_traits>
#include <utility>

#include "tracker.h"
#include "solver/options.h"
#include "solver/solve.h"
#include "solver/statistics.h"

namespace dixelu::bitreverse
{

class unsatisfiable : public std::runtime_error
{
public:
	unsatisfiable() : std::runtime_error("Unsatisfiable constraints") {}
};

template<typename Callback>
concept solution_handler = std::is_invocable_v<
	std::decay_t<Callback>&,
	const collision_resolution::solution&>;

namespace details
{

// A void callback continues enumeration; any other result is converted to
// bool, where false requests an orderly early stop.
template<solution_handler Callback>
collision_resolution::solution_callback make_solution_callback(Callback&& callback)
{
	using callback_t = std::decay_t<Callback>;
	using result_t = std::invoke_result_t<callback_t&, const collision_resolution::solution&>;

	return [handler = callback_t(std::forward<Callback>(callback))]
		(const collision_resolution::solution& found) mutable -> bool
		{
			if constexpr (std::is_void_v<result_t>)
			{
				std::invoke(handler, found);
				return true;
			}
			else
				return static_cast<bool>(std::invoke(handler, found));
		};
}

} // namespace details

// Solves lhs == rhs over the unknown leaves and returns every complete
// assignment (or the first one). Throws unsatisfiable when there is none and
// solver_limit when options.max_search_steps is exhausted.
inline collision_resolution::solutions_t assert_equality(
	const bit_tracker& lhs,
	const bit_tracker& rhs,
	const solver_options& options,
	bool first_only = false,
	solver_statistics* statistics = nullptr)
{
	auto solutions = collision_resolution::solve(lhs ^ rhs, false, first_only, options, statistics);
	if (solutions.empty())
		throw unsatisfiable();
	return solutions;
}

inline collision_resolution::solutions_t assert_equality(
	const bit_tracker& lhs,
	const bit_tracker& rhs,
	bool first_only = false)
{
	return assert_equality(lhs, rhs, solver_options{}, first_only);
}

// Streams each solution to `callback` without retaining it and returns how
// many were found.
template<solution_handler Callback>
size_t assert_equality(
	const bit_tracker& lhs,
	const bit_tracker& rhs,
	const solver_options& options,
	Callback&& callback,
	solver_statistics* statistics = nullptr)
{
	const size_t count = collision_resolution::solve_stream(
		lhs ^ rhs,
		false,
		details::make_solution_callback(std::forward<Callback>(callback)),
		options,
		statistics);
	if (count == 0)
		throw unsatisfiable();
	return count;
}

template<solution_handler Callback>
size_t assert_equality(
	const bit_tracker& lhs,
	const bit_tracker& rhs,
	Callback&& callback)
{
	return assert_equality(lhs, rhs, solver_options{}, std::forward<Callback>(callback));
}

template<size_t N>
collision_resolution::solutions_t assert_equality(
	const int_tracker<N>& lhs,
	const int_tracker<N>& rhs,
	const solver_options& options,
	bool first_only = false,
	solver_statistics* statistics = nullptr)
{
	return assert_equality(differs(lhs, rhs), false, options, first_only, statistics);
}

template<size_t N>
collision_resolution::solutions_t assert_equality(
	const int_tracker<N>& lhs,
	const int_tracker<N>& rhs,
	bool first_only = false)
{
	return assert_equality(lhs, rhs, solver_options{}, first_only);
}

template<size_t N, solution_handler Callback>
size_t assert_equality(
	const int_tracker<N>& lhs,
	const int_tracker<N>& rhs,
	const solver_options& options,
	Callback&& callback,
	solver_statistics* statistics = nullptr)
{
	return assert_equality(
		differs(lhs, rhs), false, options, std::forward<Callback>(callback), statistics);
}

template<size_t N, solution_handler Callback>
size_t assert_equality(
	const int_tracker<N>& lhs,
	const int_tracker<N>& rhs,
	Callback&& callback)
{
	return assert_equality(lhs, rhs, solver_options{}, std::forward<Callback>(callback));
}

// Replaces each unknown bit of `value` that the solution assigns by its
// assigned constant. Other bits are left unchanged.
inline void assign_assert_result(
	bit_tracker& value,
	const std::map<const counted_ptr<details::bitstate>, bool>& assignments)
{
	const auto iter = assignments.find(value.bit_state);
	if (iter != assignments.end())
		value = iter->second;
}

template <size_t N>
void assign_assert_result(
	int_tracker<N>& value,
	const std::map<const counted_ptr<details::bitstate>, bool>& assignments)
{
	for (auto& bit : value.bits)
		assign_assert_result(bit, assignments);
}

} // namespace dixelu::bitreverse

#endif // DIXELU_BITREVERSE_H
