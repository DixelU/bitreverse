#ifndef DIXELU_BITREVERSE_INVERSE_NODE_INTERNER_H
#define DIXELU_BITREVERSE_INVERSE_NODE_INTERNER_H

#include "../tracker.h"

#include <cstddef>
#include <map>
#include <tuple>
#include <unordered_map>
#include <vector>

namespace dixelu::bitreverse::inversion::detail
{

// Hash-consing for nodes whose children are already interned: structurally
// equal gates share one node. Each node receives an identity in insertion
// order, and the interner keeps every node alive. Unknowns are never merged,
// although they share an opcode.
class node_interner
{
	using node = details::bitstate;
	using pointer = counted_ptr<node>;
	using signature = std::tuple<char, bool, size_t, size_t>;

	std::vector<pointer> nodes_;
	std::unordered_map<const node*, size_t> identities_;
	std::map<signature, size_t> gates_;

	signature key(const node& value) const
	{
		const auto child = [this](const pointer& operand)
		{
			return operand ? identity(operand) : static_cast<size_t>(-1);
		};
		return {static_cast<char>(value.operation), static_cast<bool>(value.state),
			child(value._1), child(value._2)};
	}

public:
	size_t size() const { return nodes_.size(); }
	size_t identity(const pointer& value) const { return identities_.at(value.get()); }

	// The interned node equal to value, or null when value would be new.
	pointer find(const pointer& value) const
	{
		if (identities_.contains(value.get()))
			return value;
		if (value->operation == '*')
			return {};
		const auto found = gates_.find(key(*value));
		return found == gates_.end() ? pointer{} : nodes_[found->second];
	}

	// Registers a node that find() reported as new.
	void insert(pointer value)
	{
		if (value->operation != '*')
			gates_.emplace(key(*value), nodes_.size());
		identities_.emplace(value.get(), nodes_.size());
		nodes_.push_back(std::move(value));
	}
};

} // namespace dixelu::bitreverse::inversion::detail

#endif // DIXELU_BITREVERSE_INVERSE_NODE_INTERNER_H
