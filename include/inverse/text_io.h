#ifndef DIXELU_BITREVERSE_INVERSE_TEXT_IO_H
#define DIXELU_BITREVERSE_INVERSE_TEXT_IO_H

#include <charconv>
#include <cstddef>
#include <cstdint>
#include <initializer_list>
#include <istream>
#include <ostream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

// Text formats shared by every plan: saved artifact sections and exported C++.
namespace dixelu::bitreverse::inversion::detail
{

// Safety cap on the node, port and decision counts a saved section may declare.
inline constexpr size_t artifact_node_limit = 1'000'000;

// Writes a decimal number with to_chars, so neither artifacts nor generated
// C++ depend on the destination stream's base, locale or other numeric flags.
struct decimal
{
	std::uint64_t value;

	explicit decimal(std::uint64_t number) : value(number) {}

	friend std::ostream& operator<<(std::ostream& stream, decimal number)
	{
		char buffer[20];
		const auto end = std::to_chars(buffer, buffer + sizeof(buffer), number.value).ptr;
		return stream.write(buffer, end - buffer);
	}
};

// Writes each value followed by a space.
template<class Range>
void write_fields(std::ostream& stream, const Range& values)
{
	for (const auto value : values)
		stream << decimal(value) << ' ';
}

inline void write_fields(std::ostream& stream, std::initializer_list<std::uint64_t> values)
{
	write_fields<std::initializer_list<std::uint64_t>>(stream, values);
}

// Reads one artifact section: exact markers, tokens and bounded decimal
// numbers, each ending at whitespace or the end of input. Nothing after a
// field is consumed, so a section ends exactly at its closing marker.
// Every malformed field throws Error.
template<class Error>
class text_reader
{
	std::istream& stream_;
	std::string section_;

	static bool space(int c)
	{
		return c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f' || c == '\v';
	}

	bool at_field_end()
	{
		const int next = stream_.peek();
		return next == std::char_traits<char>::eof() || space(next);
	}

	void skip_space()
	{
		while (space(stream_.peek()))
			stream_.get();
	}

	[[noreturn]] void fail(std::string_view field) const
	{
		throw Error("Invalid " + section_ + ' ' + std::string(field));
	}

public:
	text_reader(std::istream& stream, std::string_view section) : stream_(stream), section_(section) {}

	void expect(std::string_view marker)
	{
		skip_space();
		for (const char c : marker)
			if (stream_.get() != c)
				fail("marker");
		if (!at_field_end())
			fail("marker");
	}

	// The next whitespace-delimited token; empty at the end of input.
	std::string token()
	{
		skip_space();
		std::string result;
		while (!at_field_end())
			result.push_back(static_cast<char>(stream_.get()));
		return result;
	}

	std::uint64_t number(std::uint64_t limit)
	{
		skip_space();
		std::uint64_t value = 0;
		size_t digits = 0;
		while (stream_.peek() >= '0' && stream_.peek() <= '9')
		{
			const auto digit = static_cast<std::uint64_t>(stream_.get() - '0');
			if (++digits > 20 || digit > limit || value > (limit - digit) / 10)
				throw Error(section_ + " number exceeds its limit");
			value = value * 10 + digit;
		}
		if (!digits || !at_field_end())
			fail("number");
		return value;
	}
};

// The exported entry point
//   std::optional<std::array<bool, inputs>> bitreverse_inverse(const std::array<bool, targets>&)
// returns every declared input: a baked constant where columns[i] is absent,
// otherwise unknown columns[i] of the inverse.
struct cpp_signature
{
	static constexpr size_t absent = static_cast<size_t>(-1);

	const std::vector<size_t>& columns;
	const std::vector<bool>& constants;
	size_t unknowns, targets;

	// Starts a standalone header and opens the function body.
	void begin(std::ostream& stream, std::string_view comment,
		std::initializer_list<std::string_view> headers) const
	{
		if (columns.size() != constants.size())
			throw std::invalid_argument("Invalid exported input layout");
		for (const size_t column : columns)
			if (column != absent && column >= unknowns)
				throw std::invalid_argument("Invalid exported input column");
		stream << "// " << comment << "\n#pragma once\n";
		for (const auto header : headers)
			stream << "#include <" << header << ">\n";
		stream << "\ninline std::optional<std::array<bool, " << decimal(columns.size()) << ">>\n"
			<< "bitreverse_inverse(const std::array<bool, " << decimal(targets) << ">& target)\n{\n";
	}

	// Returns the declared inputs and closes the function; unknown(column)
	// writes the expression of one unknown column.
	template<class Unknown>
	void end(std::ostream& stream, Unknown unknown) const
	{
		stream << "  return std::array<bool, " << decimal(columns.size()) << ">{{";
		for (size_t index = 0; index < columns.size(); ++index)
		{
			if (index)
				stream << ", ";
			if (columns[index] == absent)
				stream << (constants[index] ? "true" : "false");
			else
				unknown(columns[index]);
		}
		stream << "}};\n}\n";
	}
};

} // namespace dixelu::bitreverse::inversion::detail

#endif // DIXELU_BITREVERSE_INVERSE_TEXT_IO_H
