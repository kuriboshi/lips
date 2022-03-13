//
// Lips, lisp shell.
// Copyright 2022 Krister Joas
//

#ifndef LISP_READER_HH
#define LISP_READER_HH

#include <iostream>
#include <optional>
#include <string>

namespace lisp
{

struct token_t
{
  enum class type
  {
    MACRO,
    STRING,
    SYMBOL,
    INT
  } type;
  token_t() : type(type::SYMBOL) {}
  token_t(enum type t, const std::string& s) : type(t), token(s) {}
  std::string token;
  bool is_macro(char c) const
  {
    return type == type::MACRO && !token.empty() && token[0] == c;
  }
};

/// @brief A reader/lexer of a string input.
class Reader
{
public:
  Reader(const std::string& line) : _line(line), _pos(_line.begin()) {}
  /// @brief Read the next token from the input string.
  std::optional<token_t> read();
  void unread(token_t);

private:
  std::optional<token_t> _token;
  std::string _line;
  std::string::iterator _pos;
};

inline bool operator==(const token_t& l, const token_t& r)
{
  return l.type == r.type && l.token == r.token;
}

inline std::ostream& operator<<(std::ostream& os, const token_t& t)
{
  switch(t.type)
  {
    case token_t::type::MACRO:
      os << "MACRO:";
      break;
    case token_t::type::STRING:
      os << "STRING:";
      break;
    case token_t::type::SYMBOL:
      os << "SYMBOL:";
      break;
    case token_t::type::INT:
      os << "INT:";
      break;
    default:
      os << "?:";
      break;
  }
  os << t.token;
  return os;
}

}

#endif
