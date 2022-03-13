//
// Lips, lisp shell.
// Copyright 2022 Krister Joas
//

#ifndef LISP_READER_HH
#define LISP_READER_HH

#include <iostream>
#include <optional>
#include <string>
#include "io.hh"

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
  Reader(const std::string& s) : _file(s), _curc(next()) {}
  Reader(file_t file) : _file(std::move(file)) {}
  /// @brief Read the next token from the input string.
  std::optional<token_t> read();
  void unread(token_t);

private:
  int next();

  std::optional<token_t> _token;
  bool _eof{false};
  file_t _file;
  int _curc;
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
