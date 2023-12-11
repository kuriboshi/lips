//
// Lips, lisp shell.
// Copyright 2022-2023 Krister Joas
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//

#ifndef LISP_LEXER_HH
#define LISP_LEXER_HH

#include <cstdint>
#include <iostream>
#include <string>

#include "file.hh"
#include "types.hh"
#include "syntax.hh"
#include "vm.hh"

namespace lisp
{

///
/// @brief Represents one token in a stream of tokens.
///
/// Each token can be of one of the following types.
///
/// @li @c EMPTY An empty token signalling the end of a stream or an error
///   tokenizing the stream.
/// @li @c SPECIAL A token for special characters. This includes the left and
///   right parenthesis, the left and right super parenthesis, a single quote,
///   and a dot used to create dotted pairs.
/// @li @c STRING A string. Double quotes can be embedded by using the
///   backslash, common in other Unix tools. Note that this differs from
///   Interlisp which uses a percent sign for this purpose.
/// @li @c SYMBOL A literal symbol.
/// @li @c INT An integer.
/// @li @c FLOAT A floating point value.
/// @li @c MACRO A simple read-macro.
/// @li @c SPLICE A read-macro which splices the result into the current
///   expression.
/// @li @c INFIX A read-macro which handles infix notation.
///
struct token_t final
{
  /// @brief The type of token.
  enum class type
  {
    EMPTY,   // No type
    SPECIAL, // Special characters like '(', ')', '[', etc.
    STRING,  // Literal string
    SYMBOL,  // Atom or symbol
    INT,     // Integer
    FLOAT,   // Floating point number
    MACRO,   // Read-macro
    SPLICE,  // Splice read-macro
    INFIX    // Infix read-macro
  };

  /// @brief Token type.
  enum type type
  {
    type::EMPTY
  };
  /// @brief A string representation of the token.
  std::string token;

  /// @brief Default constructor is the empty token.
  token_t() = default;
  /// @brief Construct a token of a certain type but with an empty token
  /// string.
  ///
  /// @param t The type of token.
  token_t(enum type t)
    : type(t)
  {}
  /// @brief Construct a token of a certain type with the token string
  /// representation given.
  ///
  /// @param t The type of token.
  /// @param s The token string.
  token_t(enum type t, std::string s)
    : type(t),
      token(std::move(s))
  {}
  ~token_t() = default;
  /// @brief The operator bool for use in bool contexts.
  ///
  /// @returns True if the token is not the empty token (type EMPTY).
  explicit operator bool() const { return type != type::EMPTY; }

  /// @brief Default copy constructor.
  token_t(const token_t& t) = default;
  /// @brief Copy and move assignment operator.
  token_t& operator=(const token_t& t) noexcept
  {
    if(this != &t)
    {
      type = t.type;
      token = t.token;
    }
    return *this;
  }
  /// @brief The move constructor.
  ///
  /// The moved from token becomes an empty token.
  token_t(token_t&& t) noexcept
    : type(t.type),
      token(std::move(t.token))
  {
    t.type = type::EMPTY;
  }
  /// @brief The move assignment operator
  token_t& operator=(token_t&& t) noexcept
  {
    if(this != &t)
    {
      type = t.type;
      token = std::move(t.token);
      t.type = type::EMPTY;
    }
    return *this;
  }

  /// @brief Checks that the token is of type SPECIAL and the value matches the
  /// character in the argument.
  ///
  /// @param c The macro character.
  /// @returns True if the SPECIAL character matches the @c c.
  bool is_special(char c) const { return type == type::SPECIAL && !token.empty() && token[0] == c; }
};

inline bool operator==(const token_t& l, const token_t& r) { return l.type == r.type && l.token == r.token; }

inline std::ostream& operator<<(std::ostream& os, const token_t& t)
{
  switch(t.type)
  {
    case token_t::type::SPECIAL:
      os << "SPECIAL:";
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
    case token_t::type::FLOAT:
      os << "FLOAT:";
      break;
    case token_t::type::MACRO:
      os << "MACRO:";
      break;
    case token_t::type::SPLICE:
      os << "SPLICE:";
      break;
    case token_t::type::INFIX:
      os << "INFIX:";
      break;
    default:
      os << "?:";
      break;
  }
  os << t.token;
  return os;
}

/// @brief A lexer of a string input.
class lexer final
{
public:
  lexer(ref_file_t input)
    : _input(std::move(input)),
      _pos(begin(_input->source()))
  {}
  lexer(const std::string& s)
    : _input(ref_file_t::create(s)),
      _pos(begin(_input->source()))
  {}
  /// @brief Read the next token from the input string.
  token_t read();
  void unread(token_t);
  // Syntax table member functions.
  static syntax::type get(std::uint8_t index) { return vm::read_table().get(index); }
  static void set(std::uint8_t index, syntax::type value) { vm::read_table().set(index, value); }
  static void set(std::uint8_t index, lisp_t value) { vm::read_table().set(index, std::move(value)); }
  lisp_t macro(token_t token) { return vm::read_table().macro(_input, token.token[0]); }

private:
  ref_file_t _input;
  typename io::source::iterator _pos;
  token_t _token;
  bool _start_of_line{true};

  void next()
  {
    _start_of_line = vm::read_table().get(*_pos) == syntax::type::NEWLINE;
    ++_pos;
  }

  enum class state_t
  {
    START,      // Starting state
    IN_STRING,  // Inside a string until the next unescaped
                // double quote
    IN_SYMBOL,  // A symbol
    IN_QUOTE,   // Next character is treated as literal
    IN_COMMENT, // Inside comment
    IN_HASH,    // Encountered #, wait for ', otherwise it's symbol
    // If we fail to parse an integer or floating point number the token will
    // turn into a symbol.
    IN_DOT,   // A dot may be the start of a float or a symbol
              // unless it's followed by a terminating
              // character, like '(', ')', or a white space.
    IN_SIGN,  // A lone '+' or '-' is an atom, not an integer.
    IN_INT,   // An integer which may turn into a float if an
              // 'e' or a decimal point is found. It may also
              // turn into a symbol if there are any
              // non-number, non-break characters in the
              // sequence, e.g. '123abc'.
    IN_FLOAT, // Try to parse a floating point number.
    IN_EXP1,  // The state after finding an 'e'.
    IN_EXP2,  // Next state after finding a '+', '-', or a
              // digit.
  };
};

} // namespace lisp

#endif
