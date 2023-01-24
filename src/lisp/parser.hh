//
// Lips, lisp shell.
//
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

#ifndef LISP_PARSER_HH
#define LISP_PARSER_HH

#include <string>

#include "alloc.hh"
#include "types.hh"
#include "lexer.hh"

namespace lisp
{

template<typename T>
inline bool empty(T& t)
{
  return t && t->empty();
}

class lexer;

/// @brief The LISP input parser.
///
/// @details The main attraction.
///
class parser final
{
public:
  /// @brief Constructor.
  /// @details The parser takes a lexer as the only parameter.  The parser
  ///   calls the lexer every time it needs the next token.
  /// @param lexer A lexer object which returns the next token.
  explicit parser(lexer& lexer)
    : _lexer(lexer)
  {}
  /// @brief Parse the sequence of tokens supplied by the lexer.
  /// @return The return value is the SEXPR.
  lisp_t parse() { return parse_object(); }

  /// @brief Creates an appropriate lisp_t object of the correct type based on
  /// the token.
  lisp_t create(const token_t& token) const
  {
    switch(token.type)
    {
      case token_t::type::SPECIAL:
        return mkatom(token.token);
      case token_t::type::SYMBOL:
        // Consider the token 'nil' to be nil even in unevaluated contexts.
        if(token.token == "nil")
          return nil;
        return mkatom(token.token);
      case token_t::type::STRING:
        return mkstring(token.token);
      case token_t::type::INT:
        return mknumber(std::stoi(token.token));
      case token_t::type::FLOAT:
        return mkfloat(std::stod(token.token));
      default:
        break;
    }
    return nil;
  }

private:
  /// @brief Gets the next token and sets _token.
  /// @return Returns true if the token retrieved is valid and false if the end
  ///   of the input token stream is reached.
  bool next()
  {
    _token = _lexer.read();
    return !!_token;
  }

  /// @brief Parse an object.
  /// @return Either the object or nothing if there is a parse error.
  lisp_t parse_object();
  /// @brief Parse a list of objects.
  /// @return A list of objects or nothing if there is a parse error. This can
  /// happen if the input source ends before a complete list has been read.
  lisp_t parse_list(char);
  /// @brief Parse the tail of a list.
  lisp_t parse_tail();
  /// @brief Make a symbol from a string.
  lisp_t make_symbol(const std::string& symbol) const { return mkatom(symbol); }

  /// @brief Holds the lexer object.
  lexer& _lexer;
  /// @brief The current input token.
  token_t _token;
};

} // namespace lisp

#endif
