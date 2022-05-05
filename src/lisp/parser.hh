//
// Lips, lisp shell.
//
// Copyright 2022 Krister Joas
//

#ifndef LISP_PARSER_HH
#define LISP_PARSER_HH

#include <deque>
#include <memory>
#include <utility>
#include "alloc.hh"
#include "lisp.hh"
#include "pred.hh"
#include "lexer.hh"
#include "ref_ptr.hh"
#include "io.hh"
#include "prim.hh"

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
class parser
{
public:
  /// @brief Constructor.
  /// @details The parser takes a lexer as the only parameter.  The parser
  ///   calls the lexer every time it needs the next token.
  /// @param lexer A lexer object which returns the next token.
  explicit parser(lexer& lexer) : _lexer(lexer) {}
  /// @brief Parse the sequence of tokens supplied by the lexer.
  /// @return The return value is the SEXPR.
  LISPT parse() { return parse_object(); }

  /// @brief Creates an appropriate LISPT object of the correct type based on
  /// the token.
  LISPT create(const token_t& token) const
  {
    switch(token.type)
    {
      case token_t::type::SPECIAL:
        return mkatom(token.token);
      case token_t::type::SYMBOL:
        // Consider the token 'nil' to be NIL even in unevaluated contexts.
        if(token.token == "nil")
          return NIL;
        return mkatom(token.token);
      case token_t::type::STRING:
        return mkstring(token.token);
      case token_t::type::INT:
        return mknumber(std::stoi(token.token));
      case token_t::type::FLOAT:
        return mkfloat(std::stod(token.token));
      default:
        return NIL;
    }
    return C_EMPTY;
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
  LISPT parse_object();
  /// @brief Parse a list of objects.
  /// @return A list of objects or nothing if there is a parse error. This can
  /// happen if the input source ends before a complete list has been read.
  LISPT parse_list(char);
  /// @brief Parse the tail of a list.
  LISPT parse_tail();
  /// @brief Make a symbol from a string.
  LISPT make_symbol(const std::string& symbol) const
  {
    return mkatom(symbol);
  }

  /// @brief Holds the lexer object.
  lexer& _lexer;
  /// @brief The current input token.
  token_t _token;
};

} // namespace lisp

#endif
