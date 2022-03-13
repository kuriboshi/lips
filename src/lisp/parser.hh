#ifndef LISP_PARSER_HH
#define LISP_PARSER_HH

//
// Lips, lisp shell.
// Copyright 2022 Krister Joas
//

#include <deque>
#include <memory>
#include <utility>
#include "alloc.hh"
#include "lisp.hh"
#include "reader.hh"
#include "ref_ptr.hh"

namespace lisp
{

class Reader;

/// @brief The LISP input parser.
///
/// @details The main attraction.
///  
class Parser
{
public:
  /// @brief Constructor.
  /// @details The Parser takes a Reader as the only parameter.  The Parser
  ///   calls the Reader every time it needs the next token.
  /// @param reader A Reader object which returns the next token.
  explicit Parser(Reader& reader) : _reader(reader) {}
  /// @brief Parse the sequence of tokens supplied by the Reader.
  /// @return The return value is the SEXPR.
  LISPT parse();

private:
  /// @brief Gets the next token and sets _token.
  /// @return Returns true if the token retrieved is valid and false if the end
  ///   of the input token stream is reached.
  bool next();
  /// @brief Parse an object.
  /// @return Either the object or nothing if there is a parse error.
  LISPT parse_object();
  /// @brief Parse a list of objects.
  /// @return A list of objects or nothing if there is a parse error. This can
  /// happen if the input source ends before a complete list has been read.
  LISPT parse_list(char);
  /// @brief Parse the tail of a list.
  LISPT parse_tail();
  /// @brief Creates an appropriate lisp_t object of the correct type based on
  /// the token.
  LISPT create(const token_t& token) const
  {
    switch(token.type)
    {
      case token_t::type::MACRO:
        return mkatom(token.token);
      case token_t::type::SYMBOL:
        return mkatom(token.token);
      case token_t::type::STRING:
        return mkstring(token.token);
      case token_t::type::INT:
        return mknumber(std::stoi(token.token));
      default:
        return NIL;
    }
    return C_EMPTY;
  }

  LISPT make_symbol(const std::string& symbol) const
  {
    return mkatom(symbol);
  }

  /// @brief Holds the Reader object.
  Reader& _reader;
  /// @brief The current input token.
  token_t _token;
};

}

#endif
