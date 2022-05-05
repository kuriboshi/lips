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

template<typename Input>
class lexer;

/// @brief The LISP input parser.
///
/// @details The main attraction.
///  
template<typename Input>
class parser
{
public:
  /// @brief Constructor.
  /// @details The parser takes a lexer as the only parameter.  The parser
  ///   calls the lexer every time it needs the next token.
  /// @param lexer A lexer object which returns the next token.
  explicit parser(lexer<Input>& lexer) : _lexer(lexer) {}
  /// @brief Parse the sequence of tokens supplied by the lexer.
  /// @return The return value is the SEXPR.
  LISPT parse();

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
  /// @brief Make a symbol from a string.
  LISPT make_symbol(const std::string& symbol) const
  {
    return mkatom(symbol);
  }

  /// @brief Holds the lexer object.
  lexer<Input>& _lexer;
  /// @brief The current input token.
  token_t _token;
};

template<typename Input>
LISPT parser<Input>::parse()
{
  return parse_object();
}

template<typename Input>
bool parser<Input>::next()
{
  _token = _lexer.read();
  return !!_token;
}

template<typename Input>
LISPT parser<Input>::parse_object()
{
  if(!next())
    return C_EMPTY;
  if(_token.is_special('\''))
    return cons(mkatom("quote"), cons(parse_object(), NIL));
  if(_token.is_special('('))
    return parse_list('(');
  if(_token.is_special('['))
    return parse_list('[');
  if(_token.is_special(')'))
    return NIL;
  if(_token.is_special(']'))
    return NIL;
  if(_token.type == token_t::type::MACRO)
    return _lexer.macro(_token);
  return create(_token);
}

template<typename Input>
LISPT parser<Input>::parse_list(char c)
{
  LISPT head;
  LISPT tail;
  while(true)
  {
    if(!next())
      return head;

    if(c == '[' && _token.is_special(')'))
    {
      head = cons(head, NIL);
      tail = head;
      continue;
    }
    if(c == '(' && _token.is_special(')'))
      return head;
    else if(c == '(' && _token.is_special(']'))
    {
      _lexer.unread(_token);
      return head;
    }
    else if(c == '[' && _token.is_special(']'))
      return head;

    auto object = parse_tail();

    if(head == NIL)
      tail = head = object;
    else
      tail = cdr(rplacd(tail, object));
  }
  return NIL;
}

template<typename Input>
LISPT parser<Input>::parse_tail()
{
  if(_token.is_special('.'))
  {
    auto object = parse_list('(');
    _lexer.unread(_token);
    if(listp(object) && cdr(object) == NIL)
      return car(object);
    return cons(make_symbol("."), object);
  }
  _lexer.unread(_token);
  auto object = parse_object();
  return cons(object, NIL);
}

} // namespace lisp

#endif
