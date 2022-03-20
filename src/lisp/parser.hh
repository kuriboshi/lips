//
// Lips, lisp shell.
//
// Copyright 2022 Krister Joas
//

#pragma once

#include <deque>
#include <memory>
#include <utility>
#include "alloc.hh"
#include "lisp.hh"
#include "pred.hh"
#include "reader.hh"
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
class reader;

/// @brief The LISP input parser.
///
/// @details The main attraction.
///  
template<typename Input>
class parser
{
public:
  /// @brief Constructor.
  /// @details The parser takes a reader as the only parameter.  The parser
  ///   calls the reader every time it needs the next token.
  /// @param reader A reader object which returns the next token.
  explicit parser(reader<Input>& reader) : _reader(reader) {}
  /// @brief Parse the sequence of tokens supplied by the reader.
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
  /// @brief Creates an appropriate LISPT object of the correct type based on
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
      case token_t::type::FLOAT:
        return mkfloat(std::stod(token.token));
      default:
        return NIL;
    }
    return C_EMPTY;
  }

  LISPT make_symbol(const std::string& symbol) const
  {
    return mkatom(symbol);
  }

  /// @brief Holds the reader object.
  reader<Input>& _reader;
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
  auto token = _reader.read();
  if(token)
    _token = *token;
  return !!token;
}

template<typename Input>
LISPT parser<Input>::parse_object()
{
  if(!next())
    return C_EMPTY;
  if(_token.is_macro('\''))
    return cons(mkatom("quote"), cons(parse_object(), NIL));
  if(_token.is_macro('('))
    return parse_list('(');
  if(_token.is_macro('['))
    return parse_list('[');
  if(_token.is_macro(')'))
    return NIL;
  if(_token.is_macro(']'))
    return NIL;
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

    if(c == '[' && _token.is_macro(')'))
    {
      head = cons(head, NIL);
      tail = head;
      continue;
    }
    if(c == '(' && _token.is_macro(')'))
      return head;
    else if(c == '(' && _token.is_macro(']'))
    {
      _reader.unread(_token);
      return head;
    }
    else if(c == '[' && _token.is_macro(']'))
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
  if(_token.is_macro('.'))
  {
    auto object = parse_list('(');
    _reader.unread(_token);
    if(listp(object) && cdr(object) == NIL)
      return car(object);
    return cons(make_symbol("."), object);
  }
  _reader.unread(_token);
  auto object = parse_object();
  return cons(object, NIL);
}

}
