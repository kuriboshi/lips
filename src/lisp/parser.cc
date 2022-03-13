//
// Lips, lisp shell.
// Copyright 2022 Krister Joas
//

#include <sstream>
#include "parser.hh"
#include "io.hh"
#include "prim.hh"
#include "pred.hh"
#include "reader.hh"

using namespace std::literals;

namespace lisp
{

LISPT Parser::parse()
{
  auto head = parse_object();
  LISPT tail;
  while(true)
  {
    auto o = parse_object();
    if(o && o->empty())
      break;
    if(tail == NIL)
      tail = cdr(head = cons(head, cons(o, NIL)));
    else
      tail = cdr(rplacd(tail, cons(o, NIL)));
  }
  if(head && head->empty())
    return NIL;
  return head;
}

bool Parser::next()
{
  auto token = _reader.read();
  if(token)
    _token = *token;
  return !!token;
}

LISPT Parser::parse_object()
{
  if(!next())
    return C_EMPTY;
  if(_token.is_macro('('))
    return parse_list('(');
  if(_token.is_macro('['))
    return parse_list('[');
  if(_token.is_macro(')'))
    return C_EMPTY;
  if(_token.is_macro(']'))
    return C_EMPTY;
  return create(_token);
}

LISPT Parser::parse_list(char c)
{
  LISPT head;
  LISPT tail;
  while(true)
  {
    next();

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

LISPT Parser::parse_tail()
{
  if(_token.is_macro('.'))
  {
    auto object = parse_list('(');
    if(listp(object) && cdr(object) == NIL)
      return car(object);
    return cons(make_symbol("."), object);
  }
  _reader.unread(_token);
  auto object = parse_object();
  return cons(object, NIL);
}

}
