//
// Lips, lisp shell.
//
// Copyright 2022 Krister Joas
//

#include "parser.hh"

namespace lisp
{
LISPT parser::parse_object()
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

LISPT parser::parse_list(char c)
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

LISPT parser::parse_tail()
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
