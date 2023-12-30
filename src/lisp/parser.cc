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

#include "predicate.hh"
#include "list.hh"
#include "parser.hh"

namespace lisp
{
lisp_t parser::parse_object()
{
  if(!next())
    return C_EOF;
  if(_token.is_special('\''))
    return cons(mkatom("quote"), cons(parse_object(), nil));
  if(_token.is_special('('))
    return parse_list('(');
  if(_token.is_special('['))
    return parse_list('[');
  if(_token.is_special(')'))
    return nil;
  if(_token.is_special(']'))
    return nil;
  if(_token.type == token_t::type::MACRO)
    return _lexer->macro(_token);
  if(_token.type == token_t::type::SPLICE)
    return _lexer->macro(_token);
  if(_token.type == token_t::type::INFIX)
    return _lexer->macro(_token);
  return create(_token);
}

lisp_t parser::parse_list(char c)
{
  lisp_t head;
  lisp_t tail;
  while(true)
  {
    if(!next())
      break;

    if(c == '[' && _token.is_special(')'))
    {
      head = cons(head, nil);
      tail = head;
      continue;
    }
    if(c == '(' && _token.is_special(')'))
      break;
    if(c == '(' && _token.is_special(']'))
    {
      _lexer->unread(_token);
      break;
    }
    if(c == '[' && _token.is_special(']'))
      break;

    auto object = parse_tail();

    if(head == nil)
      tail = head = object;
    else
      tail = cdr(rplacd(tail, object));
  }
  return head;
}

lisp_t parser::parse_tail()
{
  if(_token.is_special('.'))
  {
    auto object = parse_list('(');
    _lexer->unread(_token);
    if(listp(object) && cdr(object) == nil)
      return car(object);
    return cons(make_symbol("."), object);
  }
  _lexer->unread(_token);
  auto object = parse_object();
  return cons(object, nil);
}
} // namespace lisp
