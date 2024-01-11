//
// Lips, lisp shell.
// Copyright 1988, 2020-2024 Krister Joas
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

#include "exec.hh"
#include "transform.hh"

#include <lisp/lisp.hh>

using namespace lisp;

lisp_t put_end(lisp_t list, lisp_t obj, bool conc)
{
  if(is_nil(list))
  {
    if(conc)
      return obj;
    return cons(obj, nil);
  }
  lisp_t t;
  for(t = list; type_of(t->cdr()) == object::type::Cons; t = t->cdr())
    ;
  if(conc)
    rplacd(t, obj);
  else
    rplacd(t, cons(obj, nil));
  return list;
}

lisp_t transform(const lisp_t& func, lisp_t res, lisp_t tail, bool conc)
{
  if(is_nil(res))
    return cons(func, cons(tail, nil));
  return cons(func, cons(put_end(res, tail, conc), nil));
}

lisp_t transform(const lisp_t& list)
{
  lisp_t tail = nil;
  lisp_t res = nil;
  bool conc = false;
  for(auto ll = list; type_of(ll) == object::type::Cons; ll = ll->cdr())
  {
    if(type_of(ll->car()) == object::type::Cons)
      tail = put_end(tail, transform(ll->car()), conc);
    else if(ll->car() == atoms::BAR)
    {
      res = transform(atoms::PIPE, res, tail, conc);
      tail = nil;
      conc = false;
    }
    else if(ll->car() == atoms::SEMI)
    {
      // Semicolon is considered a comment character. If progn transformation
      // is to be effective ';' cannot be a comment character.
      res = transform(atoms::PROGN, res, tail, conc);
      tail = nil;
      conc = false;
    }
    else if(ll->car() == atoms::GT)
    {
      res = transform(atoms::REDIR_TO, res, tail, conc);
      tail = nil;
      conc = true;
    }
    else if(ll->car() == atoms::GGT)
    {
      res = transform(atoms::REDIR_APPEND, res, tail, conc);
      tail = nil;
      conc = true;
    }
    else if(ll->car() == atoms::LT)
    {
      res = transform(atoms::REDIR_FROM, res, tail, conc);
      tail = nil;
      conc = true;
    }
    else if(ll->car() == atoms::AMPER)
    {
      res = transform(atoms::BACK, res, tail, conc);
      tail = nil;
      conc = true;
    }
    else
      tail = put_end(tail, ll->car(), false);
  }
  if(is_nil(res))
    return tail;
  if(!is_nil(tail))
    return put_end(res, tail, conc);
  return res;
}
