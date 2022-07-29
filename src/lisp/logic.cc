//
// Lips, lisp shell.
// Copyright 1988, 2020-2022 Krister Joas
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

#include "alloc.hh"
#include "logic.hh"
#include "low.hh"

namespace lisp::details::logic
{
LISPT p_and(context&, LISPT x)
{
  LISPT foo = T;
  while(!is_NIL(x))
  {
    foo = eval(x->car());
    if(is_NIL(foo))
      return foo;
    x = x->cdr();
  }
  return foo;
}

LISPT p_or(context&, LISPT x)
{
  LISPT foo = NIL;
  while(!is_NIL(x))
  {
    foo = eval(x->car());
    if(!is_NIL(foo))
      return foo;
    x = x->cdr();
  }
  return foo;
}

LISPT p_not(context&, LISPT x)
{
  if(is_NIL(x))
    return T;
  return NIL;
}

LISPT xif(context&, LISPT pred, LISPT true_expr, LISPT false_expr)
{
  LISPT foo = eval(pred);
  if(is_NIL(foo))
    return progn(false_expr);
  return eval(true_expr);
}

namespace pn
{
inline constexpr auto AND = "and"; // and
inline constexpr auto OR = "or";   // or
inline constexpr auto NOT = "not"; // not
inline constexpr auto IF = "if";   // if a then b else c
} // namespace pn

void init()
{
  // clang-format off
  mkprim(pn::AND, p_and, subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::OR,  p_or,  subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::NOT, p_not, subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::IF,  xif,   subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  // clang-format on
}

} // namespace lisp::details::logic
