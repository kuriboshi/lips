//
// Lips, lisp shell.
// Copyright 1988, 2020-2023 Krister Joas
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
#include "details/logic.hh"

namespace lisp::details::logic
{
lisp_t p_and(lisp_t x)
{
  lisp_t foo = T;
  while(!is_nil(x))
  {
    foo = eval(x->car());
    if(is_nil(foo))
      return foo;
    x = x->cdr();
  }
  return foo;
}

lisp_t p_or(lisp_t x)
{
  lisp_t foo = nil;
  while(!is_nil(x))
  {
    foo = eval(x->car());
    if(!is_nil(foo))
      return foo;
    x = x->cdr();
  }
  return foo;
}

lisp_t p_not(lisp_t x)
{
  if(is_nil(x))
    return T;
  return nil;
}

lisp_t xif(lisp_t pred, lisp_t true_expr, lisp_t false_expr)
{
  lisp_t foo = eval(pred);
  if(is_nil(foo))
    return progn(false_expr);
  return eval(true_expr);
}

namespace pn
{
inline constexpr std::string_view AND = "and"; // and
inline constexpr std::string_view OR = "or";   // or
inline constexpr std::string_view NOT = "not"; // not
inline constexpr std::string_view IF = "if";   // if a then b else c
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
