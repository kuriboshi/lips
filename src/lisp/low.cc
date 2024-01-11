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
#include "check.hh"
#include "low.hh"

namespace lisp::details::low
{
lisp_t cond(lisp_t args)
{
  while(!is_nil(args))
  {
    auto alt = args->car();
    check(alt, object::type::Cons);
    auto res = eval(alt->car());
    if(!is_nil(res))
    {
      if(is_nil(alt->cdr()))
        return res;
      return low::progn(alt->cdr());
    }
    args = args->cdr();
  }
  return nil;
}

lisp_t prog1(lisp_t a1, lisp_t) { return a1; }

lisp_t progn(lisp_t lexp)
{
  if(is_nil(lexp))
    return nil;
  while(!is_nil(lexp->cdr()))
  {
    eval(lexp->car());
    lexp = lexp->cdr();
  }
  return eval(lexp->car());
}

lisp_t set(lisp_t var, lisp_t val)
{
  check(var, object::type::Symbol);
  var->value(val);
  return val;
}

lisp_t setq(lisp_t var, lisp_t val) { return low::set(var, eval(val)); }

lisp_t xwhile(lisp_t pred, lisp_t exp)
{
  lisp_t res = eval(pred);
  while(!is_nil(res))
  {
    low::progn(exp);
    res = eval(pred);
  }
  return nil;
}

namespace pn
{
inline constexpr std::string_view COND = "cond";   // cond
inline constexpr std::string_view PROG1 = "prog1"; // return first expression
inline constexpr std::string_view PROGN = "progn"; // return last expression
inline constexpr std::string_view SET = "set";     // set variable
inline constexpr std::string_view SETQ = "setq";   // set quoted variable
inline constexpr std::string_view SETQQ = "setqq"; // noeval set
inline constexpr std::string_view WHILE = "while"; // while t
} // namespace pn

void init()
{
  // clang-format off
  mkprim(pn::SET,   set,    subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::SETQ,  setq,   subr_t::subr::NOEVAL, subr_t::spread::SPREAD);
  mkprim(pn::SETQQ, set,    subr_t::subr::NOEVAL, subr_t::spread::SPREAD);
  mkprim(pn::COND,  cond,   subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::WHILE, xwhile, subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::PROGN, progn,  subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::PROG1, prog1,  subr_t::subr::EVAL,   subr_t::spread::NOSPREAD);
  // clang-format on
}

} // namespace lisp::details::low
