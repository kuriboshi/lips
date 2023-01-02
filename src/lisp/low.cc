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
#include "check.hh"
#include "low.hh"

namespace lisp::details::low
{
/// @brief The cond special form.
///
/// @details The generalized conditional special form. The function takes zero
/// or more clauses. Each clause has one test followed by zero or more
/// expressions called consequents. The function evaluates each test in
/// sequence until one of them is evaluated to true (not @c nil). It then
/// evaluates each consequent in order and returns the value of the last
/// consequent. If there are no consequents the result is the value of the test
/// expression. The degenerate @c cond expression with no clauses at all
/// evaluates to @c nil.
///
/// In the following example the return value is the value of the expression
/// @c r0 if @c e0 evaluates to non-nil, @c r2 if @c e1 is evaluated to non-nil,
/// @c e2 if @c e2 evaluates to non-nil. Finally, if none of the expressions
/// @c e0, @c e1, or @c e2 is non-nil the final @c t provides a default
/// value. If none of the test expressions evaluate to non-nil then the result
/// of the entire @c cond expression is @c nil.
///
/// Note that the expressions after the conditional expressions are evaluated
/// in an implicit @c progn which is why the result of @c e1 being non-nil is
/// the value of @c r2.
///
/// @code{.lisp}
/// (cond (e0 r0)
///       (e1 r1 r2)
///       (e2)
///       (t r3))
/// @endcode
///
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
  if(var->symbol()->constant)
    error(error_errc::attempt_to_clobber, var);
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
