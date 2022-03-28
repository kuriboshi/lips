//
// Lips, lisp shell.
// Copyright 1988, 2020-2022 Krister Joas
//

#include "low.hh"
#include "alloc.hh"
#include "eval.hh"

namespace lisp::low
{
LISPT set(lisp& l, LISPT var, LISPT val)
{
  check(var, type::SYMBOL);
  if(var->symbol().constant)
    return l.error(ATTEMPT_TO_CLOBBER, var);
  if(type_of(var->value()) == type::INDIRECT)
    var->value()->set(indirect_t{val});
  else if(type_of(var->value()) == type::CVARIABLE)
  {
    auto symval = var->value();
    auto& cvar = symval->cvarval();
    cvar = val;
  }
  else
    var->value(val);
  return val;
}

LISPT setq(lisp& l, LISPT var, LISPT val) { return low::set(l, var, eval(l, val)); }

LISPT progn(lisp& l, LISPT lexp)
{
  if(is_NIL(lexp))
    return NIL;
  while(!is_NIL(lexp->cdr()))
  {
    eval(l, lexp->car());
    lexp = lexp->cdr();
  }
  return eval(l, lexp->car());
}

/// @brief The cond special form.
///
/// @details The generalized conditional special form. The function takes zero
/// or more clauses. Each clause has one test followed by zero or more
/// expressions called consequents. The function evaluates each test in
/// sequence until one of them is evaluated to true (not @c NIL). It then
/// evaluates each consequent in order and returns the value of the last
/// consequent. If there are no consequents the result is the value of the test
/// expression. The degenerate @c cond expression with no clauses at all
/// evaluates to @c NIL.
///
/// In the following example the return value is the value of the expression
/// @c r0 if @c e0 evaluates to non-NIL, @c r2 if @c e1 is evaluated to non-NIL,
/// @c e2 if @c e2 evaluates to non-NIL. Finally, if none of the expressions
/// @c e0, @c e1, or @c e2 is non-nil the final @c t provides a default
/// value. If none of the test expressions evaluate to non-NIL then the result
/// of the entire @c cond expression is @c NIL.
///
/// Note that the expressions after the conditional expressions are evaluated
/// in an implicit @c progn which is why the result of @c e1 being non-NIL is
/// the value of @c r2.
///
/// @code{.lisp}
/// (cond (e0 r0)
///       (e1 r1 r2)
///       (e2)
///       (t r3))
/// @endcode
///
LISPT cond(lisp& l, LISPT args)
{
  while(!is_NIL(args))
  {
    auto alt = args->car();
    check(alt, type::CONS);
    auto res = eval(l, alt->car());
    if(!is_NIL(res))
    {
      if(is_NIL(alt->cdr()))
        return res;
      return low::progn(l, alt->cdr());
    }
    args = args->cdr();
  }
  return NIL;
}

LISPT xwhile(lisp& l, LISPT pred, LISPT exp)
{
  LISPT res = eval(l, pred);
  while(!is_NIL(res))
  {
    low::progn(l, exp);
    res = eval(l, pred);
  }
  return NIL;
}

LISPT prog1(lisp& l, LISPT a1, LISPT) { return a1; }

namespace pn
{
inline constexpr auto SET = "set";     // set variable
inline constexpr auto SETQ = "setq";   // set quoted variable
inline constexpr auto SETQQ = "setqq"; // noeval set
inline constexpr auto COND = "cond";   // cond
inline constexpr auto WHILE = "while"; // while t
inline constexpr auto PROGN = "progn"; // return last expression
inline constexpr auto PROG1 = "prog1"; // return first expression
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

} // namespace lisp::low
