/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include "logic.hh"
#include "alloc.hh"
#include "eval.hh"
#include "low.hh"

namespace lisp
{
logic::logic(): base() {}
logic::logic(lisp& lisp): base(lisp) {}

PRIMITIVE logic::p_and(LISPT x)
{
  LISPT foo = T;
  while(!is_NIL(x))
  {
    foo = eval(l, x->car());
    if(is_NIL(foo))
      return foo;
    x = x->cdr();
  }
  return foo;
}

PRIMITIVE logic::p_or(LISPT x)
{
  LISPT foo = NIL;
  while(!is_NIL(x))
  {
    foo = eval(l, x->car());
    if(!is_NIL(foo))
      return foo;
    x = x->cdr();
  }
  return foo;
}

PRIMITIVE logic::p_not(LISPT x)
{
  if(is_NIL(x))
    return T;
  return NIL;
}

PRIMITIVE logic::xif(LISPT pred, LISPT true_expr, LISPT false_expr)
{
  LISPT foo = eval(l, pred);
  if(is_NIL(foo))
    return progn(l, false_expr);
  return eval(l, true_expr);
}

namespace pn
{
inline constexpr auto AND = "and"; // and
inline constexpr auto OR = "or";   // or
inline constexpr auto NOT = "not"; // not
inline constexpr auto IF = "if";   // if a then b else c
} // namespace pn

void logic::init()
{
  // clang-format off
  mkprim(pn::AND, ::lisp::p_and, subr_t::subr::NOEVAL, subr_t::spread::SPREAD);
  mkprim(pn::OR,  ::lisp::p_or,  subr_t::subr::NOEVAL, subr_t::spread::SPREAD);
  mkprim(pn::NOT, ::lisp::p_not, subr_t::subr::EVAL,   subr_t::spread::NOSPREAD);
  mkprim(pn::IF,  ::lisp::xif,   subr_t::subr::NOEVAL, subr_t::spread::SPREAD);
  // clang-format on
}

} // namespace lisp
