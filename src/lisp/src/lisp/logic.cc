/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include "libisp.hh"

namespace lisp
{
logic::logic(): base() {}
logic::logic(lisp& lisp): base(lisp) {}

PRIMITIVE logic::p_and(LISPT x)
{
  LISPT foo = C_T;
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
  LISPT foo = C_NIL;
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
    return C_T;
  return C_NIL;
}

PRIMITIVE logic::xif(LISPT pred, LISPT true_expr, LISPT false_expr)
{
  LISPT foo = eval(l, pred);
  if(is_NIL(foo))
    return progn(l, false_expr);
  return eval(l, true_expr);
}

inline constexpr auto PN_AND = "and"; // and
inline constexpr auto PN_OR = "or";   // or
inline constexpr auto PN_NOT = "not"; // not
inline constexpr auto PN_IF = "if";   // if a then b else c

void logic::init()
{
  // clang-format off
  mkprim(PN_AND, ::lisp::p_and, subr_t::subr::NOEVAL, subr_t::spread::SPREAD);
  mkprim(PN_OR,  ::lisp::p_or,  subr_t::subr::NOEVAL, subr_t::spread::SPREAD);
  mkprim(PN_NOT, ::lisp::p_not, subr_t::subr::EVAL,   subr_t::spread::NOSPREAD);
  mkprim(PN_IF,  ::lisp::xif,   subr_t::subr::NOEVAL, subr_t::spread::SPREAD);
  // clang-format on
}

} // namespace lisp
