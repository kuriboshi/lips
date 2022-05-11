//
// Lips, lisp shell.
// Copyright 1988, 2020-2022 Krister Joas
//

#include "logic.hh"

namespace lisp::logic
{
LISPT p_and(lisp& l, LISPT x)
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

LISPT p_or(lisp& l, LISPT x)
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

LISPT p_not(lisp& l, LISPT x)
{
  if(is_NIL(x))
    return T;
  return NIL;
}

LISPT xif(lisp& l, LISPT pred, LISPT true_expr, LISPT false_expr)
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

void init()
{
  // clang-format off
  mkprim(pn::AND, p_and, subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::OR,  p_or,  subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::NOT, p_not, subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::IF,  xif,   subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  // clang-format on
}

} // namespace lisp::logic
