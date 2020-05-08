/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include "libisp.hh"

namespace lisp
{
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

logic::logic(lisp& lisp): base(lisp) {}

void logic::init()
{
  mkprim(PN_AND, ::lisp::p_and, -1, FSUBR);
  mkprim(PN_OR, ::lisp::p_or, -1, FSUBR);
  mkprim(PN_NOT, ::lisp::p_not, 1, SUBR);
  mkprim(PN_IF, ::lisp::xif, -3, FSUBR);
}

} // namespace lisp
