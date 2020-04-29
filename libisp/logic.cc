/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include "libisp.hh"

namespace lisp
{
PRIMITIVE p_and(LISPT l)
{
  LISPT foo = C_T;
  while(!is_NIL(l))
  {
    foo = eval(l->car());
    if(is_NIL(foo))
      return foo;
    l = l->cdr();
  }
  return foo;
}

PRIMITIVE p_or(LISPT l)
{
  LISPT foo = C_NIL;
  while(!is_NIL(l))
  {
    foo = eval(l->car());
    if(!is_NIL(foo))
      return foo;
    l = l->cdr();
  }
  return foo;
}

PRIMITIVE p_not(LISPT x)
{
  if(is_NIL(x))
    return C_T;
  return C_NIL;
}

PRIMITIVE xif(LISPT pred, LISPT true_expr, LISPT false_expr)
{
  LISPT foo = eval(pred);
  if(is_NIL(foo))
    return progn(false_expr);
  return eval(true_expr);
}

logic::logic()
{
  mkprim(PN_AND, p_and, -1, FSUBR);
  mkprim(PN_OR, p_or, -1, FSUBR);
  mkprim(PN_NOT, p_not, 1, SUBR);
  mkprim(PN_IF, xif, -3, FSUBR);
}

} // namespace lisp
