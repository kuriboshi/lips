/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 * $Id$
 */
#include "lisp.h"
#include "func.h"

PRIMITIVE p_and(LISPT l)
{
  LISPT foo;

  foo = C_T;
  while (!ISNIL(l))
    {
      foo = eval(CAR(l));
      if (ISNIL(foo))
        return foo;
      l = CDR(l);
    }
  return foo;
}

PRIMITIVE p_or(LISPT l)
{
  LISPT foo;

  foo = C_NIL;
  while (!ISNIL(l))
    {
      foo = eval(CAR(l));
      if (!ISNIL(foo))
        return foo;
      l = CDR(l);
    }
  return foo;
}

PRIMITIVE not(LISPT x)
{
  if (ISNIL(x))
    return C_T;
  else
    return C_NIL;
}

PRIMITIVE xif(LISPT pred, LISPT true_expr, LISPT false_expr)
{
  LISPT foo;

  foo = eval(pred);
  if (ISNIL(foo))
    return progn(false_expr);
  else
    return eval(true_expr);
}

void init_logic()
{
  mkprim1(PN_AND, p_and, -1, FSUBR);
  mkprim1(PN_OR, p_or, -1, FSUBR);
  mkprim1(PN_NOT, not, 1, SUBR);
  mkprim3(PN_IF, xif, -3, FSUBR);
}
