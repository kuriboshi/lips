/*
 * Lips, lisp shell.
 * Copyright 1988, Krister Joas
 *
 * $Id$
 */
#include "lisp.h"

#ifndef lint
static char rcsid[] = "$Id$";
#endif

PRIMITIVE and(l)
register LISPT l;
{
  register LISPT foo;
  
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

PRIMITIVE or(l)
  register LISPT l;
{
  register LISPT foo;
  
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

PRIMITIVE not(x)
  LISPT x;
{
  if (ISNIL(x))
    return C_T;
  else
    return C_NIL;
}

PRIMITIVE xif(pred, true, false)
  LISPT pred, true, false;
{
  LISPT foo;

  foo = eval(pred);
  if (ISNIL(foo))
    return progn(false);
  else
    return eval(true);
}

public void init_logic()
{
  mkprim(PN_AND, and, -1, FSUBR);
  mkprim(PN_OR,  or,  -1, FSUBR);
  mkprim(PN_NOT, not,  1, SUBR);
  mkprim(PN_IF,  xif, -3, FSUBR);
}
