/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 * $Id$
 */
#include "lisp.h"

#ifndef lint
static char rcsid[] = "$Id$";
#endif

LISPT verboseflg;

/*
Dummy definition for cpprint.
PRIMITIVE set(var, val)
*/
PRIMITIVE set(var, val)
  LISPT var, val;
{
  CHECK(var, SYMBOL);
  if (EQ(var, CE_NIL) || EQ(var, CE_T))
    return error(ATTEMPT_TO_RESET, var);
  if (TYPEOF(SYMVAL(var).value) == INDIRECT)
    INDIRECTVAL(SYMVAL(var).value) = val;
  else if (TYPEOF(SYMVAL(var).value) == CVARIABLE)
    *CVARVAL(SYMVAL(var).value) = val;
  else
    SYMVAL(var).value = val;
  return val;
}

PRIMITIVE setq(var, val)
  LISPT var, val;
{
  return set(var, eval(val));
}

PRIMITIVE progn(lexp)
  LISPT lexp;
{
  if (ISNIL(lexp))
    return C_NIL;
  while (!ISNIL(CDR(lexp)))
    {
      (void)eval(CAR(lexp));
      lexp = CDR(lexp);
    }
  return eval(CAR(lexp));
}

PRIMITIVE cond(args)
  LISPT args;
{
  LISPT alt;
  LISPT res;
  
  res = C_NIL;
  if (ISNIL(args))
    return C_NIL;
  while (ISNIL(res))
    {
      alt = CAR(args);
      CHECK(alt, CONS);
      res = eval(CAR(alt));
      if (!ISNIL(res))
        {
          if (ISNIL(CDR(alt)))
            return res;
          else
            return progn(CDR(alt));
        }
      args = CDR(args);
      if (ISNIL(args))
        break;
    }
  return C_NIL;
}

PRIMITIVE xwhile(pred, exp)
  LISPT pred, exp;
{
  LISPT res;

  res = eval(pred);
  while(!ISNIL(res))
    {
      (void)progn(exp);
      res = eval(pred);
    }
  return C_NIL;
}

/*ARGSUSED*/
PRIMITIVE prog1(a1, la)
  LISPT a1, la;
{
  return a1;
}

/*ARGSUSED*/
PRIMITIVE prog2(a1, a2, la)
  LISPT a1, a2, la;
{
  return a2;
}

#if 0
PRIMITIVE topofstack()
{
  return env;
}
#endif

PRIMITIVE envget(e, n)
  LISPT e, n;
{
#if 0
  LISPT foo;
  
  CHECK(e, ENVIRON);
  CHECK(n, INTEGER);
  if (INTVAL(n) <= 0)
    foo = cons(CAR(ENVVAL(e)), mknumber(CDR(ENVVAL(e))));
  else
    if (INTVAL(n) <= INTVAL(CDR(e)))
      foo = cons (CAR(ENVVAL(e) + INTVAL(n)),
		  CDR(ENVVAL(e) + INTVAL(n)));
    else
      foo = C_NIL;
  return foo;
#else
  return e;
#endif
}

void init_low()
{
  mkprim(PN_SET,        set,         2, SUBR);
  mkprim(PN_SETQ,       setq,        2, FSUBR);
  mkprim(PN_SETQQ,      set,         2, FSUBR);
  mkprim(PN_COND,       cond,       -1, FSUBR);
  mkprim(PN_WHILE,      xwhile,     -2, FSUBR);
  mkprim(PN_PROGN,      progn,      -1, FSUBR);
  mkprim(PN_PROG1,      prog1,      -2, SUBR);
  mkprim(PN_PROG2,      prog2,      -3, SUBR);
#if 0
  mkprim(PN_TOPOFSTACK, topofstack,  0, SUBR);
#endif
  mkprim(PN_ENVGET,     envget,      2, SUBR);
  initcvar(&verboseflg, "verboseflg", C_NIL);
}
