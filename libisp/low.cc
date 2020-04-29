/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include "libisp.hh"

namespace lisp
{
LISPT verboseflg;

/*
Dummy definition for cpprint.
PRIMITIVE set(var, val)
*/
PRIMITIVE set(LISPT var, LISPT val)
{
  check(var, SYMBOL);
  if(EQ(var, CE_NIL) || EQ(var, CE_T))
    return error(ATTEMPT_TO_RESET, var);
  if(TYPEOF(var->symval().value) == INDIRECT)
    var->symval().value->indirectval() = val;
  else if(TYPEOF(var->symval().value) == CVARIABLE)
    *var->symval().value->cvarval() = val;
  else
    var->symvalue(val);
  return val;
}

PRIMITIVE setq(LISPT var, LISPT val) { return set(var, eval(val)); }

PRIMITIVE progn(LISPT lexp)
{
  if(ISNIL(lexp))
    return C_NIL;
  while(!ISNIL(lexp->cdr()))
  {
    eval(lexp->car());
    lexp = lexp->cdr();
  }
  return eval(lexp->car());
}

PRIMITIVE cond(LISPT args)
{
  LISPT res = C_NIL;
  if(ISNIL(args))
    return C_NIL;
  while(ISNIL(res))
  {
    LISPT alt = args->car();
    check(alt, CONS);
    res = eval(alt->car());
    if(!ISNIL(res))
    {
      if(ISNIL(alt->cdr()))
        return res;
      else
        return progn(alt->cdr());
    }
    args = args->cdr();
    if(ISNIL(args))
      break;
  }
  return C_NIL;
}

PRIMITIVE xwhile(LISPT pred, LISPT exp)
{
  LISPT res = eval(pred);
  while(!ISNIL(res))
  {
    progn(exp);
    res = eval(pred);
  }
  return C_NIL;
}

PRIMITIVE prog1(LISPT a1, LISPT) { return a1; }

PRIMITIVE prog2(LISPT, LISPT a2, LISPT) { return a2; }

#if 0
PRIMITIVE topofstack()
{
  return env;
}
#endif

PRIMITIVE envget(LISPT e, LISPT n)
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
  mkprim(PN_SET, set, 2, SUBR);
  mkprim(PN_SETQ, setq, 2, FSUBR);
  mkprim(PN_SETQQ, set, 2, FSUBR);
  mkprim(PN_COND, cond, -1, FSUBR);
  mkprim(PN_WHILE, xwhile, -2, FSUBR);
  mkprim(PN_PROGN, progn, -1, FSUBR);
  mkprim(PN_PROG1, prog1, -2, SUBR);
  mkprim(PN_PROG2, prog2, -3, SUBR);
#if 0
  mkprim(PN_TOPOFSTACK, topofstack,  0, SUBR);
#endif
  mkprim(PN_ENVGET, envget, 2, SUBR);
  initcvar(&verboseflg, "verboseflg", C_NIL);
}

} // namespace lisp
