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

private LISPT getargs(al)
  LISPT al;
{
  if (ISNIL(CDR(al)))
    return CAR(al);
  else
    return cons(CAR(al), getargs(CDR(al)));
}

PRIMITIVE getrep(fun)
  LISPT fun;
{
  LAMBDAT *x;
  LISPT args;
  
  if (TYPEOF(fun) != LAMBDA
      && TYPEOF(fun) != NLAMBDA) return C_NIL;
  x = &LAMVAL(fun);
  if (x->argcnt == -1)
    args = CAR(x->arglist);
  else if (x->argcnt < 0)
    args = getargs (x->arglist);
  else args = x->arglist;
  if (TYPEOF(fun) == LAMBDA)
    return cons (C_LAMBDA, cons (args, x->lambdarep));
  else
    return cons(C_NLAMBDA,cons(args,x->lambdarep));
}

public LISPT funeq(f1, f2)
  LISPT f1, f2;
{
  LISPT t1, t2;
  LISPT tmp;

  if (EQ(f1, f2))
    return C_T;
  if(LAMVAL(f1).argcnt == LAMVAL(f2).argcnt)
    {
      t1 = LAMVAL(f1).arglist;
      t2 = LAMVAL(f2).arglist;
      tmp = equal (t1, t2);
      if (!ISNIL (tmp))
        {
          t1 = LAMVAL(f1).lambdarep;
          t2 = LAMVAL(f2).lambdarep;
	  tmp = equal (t1, t2);
	  if (!ISNIL (tmp))
            return C_T;
        }
    }
  return C_NIL;
}

private LISPT checkfn(name, lam)
  LISPT name, lam;
{
  LISPT t;

  if (TYPEOF(GETOPVAL(name)) != UNBOUND)
    if (TYPEOF(GETOPVAL(name)) == LAMBDA
        || TYPEOF(GETOPVAL(name)) == NLAMBDA)
      {
        t = funeq(GETOPVAL(name), lam);
        if (ISNIL(t))
          {
            (void) putprop(name, C_OLDDEF, GETOPVAL(name));
            if (!ISNIL (verboseflg))
              (void) xprint(cons(name, cons(C_REDEFINED, C_NIL)), C_NIL);
          }
      }
}

PRIMITIVE define(name, lam)
  LISPT name, lam;
{
  CHECK(name, SYMBOL);
  CHECK2(lam, LAMBDA, NLAMBDA);
  checkfn(name, lam);
  SETOPVAL(name, lam);
  return name;
}

private LISPT def(name, pars, body, type)
  LISPT name, pars, body;
  int type;
{
  LISPT foo;

  CHECK(name, SYMBOL);
  if (!ISNIL(pars) && TYPEOF(pars) != SYMBOL)
    CHECK(pars, CONS);
  foo = mklambda(pars, body, type);
  if (TYPEOF(foo) == ERROR)
    return C_NIL;
  checkfn(name, foo);
  SETOPVAL(name, foo);
  return cons(name, C_NIL);
}

PRIMITIVE de(name, pars, body)
  LISPT name, pars, body;
{
  return def(name, pars, body, LAMBDA);
}

PRIMITIVE df(name, pars, body)
  LISPT name, pars, body;
{
  return def(name, pars, body, NLAMBDA);
}

public void init_user()
{
  mkprim(PN_DEFINE, define,  2, SUBR);
  mkprim(PN_GETREP, getrep,  1, SUBR);
  mkprim(PN_DE,     de,     -3, FSUBR);
  mkprim(PN_DF,     df,     -3, FSUBR);
}
