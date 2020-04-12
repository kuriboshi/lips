/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 * $Id$
 */
#include "lisp.hh"

static LISPT getargs(LISPT al)
{
  if (ISNIL(CDR(al)))
    return CAR(al);
  else
    return cons(CAR(al), getargs(CDR(al)));
}

PRIMITIVE getrep(LISPT fun)
{
  LAMBDAT* x;
  LISPT args;

  if (TYPEOF(fun) != LAMBDA && TYPEOF(fun) != NLAMBDA)
    return C_NIL;
  x = &LAMVAL(fun);
  if (x->argcnt == -1)
    args = CAR(x->arglist);
  else if (x->argcnt < 0)
    args = getargs(x->arglist);
  else
    args = x->arglist;
  if (TYPEOF(fun) == LAMBDA)
    return cons(C_LAMBDA, cons(args, x->lambdarep));
  else
    return cons(C_NLAMBDA, cons(args, x->lambdarep));
}

LISPT funeq(LISPT f1, LISPT f2)
{
  LISPT t1, t2;
  LISPT tmp;

  if (EQ(f1, f2))
    return C_T;
  if (LAMVAL(f1).argcnt == LAMVAL(f2).argcnt)
    {
      t1 = LAMVAL(f1).arglist;
      t2 = LAMVAL(f2).arglist;
      tmp = equal(t1, t2);
      if (!ISNIL(tmp))
        {
          t1 = LAMVAL(f1).lambdarep;
          t2 = LAMVAL(f2).lambdarep;
          tmp = equal(t1, t2);
          if (!ISNIL(tmp))
            return C_T;
        }
    }
  return C_NIL;
}

static LISPT checkfn(LISPT name, LISPT lam)
{
  LISPT t;

  if (TYPEOF(GETOPVAL(name)) != UNBOUND)
    if (TYPEOF(GETOPVAL(name)) == LAMBDA || TYPEOF(GETOPVAL(name)) == NLAMBDA)
      {
        t = funeq(GETOPVAL(name), lam);
        if (ISNIL(t))
          {
            putprop(name, C_OLDDEF, GETOPVAL(name));
            if (!ISNIL(verboseflg))
              xprint(cons(name, cons(C_REDEFINED, C_NIL)), C_NIL);
          }
      }
  return C_NIL;
}

PRIMITIVE define(LISPT name, LISPT lam)
{
  CHECK(name, SYMBOL);
  CHECK2(lam, LAMBDA, NLAMBDA);
  checkfn(name, lam);
  SETOPVAL(name, lam);
  return name;
}

static LISPT def(LISPT name, LISPT pars, LISPT body, enum lisp_type type)
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

PRIMITIVE de(LISPT name, LISPT pars, LISPT body)
{
  return def(name, pars, body, LAMBDA);
}

PRIMITIVE df(LISPT name, LISPT pars, LISPT body)
{
  return def(name, pars, body, NLAMBDA);
}

void init_user()
{
  mkprim2(PN_DEFINE, define, 2, SUBR);
  mkprim1(PN_GETREP, getrep, 1, SUBR);
  mkprim3(PN_DE, de, -3, FSUBR);
  mkprim3(PN_DF, df, -3, FSUBR);
}
