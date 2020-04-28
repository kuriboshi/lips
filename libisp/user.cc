/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 * $Id$
 */

#include "libisp.hh"
#include "low.hh"

namespace lisp
{
static LISPT getargs(LISPT al)
{
  if(ISNIL(al->cdr()))
    return al->car();
  else
    return cons(al->car(), getargs(al->cdr()));
}

PRIMITIVE getrep(LISPT fun)
{
  LISPT args;

  if(TYPEOF(fun) != LAMBDA && TYPEOF(fun) != NLAMBDA)
    return C_NIL;
  auto& x = fun->lamval();
  if(x.argcnt == -1)
    args = x.arglist->car();
  else if(x.argcnt < 0)
    args = getargs(x.arglist);
  else
    args = x.arglist;
  if(TYPEOF(fun) == LAMBDA)
    return cons(C_LAMBDA, cons(args, x.lambdarep));
  return cons(C_NLAMBDA, cons(args, x.lambdarep));
}

LISPT funeq(LISPT f1, LISPT f2)
{
  if(EQ(f1, f2))
    return C_T;
  if(f1->lamval().argcnt == f2->lamval().argcnt)
  {
    LISPT t1 = f1->lamval().arglist;
    LISPT t2 = f2->lamval().arglist;
    LISPT tmp = equal(t1, t2);
    if(!ISNIL(tmp))
    {
      t1 = f1->lamval().lambdarep;
      t2 = f2->lamval().lambdarep;
      tmp = equal(t1, t2);
      if(!ISNIL(tmp))
        return C_T;
    }
  }
  return C_NIL;
}

static LISPT checkfn(LISPT name, LISPT lam)
{
  if(TYPEOF(name->getopval()) != UNBOUND)
    if(TYPEOF(name->getopval()) == LAMBDA || TYPEOF(name->getopval()) == NLAMBDA)
    {
      LISPT t = funeq(name->getopval(), lam);
      if(ISNIL(t))
      {
        putprop(name, C_OLDDEF, name->getopval());
        if(!ISNIL(verboseflg))
          xprint(cons(name, cons(C_REDEFINED, C_NIL)), C_NIL);
      }
    }
  return C_NIL;
}

PRIMITIVE define(LISPT name, LISPT lam)
{
  check(name, SYMBOL);
  check2(lam, LAMBDA, NLAMBDA);
  checkfn(name, lam);
  name->setopval(lam);
  return name;
}

static LISPT def(LISPT name, LISPT pars, LISPT body, lisp_type type)
{
  check(name, SYMBOL);
  if(!ISNIL(pars) && TYPEOF(pars) != SYMBOL)
    check(pars, CONS);
  LISPT foo = mklambda(pars, body, type);
  if(TYPEOF(foo) == ERROR)
    return C_NIL;
  checkfn(name, foo);
  name->setopval(foo);
  return cons(name, C_NIL);
}

PRIMITIVE de(LISPT name, LISPT pars, LISPT body) { return def(name, pars, body, LAMBDA); }

PRIMITIVE df(LISPT name, LISPT pars, LISPT body) { return def(name, pars, body, NLAMBDA); }

void init_user()
{
  mkprim(PN_DEFINE, define, 2, SUBR);
  mkprim(PN_GETREP, getrep, 1, SUBR);
  mkprim(PN_DE, de, -3, FSUBR);
  mkprim(PN_DF, df, -3, FSUBR);
}

} // namespace lisp
