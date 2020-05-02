/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include "libisp.hh"

namespace lisp
{
LISPT user::getargs(LISPT al)
{
  if(is_NIL(al->cdr()))
    return al->car();
  else
    return cons(_lisp, al->car(), getargs(al->cdr()));
}

PRIMITIVE user::getrep(LISPT fun)
{
  LISPT args;

  if(type_of(fun) != LAMBDA && type_of(fun) != NLAMBDA)
    return C_NIL;
  auto& x = fun->lamval();
  if(x.argcnt == -1)
    args = x.arglist->car();
  else if(x.argcnt < 0)
    args = getargs(x.arglist);
  else
    args = x.arglist;
  if(type_of(fun) == LAMBDA)
    return cons(_lisp, C_LAMBDA, cons(_lisp, args, x.lambdarep));
  return cons(_lisp, C_NLAMBDA, cons(_lisp, args, x.lambdarep));
}

LISPT user::funeq(LISPT f1, LISPT f2)
{
  if(EQ(f1, f2))
    return C_T;
  if(f1->lamval().argcnt == f2->lamval().argcnt)
  {
    LISPT t1 = f1->lamval().arglist;
    LISPT t2 = f2->lamval().arglist;
    LISPT tmp = equal(_lisp, t1, t2);
    if(!is_NIL(tmp))
    {
      t1 = f1->lamval().lambdarep;
      t2 = f2->lamval().lambdarep;
      tmp = equal(_lisp, t1, t2);
      if(!is_NIL(tmp))
        return C_T;
    }
  }
  return C_NIL;
}

LISPT user::checkfn(LISPT name, LISPT lam)
{
  if(type_of(name->getopval()) != UNBOUND)
    if(type_of(name->getopval()) == LAMBDA || type_of(name->getopval()) == NLAMBDA)
    {
      LISPT t = funeq(name->getopval(), lam);
      if(is_NIL(t))
      {
        putprop(_lisp, name, C_OLDDEF, name->getopval());
        if(!is_NIL(verboseflg))
          xprint(_lisp, cons(_lisp, name, cons(_lisp, C_REDEFINED, C_NIL)), C_NIL);
      }
    }
  return C_NIL;
}

PRIMITIVE user::define(LISPT name, LISPT lam)
{
  _lisp.check(name, SYMBOL);
  _lisp.check2(lam, LAMBDA, NLAMBDA);
  checkfn(name, lam);
  name->setopval(lam);
  return name;
}

LISPT user::def(LISPT name, LISPT pars, LISPT body, lisp_type type)
{
  _lisp.check(name, SYMBOL);
  if(!is_NIL(pars) && type_of(pars) != SYMBOL)
    _lisp.check(pars, CONS);
  LISPT foo = mklambda(_lisp, pars, body, type);
  if(type_of(foo) == ERROR)
    return C_NIL;
  checkfn(name, foo);
  name->setopval(foo);
  return cons(_lisp, name, C_NIL);
}

PRIMITIVE user::de(LISPT name, LISPT pars, LISPT body) { return def(name, pars, body, LAMBDA); }

PRIMITIVE user::df(LISPT name, LISPT pars, LISPT body) { return def(name, pars, body, NLAMBDA); }

user::user(lisp& lisp) : base(lisp) {}

void user::init()
{
  alloc::mkprim(PN_DEFINE, ::lisp::define, 2, SUBR);
  alloc::mkprim(PN_GETREP, ::lisp::getrep, 1, SUBR);
  alloc::mkprim(PN_DE, ::lisp::de, -3, FSUBR);
  alloc::mkprim(PN_DF, ::lisp::df, -3, FSUBR);
}

} // namespace lisp
