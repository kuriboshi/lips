/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include "libisp.hh"
#include "alloc.hh"

namespace lisp
{
user::user(): base() {}
user::user(lisp& lisp): base(lisp) {}

LISPT user::getargs(LISPT al)
{
  if(is_NIL(al->cdr()))
    return al->car();
  else
    return cons(l, al->car(), getargs(al->cdr()));
}

PRIMITIVE user::getrep(LISPT fun)
{
  LISPT args;

  if(type_of(fun) != lisp_type::LAMBDA && type_of(fun) != lisp_type::NLAMBDA)
    return C_NIL;
  auto& x = fun->lamval();
  if(x.argcnt == -1)
    args = x.arglist->car();
  else if(x.argcnt < 0)
    args = getargs(x.arglist);
  else
    args = x.arglist;
  if(type_of(fun) == lisp_type::LAMBDA)
    return cons(l, C_LAMBDA, cons(l, args, x.lambdarep));
  return cons(l, C_NLAMBDA, cons(l, args, x.lambdarep));
}

LISPT user::funeq(LISPT f1, LISPT f2)
{
  if(EQ(f1, f2))
    return C_T;
  if(f1->lamval().argcnt == f2->lamval().argcnt)
  {
    LISPT t1 = f1->lamval().arglist;
    LISPT t2 = f2->lamval().arglist;
    LISPT tmp = equal(l, t1, t2);
    if(!is_NIL(tmp))
    {
      t1 = f1->lamval().lambdarep;
      t2 = f2->lamval().lambdarep;
      tmp = equal(l, t1, t2);
      if(!is_NIL(tmp))
        return C_T;
    }
  }
  return C_NIL;
}

LISPT user::checkfn(LISPT name, LISPT lam)
{
  if(type_of(name->symvalue()) != lisp_type::UNBOUND)
    if(type_of(name->symvalue()) == lisp_type::LAMBDA || type_of(name->symvalue()) == lisp_type::NLAMBDA)
    {
      LISPT t = funeq(name->symvalue(), lam);
      if(is_NIL(t))
      {
        putprop(l, name, C_OLDDEF, name->symvalue());
        if(!is_NIL(l.verbose))
          print(l, cons(l, name, cons(l, C_REDEFINED, C_NIL)), C_NIL);
      }
    }
  return C_NIL;
}

PRIMITIVE user::define(LISPT name, LISPT lam)
{
  l.check(name, lisp_type::SYMBOL);
  l.check(lam, lisp_type::LAMBDA, lisp_type::NLAMBDA);
  checkfn(name, lam);
  name->symvalue(lam);
  return name;
}

LISPT user::def(LISPT name, LISPT pars, LISPT body, lisp_type type)
{
  l.check(name, lisp_type::SYMBOL);
  if(!is_NIL(pars) && type_of(pars) != lisp_type::SYMBOL)
    l.check(pars, lisp_type::CONS);
  LISPT foo = mklambda(l, pars, body, type);
  if(type_of(foo) == lisp_type::ERROR)
    return C_NIL;
  checkfn(name, foo);
  name->symvalue(foo);
  return cons(l, name, C_NIL);
}

PRIMITIVE user::de(LISPT name, LISPT pars, LISPT body) { return def(name, pars, body, lisp_type::LAMBDA); }

PRIMITIVE user::df(LISPT name, LISPT pars, LISPT body) { return def(name, pars, body, lisp_type::NLAMBDA); }

inline constexpr auto PN_DEFINE = "define"; // define function
inline constexpr auto PN_GETREP = "getrep"; // get function representation
inline constexpr auto PN_DE = "de";         // defile lambda function
inline constexpr auto PN_DF = "df";         // define nlambda function

void user::init()
{
  // clang-format off
  mkprim(PN_DEFINE, ::lisp::define, subr_t::subr::EVAL,   subr_t::spread::NOSPREAD);
  mkprim(PN_GETREP, ::lisp::getrep, subr_t::subr::EVAL,   subr_t::spread::NOSPREAD);
  mkprim(PN_DE,     ::lisp::de,     subr_t::subr::NOEVAL, subr_t::spread::SPREAD);
  mkprim(PN_DF,     ::lisp::df,     subr_t::subr::NOEVAL, subr_t::spread::SPREAD);
  // clang-format on
}

} // namespace lisp
