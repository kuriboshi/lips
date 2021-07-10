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

  if(type_of(fun) != type::LAMBDA && type_of(fun) != type::NLAMBDA)
    return NIL;
  auto& x = fun->lamval();
  if(x.argcnt == -1)
    args = x.arglist->car();
  else if(x.argcnt < 0)
    args = getargs(x.arglist);
  else
    args = x.arglist;
  if(type_of(fun) == type::LAMBDA)
    return cons(l, C_LAMBDA, cons(l, args, x.lambdarep));
  return cons(l, C_NLAMBDA, cons(l, args, x.lambdarep));
}

LISPT user::funeq(LISPT f1, LISPT f2)
{
  if(EQ(f1, f2))
    return T;
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
        return T;
    }
  }
  return NIL;
}

LISPT user::checkfn(LISPT name, LISPT lam)
{
  if(type_of(name->symvalue()) != type::UNBOUND)
    if(type_of(name->symvalue()) == type::LAMBDA || type_of(name->symvalue()) == type::NLAMBDA)
    {
      LISPT t = funeq(name->symvalue(), lam);
      if(is_NIL(t))
      {
        putprop(l, name, C_OLDDEF, name->symvalue());
        if(!is_NIL(l.verbose))
          print(l, cons(l, name, cons(l, C_REDEFINED, NIL)), NIL);
      }
    }
  return NIL;
}

PRIMITIVE user::define(LISPT name, LISPT lam)
{
  l.check(name, type::SYMBOL);
  l.check(lam, type::LAMBDA, type::NLAMBDA);
  checkfn(name, lam);
  name->symvalue(lam);
  return name;
}

LISPT user::def(LISPT name, LISPT pars, LISPT body, type type)
{
  l.check(name, type::SYMBOL);
  if(!is_NIL(pars) && type_of(pars) != type::SYMBOL)
    l.check(pars, type::CONS);
  LISPT foo = mklambda(l, pars, body, type);
  if(type_of(foo) == type::ERROR)
    return NIL;
  checkfn(name, foo);
  name->symvalue(foo);
  return cons(l, name, NIL);
}

PRIMITIVE user::de(LISPT name, LISPT pars, LISPT body) { return def(name, pars, body, type::LAMBDA); }

PRIMITIVE user::df(LISPT name, LISPT pars, LISPT body) { return def(name, pars, body, type::NLAMBDA); }

namespace pn
{
inline constexpr auto DEFINE = "define"; // define function
inline constexpr auto GETREP = "getrep"; // get function representation
inline constexpr auto DE = "de";         // defile lambda function
inline constexpr auto DF = "df";         // define nlambda function
} // namespace pn

void user::init()
{
  // clang-format off
  mkprim(pn::DEFINE, ::lisp::define, subr_t::subr::EVAL,   subr_t::spread::NOSPREAD);
  mkprim(pn::GETREP, ::lisp::getrep, subr_t::subr::EVAL,   subr_t::spread::NOSPREAD);
  mkprim(pn::DE,     ::lisp::de,     subr_t::subr::NOEVAL, subr_t::spread::SPREAD);
  mkprim(pn::DF,     ::lisp::df,     subr_t::subr::NOEVAL, subr_t::spread::SPREAD);
  // clang-format on
}

} // namespace lisp
