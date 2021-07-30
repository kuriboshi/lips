/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include "user.hh"
#include "alloc.hh"
#include "eval.hh"
#include "file.hh"
#include "io.hh"
#include "iter.hh"
#include "pred.hh"
#include "prop.hh"
#include "prim.hh"

namespace lisp::user
{
LISPT getargs(lisp& l, LISPT al)
{
  if(is_NIL(al->cdr()))
    return al->car();
  else
    return cons(l, al->car(), getargs(l, al->cdr()));
}

LISPT getrep(lisp& l, LISPT fun)
{
  LISPT args;

  if(type_of(fun) != type::LAMBDA && type_of(fun) != type::NLAMBDA)
    return NIL;
  auto& x = fun->lamval();
  if(x.argcnt == -1)
    args = x.arglist->car();
  else if(x.argcnt < 0)
    args = getargs(l, x.arglist);
  else
    args = x.arglist;
  if(type_of(fun) == type::LAMBDA)
    return cons(l, C_LAMBDA, cons(l, args, x.lambdarep));
  return cons(l, C_NLAMBDA, cons(l, args, x.lambdarep));
}

LISPT funeq(lisp& l, LISPT f1, LISPT f2)
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

LISPT checkfn(lisp& l, LISPT name, LISPT lam)
{
  if(type_of(name->symvalue()) != type::UNBOUND)
    if(type_of(name->symvalue()) == type::LAMBDA || type_of(name->symvalue()) == type::NLAMBDA)
    {
      LISPT t = user::funeq(l, name->symvalue(), lam);
      if(is_NIL(t))
      {
        putprop(l, name, C_OLDDEF, name->symvalue());
        if(!is_NIL(l.verbose()))
          print(l, cons(l, name, cons(l, C_REDEFINED, NIL)));
      }
    }
  return NIL;
}

LISPT define(lisp& l, LISPT name, LISPT lam)
{
  check(name, type::SYMBOL);
  check(lam, type::LAMBDA, type::NLAMBDA);
  checkfn(l, name, lam);
  name->symvalue(lam);
  return name;
}

LISPT defineq(lisp& l, LISPT defs)
{
  if(is_NIL(defs))
    return NIL;
  auto result = cons(l, NIL, NIL);
  auto r = result;
  for(auto d: defs)
  {
    auto name = car(l, d);
    auto lam = eval(l, cadr(l, d));
    auto def = cons(l, user::define(l, name, lam), NIL);
    rplacd(r, def);
    r = def;
  }
  return result->cdr();
}

LISPT def(lisp& l, LISPT name, LISPT pars, LISPT body, type type)
{
  check(name, type::SYMBOL);
  if(!is_NIL(pars) && type_of(pars) != type::SYMBOL)
    check(pars, type::CONS);
  LISPT foo = mklambda(l, pars, body, type);
  if(type_of(foo) == type::ERROR)
    return NIL;
  checkfn(l, name, foo);
  name->symvalue(foo);
  return cons(l, name, NIL);
}

LISPT de(lisp& l, LISPT name, LISPT pars, LISPT body) { return def(l, name, pars, body, type::LAMBDA); }

LISPT df(lisp& l, LISPT name, LISPT pars, LISPT body) { return def(l, name, pars, body, type::NLAMBDA); }

namespace pn
{
inline constexpr auto DEFINE = "define";   // define function
inline constexpr auto DEFINEQ = "defineq"; // defineq function
inline constexpr auto GETREP = "getrep";   // get function representation
inline constexpr auto DE = "de";           // defile lambda function
inline constexpr auto DF = "df";           // define nlambda function
} // namespace pn

void init()
{
  // clang-format off
  mkprim(pn::DEFINE,  define,  subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::DEFINEQ, defineq, subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::GETREP,  getrep,  subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::DE,      de,      subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::DF,      df,      subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  // clang-format on
}

} // namespace lisp::user
