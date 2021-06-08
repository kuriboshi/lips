/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include "libisp.hh"

namespace lisp
{
prop::prop(): base() {}
prop::prop(lisp& lisp): base(lisp) {}

PRIMITIVE prop::setplist(LISPT x, LISPT pl)
{
  l.check(x, lisp_type::SYMBOL);
  x->symval().plist = pl;
  return pl;
}

PRIMITIVE prop::getplist(LISPT x)
{
  l.check(x, lisp_type::SYMBOL);
  return x->symval().plist;
}

PRIMITIVE prop::putprop(LISPT x, LISPT p, LISPT v)
{
  l.check(x, lisp_type::SYMBOL);
  l.check(p, lisp_type::SYMBOL);
  for(auto pl = x->symval().plist; !is_NIL(pl); pl = pl->cdr()->cdr())
    if(EQ(pl->car(), p))
    {
      rplaca(l, pl->cdr(), v);
      return v;
    }
  x->symval().plist = cons(l, p, cons(l, v, x->symval().plist));
  return v;
}

PRIMITIVE prop::getprop(LISPT x, LISPT p)
{
  l.check(x, lisp_type::SYMBOL);
  l.check(p, lisp_type::SYMBOL);
  for(auto pl = x->symval().plist; !is_NIL(pl); pl = pl->cdr()->cdr())
  {
    if(EQ(pl->car(), p))
      return pl->cdr()->car();
  }
  return C_NIL;
}

PRIMITIVE prop::remprop(LISPT x, LISPT p)
{
  LISPT pl, pl2;

  l.check(x, lisp_type::SYMBOL);
  l.check(p, lisp_type::SYMBOL);
  LISPT r = C_NIL;
  for(pl = x->symval().plist, pl2 = C_NIL; !is_NIL(pl); pl = pl->cdr()->cdr())
  {
    if(EQ(pl->car(), p))
    {
      r = pl->cdr()->car();
      if(is_NIL(pl2))
        x->symval().plist = pl->cdr()->cdr();
      else
        rplacd(l, pl2, pl->cdr()->cdr());
    }
    pl2 = pl->cdr();
  }
  return r;
}

inline constexpr auto PN_SETPLIST = "setplist"; // set property list
inline constexpr auto PN_GETPLIST = "getplist"; // get property list
inline constexpr auto PN_PUTPROP = "putprop";   // put property on atom
inline constexpr auto PN_GETPROP = "getprop";   // get property value
inline constexpr auto PN_REMPROP = "remprop";   // remove prop

void prop::init()
{
  // clang-format off
  mkprim(PN_SETPLIST, ::lisp::setplist, subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_GETPLIST, ::lisp::getplist, subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_PUTPROP,  ::lisp::putprop,  subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_GETPROP,  ::lisp::getprop,  subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_REMPROP,  ::lisp::remprop,  subr_t::S_EVAL, subr_t::S_NOSPREAD);
  // clang-format on
}

} // namespace lisp
