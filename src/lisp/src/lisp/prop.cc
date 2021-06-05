/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include "libisp.hh"

namespace lisp
{
PRIMITIVE prop::setplist(LISPT x, LISPT pl)
{
  l.check(x, SYMBOL);
  x->symval().plist = pl;
  return pl;
}

PRIMITIVE prop::getplist(LISPT x)
{
  l.check(x, SYMBOL);
  return x->symval().plist;
}

PRIMITIVE prop::putprop(LISPT x, LISPT p, LISPT v)
{
  l.check(x, SYMBOL);
  l.check(p, SYMBOL);
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
  l.check(x, SYMBOL);
  l.check(p, SYMBOL);
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

  l.check(x, SYMBOL);
  l.check(p, SYMBOL);
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

prop::prop(lisp& lisp): base(lisp) {}

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