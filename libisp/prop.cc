/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include "libisp.hh"

namespace lisp
{
PRIMITIVE prop::setplist(LISPT a, LISPT pl)
{
  check(a, SYMBOL);
  a->symval().plist = pl;
  return pl;
}

PRIMITIVE prop::getplist(LISPT a)
{
  check(a, SYMBOL);
  return a->symval().plist;
}

PRIMITIVE prop::putprop(LISPT a, LISPT p, LISPT v)
{
  check(a, SYMBOL);
  check(p, SYMBOL);
  for(auto pl = a->symval().plist; !is_NIL(pl); pl = pl->cdr()->cdr())
    if(EQ(pl->car(), p))
    {
      rplaca(_lisp, pl->cdr(), v);
      return v;
    }
  a->symval().plist = cons(_lisp, p, cons(_lisp, v, a->symval().plist));
  return v;
}

PRIMITIVE prop::getprop(LISPT a, LISPT p)
{
  check(a, SYMBOL);
  check(p, SYMBOL);
  for(auto pl = a->symval().plist; !is_NIL(pl); pl = pl->cdr()->cdr())
  {
    if(EQ(pl->car(), p))
      return pl->cdr()->car();
  }
  return C_NIL;
}

PRIMITIVE prop::remprop(LISPT a, LISPT p)
{
  LISPT pl, pl2;

  check(a, SYMBOL);
  check(p, SYMBOL);
  LISPT r = C_NIL;
  for(pl = a->symval().plist, pl2 = C_NIL; !is_NIL(pl); pl = pl->cdr()->cdr())
  {
    if(EQ(pl->car(), p))
    {
      r = pl->cdr()->car();
      if(is_NIL(pl2))
        a->symval().plist = pl->cdr()->cdr();
      else
        rplacd(_lisp, pl2, pl->cdr()->cdr());
    }
    pl2 = pl->cdr();
  }
  return r;
}

prop::prop(lisp& lisp) : base(lisp) {}

void prop::init()
{
  alloc::mkprim(PN_SETPLIST, ::lisp::setplist, 2, SUBR);
  alloc::mkprim(PN_GETPLIST, ::lisp::getplist, 1, SUBR);
  alloc::mkprim(PN_PUTPROP, ::lisp::putprop, 3, SUBR);
  alloc::mkprim(PN_GETPROP, ::lisp::getprop, 2, SUBR);
  alloc::mkprim(PN_REMPROP, ::lisp::remprop, 2, SUBR);
}

} // namespace lisp
