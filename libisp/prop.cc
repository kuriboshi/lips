/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 * $Id$
 */

#include "libisp.hh"

namespace lisp
{
PRIMITIVE setplist(LISPT a, LISPT pl)
{
  check(a, SYMBOL);
  a->symval().plist = pl;
  return pl;
}

PRIMITIVE getplist(LISPT a)
{
  check(a, SYMBOL);
  return a->symval().plist;
}

PRIMITIVE putprop(LISPT a, LISPT p, LISPT v)
{
  check(a, SYMBOL);
  check(p, SYMBOL);
  for(auto pl = a->symval().plist; !ISNIL(pl); pl = pl->cdr()->cdr())
    if(EQ(pl->car(), p))
    {
      rplaca(pl->cdr(), v);
      return v;
    }
  a->symval().plist = cons(p, cons(v, a->symval().plist));
  return v;
}

PRIMITIVE getprop(LISPT a, LISPT p)
{
  check(a, SYMBOL);
  check(p, SYMBOL);
  for(auto pl = a->symval().plist; !ISNIL(pl); pl = pl->cdr()->cdr())
  {
    if(EQ(pl->car(), p))
      return pl->cdr()->car();
  }
  return C_NIL;
}

PRIMITIVE remprop(LISPT a, LISPT p)
{
  LISPT pl, pl2;

  check(a, SYMBOL);
  check(p, SYMBOL);
  LISPT r = C_NIL;
  for(pl = a->symval().plist, pl2 = C_NIL; !ISNIL(pl); pl = pl->cdr()->cdr())
  {
    if(EQ(pl->car(), p))
    {
      r = pl->cdr()->car();
      if(ISNIL(pl2))
        a->symval().plist = pl->cdr()->cdr();
      else
        rplacd(pl2, pl->cdr()->cdr());
    }
    pl2 = pl->cdr();
  }
  return r;
}

void init_prop()
{
  mkprim(PN_SETPLIST, setplist, 2, SUBR);
  mkprim(PN_GETPLIST, getplist, 1, SUBR);
  mkprim(PN_PUTPROP, putprop, 3, SUBR);
  mkprim(PN_GETPROP, getprop, 2, SUBR);
  mkprim(PN_REMPROP, remprop, 2, SUBR);
}

} // namespace lisp
