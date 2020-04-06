/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 * $Id$
 */
#include "lisp.h"

#ifndef lint
static char rcsid[] = "$Id$";
#endif

PRIMITIVE setplist(a, pl)
  LISPT a, pl;
{
  CHECK(a, SYMBOL);
  SYMVAL(a).plist = pl;
  return pl;
}

PRIMITIVE getplist(a)
  LISPT a;
{
  CHECK(a, SYMBOL);
  return SYMVAL(a).plist;
}

PRIMITIVE putprop(a, p, v)
  LISPT a, p, v;
{
  LISPT pl;

  CHECK(a, SYMBOL);
  CHECK(p, SYMBOL);
  for (pl = SYMVAL(a).plist; !ISNIL(pl); pl = CDR(CDR(pl)))
    if(EQ(CAR(pl), p))
      {
        (void)rplaca(CDR(pl), v);
        return v;
      }
  SYMVAL(a).plist = cons(p, cons(v, SYMVAL(a).plist));
  return v;
}

PRIMITIVE getprop(a, p)
  LISPT a, p;
{
  LISPT pl;

  CHECK(a, SYMBOL);
  CHECK(p, SYMBOL);
  for (pl = SYMVAL(a).plist; !ISNIL(pl); pl = CDR(CDR(pl)))
    {
      if (EQ(CAR(pl), p))
        return CAR(CDR(pl));
    }
  return C_NIL;
}

PRIMITIVE remprop(a, p)
  LISPT a, p;
{
  LISPT pl, pl2, r;

  CHECK(a, SYMBOL);
  CHECK(p, SYMBOL);
  r = C_NIL;
  for (pl = SYMVAL(a).plist, pl2 = C_NIL; !ISNIL(pl); pl = CDR(CDR(pl)))
    {
      if (EQ(CAR(pl), p))
        {
          r = CAR(CDR(pl));
          if (ISNIL(pl2))
            SYMVAL(a).plist = CDR(CDR(pl));
          else
            (void) rplacd(pl2, CDR(CDR(pl)));
        }
      pl2 = CDR(pl);
    }
  return r;
}

void init_prop()
{
  mkprim(PN_SETPLIST, setplist, 2, SUBR);
  mkprim(PN_GETPLIST, getplist, 1, SUBR);
  mkprim(PN_PUTPROP,  putprop,  3, SUBR);
  mkprim(PN_GETPROP,  getprop,  2, SUBR);
  mkprim(PN_REMPROP,  remprop,  2, SUBR);
}
