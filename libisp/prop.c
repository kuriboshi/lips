/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 * $Id$
 */
#include "lisp.h"

PRIMITIVE setplist(LISPT a, LISPT pl)
{
  CHECK(a, SYMBOL);
  SYMVAL(a).plist = pl;
  return pl;
}

PRIMITIVE getplist(LISPT a)
{
  CHECK(a, SYMBOL);
  return SYMVAL(a).plist;
}

PRIMITIVE putprop(LISPT a, LISPT p, LISPT v)
{
  LISPT pl;

  CHECK(a, SYMBOL);
  CHECK(p, SYMBOL);
  for (pl = SYMVAL(a).plist; !ISNIL(pl); pl = CDR(CDR(pl)))
    if (EQ(CAR(pl), p))
      {
        rplaca(CDR(pl), v);
        return v;
      }
  SYMVAL(a).plist = cons(p, cons(v, SYMVAL(a).plist));
  return v;
}

PRIMITIVE getprop(LISPT a, LISPT p)
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

PRIMITIVE remprop(LISPT a, LISPT p)
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
            rplacd(pl2, CDR(CDR(pl)));
        }
      pl2 = CDR(pl);
    }
  return r;
}

void init_prop()
{
  mkprim2(PN_SETPLIST, setplist, 2, SUBR);
  mkprim1(PN_GETPLIST, getplist, 1, SUBR);
  mkprim3(PN_PUTPROP, putprop, 3, SUBR);
  mkprim2(PN_GETPROP, getprop, 2, SUBR);
  mkprim2(PN_REMPROP, remprop, 2, SUBR);
}
