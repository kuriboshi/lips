/*
 * Lips, lisp shell.
 * Copyright 1988, Krister Joas
 *
 * $Id$
 */
#include "lisp.h"

#ifndef lint
static char rcsid[] = "$Id$";
#endif

USESAVE

PRIMITIVE map(obj, fn1, fn2)
  LISPT obj, fn1, fn2;
{
  while (TYPEOF(obj) == CONS)
    {
      (void) apply(fn1, cons(obj, C_NIL));
      if (ISNIL(fn2))
        obj = CDR(obj);
      else
        obj = apply(fn2, obj);
    }
  return C_NIL;
}

PRIMITIVE mapc(obj, fn1, fn2)
  LISPT obj, fn1, fn2;
{
  while (TYPEOF(obj) == CONS)
    {
      (void) apply(fn1, cons(CAR(obj), C_NIL));
      if (ISNIL(fn2))
        obj = CDR(obj);
      else
        (void) apply(fn2, cons(obj, C_NIL));
    }
  return C_NIL;
}

PRIMITIVE maplist(obj, fn1, fn2)
  LISPT obj, fn1, fn2;
{
  LISPT rval, tmp;

  tmp = C_NIL;
  if (TYPEOF(obj) == CONS)
    {
      tmp = cons(apply(fn1, cons(obj, C_NIL)), C_NIL);
      obj = CDR(obj);
    }
  rval = tmp;
  SAVE(rval);
  while (TYPEOF(obj) == CONS)
    {
      (void) rplacd(tmp, cons(apply(fn1, cons(obj, C_NIL)), C_NIL));
      tmp = CDR(tmp);
      if (ISNIL(fn2))
        obj = CDR(obj);
      else
        obj = apply(fn2, cons(obj, C_NIL));
    }
  UNSAVE(rval);
  return rval;
}

PRIMITIVE mapcar(obj, fn1, fn2)
  LISPT obj, fn1, fn2;
{
  LISPT rval, tmp; 

  tmp = C_NIL;
  if (TYPEOF(obj) == CONS)
    {
      tmp = cons(apply(fn1, cons(CAR(obj), C_NIL)), C_NIL);
      obj = CDR(obj);
    }
  rval = tmp;
  SAVE(rval);
  while (TYPEOF(obj) == CONS)
    {
      (void) rplacd(tmp, cons(apply(fn1, cons(CAR(obj), C_NIL)), C_NIL));
      tmp = CDR(tmp);
      if (ISNIL(fn2))
        obj = CDR(obj);
      else
        (void) apply(fn2, cons(obj, C_NIL));
    }
  UNSAVE(rval);
  return rval;
}

public void init_map()
{
  mkprim(PN_MAP,     map,      3, SUBR);
  mkprim(PN_MAPC,    mapc,     3, SUBR);
  mkprim(PN_MAPLIST, maplist,  3, SUBR);
  mkprim(PN_MAPCAR,  mapcar,   3, SUBR);
}
