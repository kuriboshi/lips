/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 * $Id$
 */

#include "libisp.hh"

using namespace lisp;

PRIMITIVE map(LISPT obj, LISPT fn1, LISPT fn2)
{
  while(TYPEOF(obj) == CONS)
  {
    apply(fn1, cons(obj, C_NIL));
    if(ISNIL(fn2))
      obj = CDR(obj);
    else
      obj = apply(fn2, obj);
  }
  return C_NIL;
}

PRIMITIVE mapc(LISPT obj, LISPT fn1, LISPT fn2)
{
  while(TYPEOF(obj) == CONS)
  {
    apply(fn1, cons(CAR(obj), C_NIL));
    if(ISNIL(fn2))
      obj = CDR(obj);
    else
      apply(fn2, cons(obj, C_NIL));
  }
  return C_NIL;
}

PRIMITIVE maplist(LISPT obj, LISPT fn1, LISPT fn2)
{
  LISPT tmp = C_NIL;
  if(TYPEOF(obj) == CONS)
  {
    tmp = cons(apply(fn1, cons(obj, C_NIL)), C_NIL);
    obj = CDR(obj);
  }
  LISPT rval = tmp;
  SAVE(rval);
  while(TYPEOF(obj) == CONS)
  {
    rplacd(tmp, cons(apply(fn1, cons(obj, C_NIL)), C_NIL));
    tmp = CDR(tmp);
    if(ISNIL(fn2))
      obj = CDR(obj);
    else
      obj = apply(fn2, cons(obj, C_NIL));
  }
  UNSAVE(rval);
  return rval;
}

PRIMITIVE mapcar(LISPT obj, LISPT fn1, LISPT fn2)
{
  LISPT tmp = C_NIL;
  if(TYPEOF(obj) == CONS)
  {
    tmp = cons(apply(fn1, cons(CAR(obj), C_NIL)), C_NIL);
    obj = CDR(obj);
  }
  LISPT rval = tmp;
  SAVE(rval);
  while(TYPEOF(obj) == CONS)
  {
    rplacd(tmp, cons(apply(fn1, cons(CAR(obj), C_NIL)), C_NIL));
    tmp = CDR(tmp);
    if(ISNIL(fn2))
      obj = CDR(obj);
    else
      apply(fn2, cons(obj, C_NIL));
  }
  UNSAVE(rval);
  return rval;
}

void init_map()
{
  mkprim(PN_MAP, map, 3, SUBR);
  mkprim(PN_MAPC, mapc, 3, SUBR);
  mkprim(PN_MAPLIST, maplist, 3, SUBR);
  mkprim(PN_MAPCAR, mapcar, 3, SUBR);
}
