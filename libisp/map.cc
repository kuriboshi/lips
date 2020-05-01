/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include "libisp.hh"

namespace lisp
{
PRIMITIVE map::xmap(LISPT obj, LISPT fn1, LISPT fn2)
{
  while(type_of(obj) == CONS)
  {
    apply(_lisp, fn1, cons(_lisp, obj, C_NIL));
    if(is_NIL(fn2))
      obj = obj->cdr();
    else
      obj = apply(_lisp, fn2, obj);
  }
  return C_NIL;
}

PRIMITIVE map::mapc(LISPT obj, LISPT fn1, LISPT fn2)
{
  while(type_of(obj) == CONS)
  {
    apply(_lisp, fn1, cons(_lisp, obj->car(), C_NIL));
    if(is_NIL(fn2))
      obj = obj->cdr();
    else
      apply(_lisp, fn2, cons(_lisp, obj, C_NIL));
  }
  return C_NIL;
}

PRIMITIVE map::maplist(LISPT obj, LISPT fn1, LISPT fn2)
{
  LISPT tmp = C_NIL;
  if(type_of(obj) == CONS)
  {
    tmp = cons(_lisp, apply(_lisp, fn1, cons(_lisp, obj, C_NIL)), C_NIL);
    obj = obj->cdr();
  }
  LISPT rval = tmp;
  a().save(rval);
  while(type_of(obj) == CONS)
  {
    rplacd(_lisp, tmp, cons(_lisp, apply(_lisp, fn1, cons(_lisp, obj, C_NIL)), C_NIL));
    tmp = tmp->cdr();
    if(is_NIL(fn2))
      obj = obj->cdr();
    else
      obj = apply(_lisp, fn2, cons(_lisp, obj, C_NIL));
  }
  rval = a().unsave();
  return rval;
}

PRIMITIVE map::mapcar(LISPT obj, LISPT fn1, LISPT fn2)
{
  LISPT tmp = C_NIL;
  if(type_of(obj) == CONS)
  {
    tmp = cons(_lisp, apply(_lisp, fn1, cons(_lisp, obj->car(), C_NIL)), C_NIL);
    obj = obj->cdr();
  }
  LISPT rval = tmp;
  a().save(rval);
  while(type_of(obj) == CONS)
  {
    rplacd(_lisp, tmp, cons(_lisp, apply(_lisp, fn1, cons(_lisp, obj->car(), C_NIL)), C_NIL));
    tmp = tmp->cdr();
    if(is_NIL(fn2))
      obj = obj->cdr();
    else
      apply(_lisp, fn2, cons(_lisp, obj, C_NIL));
  }
  rval = a().unsave();
  return rval;
}

map::map(lisp& lisp) : base(lisp)
{
  mkprim(PN_MAP, ::lisp::xmap, 3, SUBR);
  mkprim(PN_MAPC, ::lisp::mapc, 3, SUBR);
  mkprim(PN_MAPLIST, ::lisp::maplist, 3, SUBR);
  mkprim(PN_MAPCAR, ::lisp::mapcar, 3, SUBR);
}

} // namespace lisp
