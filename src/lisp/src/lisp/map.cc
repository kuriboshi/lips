/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include "libisp.hh"

namespace lisp
{
map::map(): base() {}
map::map(lisp& lisp): base(lisp) {}

PRIMITIVE map::xmap(LISPT obj, LISPT fn1, LISPT fn2)
{
  while(type_of(obj) == lisp_type::CONS)
  {
    apply(l, fn1, cons(l, obj, C_NIL));
    if(is_NIL(fn2))
      obj = obj->cdr();
    else
      obj = apply(l, fn2, obj);
  }
  return C_NIL;
}

PRIMITIVE map::mapc(LISPT obj, LISPT fn1, LISPT fn2)
{
  while(type_of(obj) == lisp_type::CONS)
  {
    apply(l, fn1, cons(l, obj->car(), C_NIL));
    if(is_NIL(fn2))
      obj = obj->cdr();
    else
      apply(l, fn2, cons(l, obj, C_NIL));
  }
  return C_NIL;
}

PRIMITIVE map::maplist(LISPT obj, LISPT fn1, LISPT fn2)
{
  LISPT tmp = C_NIL;
  if(type_of(obj) == lisp_type::CONS)
  {
    tmp = cons(l, apply(l, fn1, cons(l, obj, C_NIL)), C_NIL);
    obj = obj->cdr();
  }
  LISPT rval = tmp;
  a.save(rval);
  while(type_of(obj) == lisp_type::CONS)
  {
    rplacd(l, tmp, cons(l, apply(l, fn1, cons(l, obj, C_NIL)), C_NIL));
    tmp = tmp->cdr();
    if(is_NIL(fn2))
      obj = obj->cdr();
    else
      obj = apply(l, fn2, cons(l, obj, C_NIL));
  }
  rval = a.unsave();
  return rval;
}

PRIMITIVE map::mapcar(LISPT obj, LISPT fn1, LISPT fn2)
{
  LISPT tmp = C_NIL;
  if(type_of(obj) == lisp_type::CONS)
  {
    tmp = cons(l, apply(l, fn1, cons(l, obj->car(), C_NIL)), C_NIL);
    obj = obj->cdr();
  }
  LISPT rval = tmp;
  a.save(rval);
  while(type_of(obj) == lisp_type::CONS)
  {
    rplacd(l, tmp, cons(l, apply(l, fn1, cons(l, obj->car(), C_NIL)), C_NIL));
    tmp = tmp->cdr();
    if(is_NIL(fn2))
      obj = obj->cdr();
    else
      apply(l, fn2, cons(l, obj, C_NIL));
  }
  rval = a.unsave();
  return rval;
}

inline constexpr auto PN_MAP = "map";         // map
inline constexpr auto PN_MAPC = "mapc";       // map on car
inline constexpr auto PN_MAPLIST = "maplist"; // map and build result
inline constexpr auto PN_MAPCAR = "mapcar";   // mapc and build result

void map::init()
{
  // clang-format off
  mkprim(PN_MAP,     ::lisp::xmap,    subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_MAPC,    ::lisp::mapc,    subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_MAPLIST, ::lisp::maplist, subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_MAPCAR,  ::lisp::mapcar,  subr_t::S_EVAL, subr_t::S_NOSPREAD);
  // clang-format on
}

} // namespace lisp
