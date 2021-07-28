/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include "map.hh"
#include "alloc.hh"
#include "eval.hh"
#include "prim.hh"

namespace lisp
{
Map::Map(): base() {}
Map::Map(lisp& lisp): base(lisp) {}

LISPT Map::map(LISPT obj, LISPT fn1, LISPT fn2)
{
  while(type_of(obj) == type::CONS)
  {
    apply(l, fn1, cons(l, obj, NIL));
    if(is_NIL(fn2))
      obj = obj->cdr();
    else
      obj = apply(l, fn2, obj);
  }
  return NIL;
}

LISPT Map::mapc(LISPT obj, LISPT fn1, LISPT fn2)
{
  while(type_of(obj) == type::CONS)
  {
    apply(l, fn1, cons(l, obj->car(), NIL));
    if(is_NIL(fn2))
      obj = obj->cdr();
    else
      apply(l, fn2, cons(l, obj, NIL));
  }
  return NIL;
}

LISPT Map::maplist(LISPT obj, LISPT fn1, LISPT fn2)
{
  LISPT tmp = NIL;
  if(type_of(obj) == type::CONS)
  {
    tmp = cons(l, apply(l, fn1, cons(l, obj, NIL)), NIL);
    obj = obj->cdr();
  }
  LISPT rval = tmp;
  while(type_of(obj) == type::CONS)
  {
    rplacd(l, tmp, cons(l, apply(l, fn1, cons(l, obj, NIL)), NIL));
    tmp = tmp->cdr();
    if(is_NIL(fn2))
      obj = obj->cdr();
    else
      obj = apply(l, fn2, cons(l, obj, NIL));
  }
  return rval;
}

LISPT Map::mapcar(LISPT obj, LISPT fn1, LISPT fn2)
{
  LISPT tmp = NIL;
  if(type_of(obj) == type::CONS)
  {
    tmp = cons(l, apply(l, fn1, cons(l, obj->car(), NIL)), NIL);
    obj = obj->cdr();
  }
  LISPT rval = tmp;
  while(type_of(obj) == type::CONS)
  {
    rplacd(l, tmp, cons(l, apply(l, fn1, cons(l, obj->car(), NIL)), NIL));
    tmp = tmp->cdr();
    if(is_NIL(fn2))
      obj = obj->cdr();
    else
      apply(l, fn2, cons(l, obj, NIL));
  }
  return rval;
}

namespace pn
{
inline constexpr auto MAP = "map";         // map
inline constexpr auto MAPC = "mapc";       // map on car
inline constexpr auto MAPLIST = "maplist"; // map and build result
inline constexpr auto MAPCAR = "mapcar";   // mapc and build result
} // namespace pn

void Map::init()
{
  // clang-format off
  mkprim(pn::MAP,     ::lisp::map,     subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::MAPC,    ::lisp::mapc,    subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::MAPLIST, ::lisp::maplist, subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::MAPCAR,  ::lisp::mapcar,  subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  // clang-format on
}

} // namespace lisp
