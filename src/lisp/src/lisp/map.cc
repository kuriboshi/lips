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
  while(type_of(obj) == type::CONS)
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
  while(type_of(obj) == type::CONS)
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
  if(type_of(obj) == type::CONS)
  {
    tmp = cons(l, apply(l, fn1, cons(l, obj, C_NIL)), C_NIL);
    obj = obj->cdr();
  }
  LISPT rval = tmp;
  a.save(rval);
  while(type_of(obj) == type::CONS)
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
  if(type_of(obj) == type::CONS)
  {
    tmp = cons(l, apply(l, fn1, cons(l, obj->car(), C_NIL)), C_NIL);
    obj = obj->cdr();
  }
  LISPT rval = tmp;
  a.save(rval);
  while(type_of(obj) == type::CONS)
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

namespace pn
{
inline constexpr auto MAP = "map";         // map
inline constexpr auto MAPC = "mapc";       // map on car
inline constexpr auto MAPLIST = "maplist"; // map and build result
inline constexpr auto MAPCAR = "mapcar";   // mapc and build result
} // namespace pn

void map::init()
{
  // clang-format off
  mkprim(pn::MAP,     ::lisp::xmap,    subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::MAPC,    ::lisp::mapc,    subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::MAPLIST, ::lisp::maplist, subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::MAPCAR,  ::lisp::mapcar,  subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  // clang-format on
}

} // namespace lisp
