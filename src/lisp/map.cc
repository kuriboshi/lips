//
// Lips, lisp shell.
// Copyright 1988, 2020-2023 Krister Joas
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//

#include "alloc.hh"
#include "eval.hh"
#include "map.hh"
#include "prim.hh"

namespace lisp::details::map
{

/// @brief Apply function FN1 on each tail of OBJ.
///
/// @param l The lisp interpreter.
/// @param obj A list of items.
/// @param fn1 The function to apply on each tail of obj.
/// @param fn2 Function to apply to get the next element of the list (default is CDR).
///
/// @returns nil
lisp_t map(lisp_t obj, lisp_t fn1, lisp_t fn2)
{
  while(type_of(obj) == object::type::Cons)
  {
    apply(fn1, cons(obj, nil));
    if(is_nil(fn2))
      obj = obj->cdr();
    else
      obj = apply(fn2, cons(obj, nil));
  }
  return nil;
}

/// @brief Apply function on each CAR of a list.
///
/// @param l The lisp interpreter.
/// @param obj A list of items. If not a list the function is a no-op.
/// @param fn1 The function to apply on each CAR of the list.
/// @param fn2 Function to apply to get the next element (default is CDR).
///
/// @returns nil
lisp_t mapc(lisp_t obj, lisp_t fn1, lisp_t fn2)
{
  while(type_of(obj) == object::type::Cons)
  {
    apply(fn1, cons(obj->car(), nil));
    if(is_nil(fn2))
      obj = obj->cdr();
    else
      obj = apply(fn2, cons(obj, nil));
  }
  return nil;
}

/// @brief Apply FN1 on each consecutive tail of OBJ.
///
/// @param l The lisp interpreter.
/// @param obj A list of items.
/// @param fn1 The function to apply on each tail of the list.
/// @param fn2 Function to apply to get the next element (default is CDR).
///
/// @returns A list of the result of applying FN1 on each element in the list.
lisp_t maplist(lisp_t obj, lisp_t fn1, lisp_t fn2)
{
  lisp_t tmp = nil;
  if(type_of(obj) == object::type::Cons)
  {
    tmp = cons(apply(fn1, cons(obj, nil)), nil);
    obj = obj->cdr();
  }
  lisp_t rval = tmp;
  while(type_of(obj) == object::type::Cons)
  {
    rplacd(tmp, cons(apply(fn1, cons(obj, nil)), nil));
    tmp = tmp->cdr();
    if(is_nil(fn2))
      obj = obj->cdr();
    else
      obj = apply(fn2, cons(obj, nil));
  }
  return rval;
}

/// @brief Apply FN1 on each element of OBJ.
///
/// @param l The lisp interpreter.
/// @param obj A list of items.
/// @param fn1 The function to apply on each element of the list.
/// @param fn2 Function to apply to get the next element (default is CDR).
///
/// @returns A list of the result of applying FN1 on each element in the list.
lisp_t mapcar(lisp_t obj, lisp_t fn1, lisp_t fn2)
{
  lisp_t tmp = nil;
  if(type_of(obj) == object::type::Cons)
  {
    tmp = cons(apply(fn1, cons(obj->car(), nil)), nil);
    obj = obj->cdr();
  }
  lisp_t rval = tmp;
  while(type_of(obj) == object::type::Cons)
  {
    rplacd(tmp, cons(apply(fn1, cons(obj->car(), nil)), nil));
    tmp = tmp->cdr();
    if(is_nil(fn2))
      obj = obj->cdr();
    else
      obj = apply(fn2, cons(obj, nil));
  }
  return rval;
}

namespace pn
{
inline constexpr std::string_view MAP = "map";         // map
inline constexpr std::string_view MAPC = "mapc";       // map on car
inline constexpr std::string_view MAPLIST = "maplist"; // map and build result
inline constexpr std::string_view MAPCAR = "mapcar";   // mapc and build result
} // namespace pn

void init()
{
  // clang-format off
  mkprim(pn::MAP,     map,     subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::MAPC,    mapc,    subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::MAPLIST, maplist, subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::MAPCAR,  mapcar,  subr_t::subr::EVAL, subr_t::spread::SPREAD);
  // clang-format on
}

} // namespace lisp::details::map
