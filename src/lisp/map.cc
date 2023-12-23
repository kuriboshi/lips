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
#include "map.hh"
#include "prim.hh"
#include "vm.hh"

namespace lisp::details::map
{

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
