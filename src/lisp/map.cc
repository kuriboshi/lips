//
// Lips, lisp shell.
// Copyright 1988, 2020-2024 Krister Joas
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
#include "list.hh"
#include "vm.hh"

namespace lisp::details::map
{

lisp_t map(const lisp_t& obj, const lisp_t& fn1, const lisp_t& fn2)
{
  auto copy = obj;
  while(type_of(copy) == object::type::Cons)
  {
    apply(fn1, cons(copy, nil));
    if(is_nil(fn2))
      copy = copy->cdr();
    else
      copy = apply(fn2, cons(copy, nil));
  }
  return nil;
}

lisp_t mapc(const lisp_t& obj, const lisp_t& fn1, const lisp_t& fn2)
{
  auto copy = obj;
  while(type_of(copy) == object::type::Cons)
  {
    apply(fn1, cons(copy->car(), nil));
    if(is_nil(fn2))
      copy = copy->cdr();
    else
      copy = apply(fn2, cons(copy, nil));
  }
  return nil;
}

lisp_t maplist(const lisp_t& obj, const lisp_t& fn1, const lisp_t& fn2)
{
  lisp_t tmp = nil;
  auto copy = obj;
  if(type_of(copy) == object::type::Cons)
  {
    tmp = cons(apply(fn1, cons(copy, nil)), nil);
    copy = copy->cdr();
  }
  lisp_t rval = tmp;
  while(type_of(copy) == object::type::Cons)
  {
    rplacd(tmp, cons(apply(fn1, cons(copy, nil)), nil));
    tmp = tmp->cdr();
    if(is_nil(fn2))
      copy = copy->cdr();
    else
      copy = apply(fn2, cons(copy, nil));
  }
  return rval;
}

lisp_t mapcar(const lisp_t& obj, const lisp_t& fn1, const lisp_t& fn2)
{
  lisp_t tmp = nil;
  auto copy = obj;
  if(type_of(copy) == object::type::Cons)
  {
    tmp = cons(apply(fn1, cons(copy->car(), nil)), nil);
    copy = copy->cdr();
  }
  lisp_t rval = tmp;
  while(type_of(copy) == object::type::Cons)
  {
    rplacd(tmp, cons(apply(fn1, cons(copy->car(), nil)), nil));
    tmp = tmp->cdr();
    if(is_nil(fn2))
      copy = copy->cdr();
    else
      copy = apply(fn2, cons(copy, nil));
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
