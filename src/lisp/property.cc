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
#include "check.hh"
#include "list.hh"
#include "property.hh"

namespace lisp::details::property
{
lisp_t setplist(const lisp_t& x, const lisp_t& pl)
{
  check(x, object::type::Symbol);
  x->as_symbol()->property_list(pl);
  return pl;
}

lisp_t getplist(const lisp_t& x)
{
  check(x, object::type::Symbol);
  return x->as_symbol()->property_list();
}

lisp_t putprop(const lisp_t& x, const lisp_t& p, const lisp_t& v)
{
  check(x, object::type::Symbol);
  check(p, object::type::Symbol);
  for(auto pl = x->as_symbol()->property_list(); !is_nil(pl); pl = pl->cdr()->cdr())
    if(pl->car() == p)
    {
      rplaca(pl->cdr(), v);
      return v;
    }
  x->as_symbol()->property_list(cons(p, cons(v, x->as_symbol()->property_list())));
  return v;
}

lisp_t getprop(const lisp_t& x, const lisp_t& p)
{
  check(x, object::type::Symbol);
  check(p, object::type::Symbol);
  for(auto pl = x->as_symbol()->property_list(); !is_nil(pl); pl = pl->cdr()->cdr())
  {
    if(pl->car() == p)
      return pl->cdr()->car();
  }
  return nil;
}

lisp_t remprop(const lisp_t& x, const lisp_t& p)
{
  check(x, object::type::Symbol);
  check(p, object::type::Symbol);
  lisp_t r = nil;
  auto pl = x->as_symbol()->property_list();
  lisp_t pl2 = nil;
  for(; !is_nil(pl); pl = pl->cdr()->cdr())
  {
    if(pl->car() == p)
    {
      r = pl->cdr()->car();
      if(is_nil(pl2))
        x->as_symbol()->property_list(pl->cdr()->cdr());
      else
        rplacd(pl2, pl->cdr()->cdr());
    }
    pl2 = pl->cdr();
  }
  return r;
}

namespace pn
{
inline constexpr std::string_view SETPLIST = "setplist"; // set property list
inline constexpr std::string_view GETPLIST = "getplist"; // get property list
inline constexpr std::string_view PUTPROP = "putprop";   // put property on atom
inline constexpr std::string_view GETPROP = "getprop";   // get property value
inline constexpr std::string_view REMPROP = "remprop";   // remove prop
} // namespace pn

void init()
{
  // clang-format off
  mkprim(pn::SETPLIST, setplist, subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::GETPLIST, getplist, subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::PUTPROP,  putprop,  subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::GETPROP,  getprop,  subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::REMPROP,  remprop,  subr_t::subr::EVAL, subr_t::spread::SPREAD);
  // clang-format on
}

} // namespace lisp::details::property
