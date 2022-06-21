//
// Lips, lisp shell.
// Copyright 1988, 2020-2022 Krister Joas
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

#include "prop.hh"

namespace lisp::prop
{
LISPT setplist(lisp& l, LISPT x, LISPT pl)
{
  check(x, type::SYMBOL);
  x->symbol().plist = pl;
  return pl;
}

LISPT getplist(lisp& l, LISPT x)
{
  check(x, type::SYMBOL);
  return x->symbol().plist;
}

LISPT putprop(lisp& l, LISPT x, LISPT p, LISPT v)
{
  check(x, type::SYMBOL);
  check(p, type::SYMBOL);
  for(auto pl = x->symbol().plist; !is_NIL(pl); pl = pl->cdr()->cdr())
    if(pl->car() == p)
    {
      rplaca(l, pl->cdr(), v);
      return v;
    }
  x->symbol().plist = cons(l, p, cons(l, v, x->symbol().plist));
  return v;
}

LISPT getprop(lisp& l, LISPT x, LISPT p)
{
  check(x, type::SYMBOL);
  check(p, type::SYMBOL);
  for(auto pl = x->symbol().plist; !is_NIL(pl); pl = pl->cdr()->cdr())
  {
    if(pl->car() == p)
      return pl->cdr()->car();
  }
  return NIL;
}

LISPT remprop(lisp& l, LISPT x, LISPT p)
{
  check(x, type::SYMBOL);
  check(p, type::SYMBOL);
  LISPT r = NIL;
  auto pl = x->symbol().plist;
  LISPT pl2 = NIL;
  for(; !is_NIL(pl); pl = pl->cdr()->cdr())
  {
    if(pl->car() == p)
    {
      r = pl->cdr()->car();
      if(is_NIL(pl2))
        x->symbol().plist = pl->cdr()->cdr();
      else
        rplacd(l, pl2, pl->cdr()->cdr());
    }
    pl2 = pl->cdr();
  }
  return r;
}

namespace pn
{
inline constexpr auto SETPLIST = "setplist"; // set property list
inline constexpr auto GETPLIST = "getplist"; // get property list
inline constexpr auto PUTPROP = "putprop";   // put property on atom
inline constexpr auto GETPROP = "getprop";   // get property value
inline constexpr auto REMPROP = "remprop";   // remove prop
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

} // namespace lisp
