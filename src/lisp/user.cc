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

#include "user.hh"

namespace lisp::user
{
LISPT getargs(lisp& l, LISPT al)
{
  if(is_NIL(al->cdr()))
    return al->car();
  return cons(l, al->car(), getargs(l, al->cdr()));
}

LISPT getrep(lisp& l, LISPT fun)
{
  LISPT args;

  if(type_of(fun) != type::LAMBDA && type_of(fun) != type::NLAMBDA)
    return NIL;
  auto& x = fun->lambda();
  if(x.count == -1)
    args = x.args->car();
  else if(x.count < 0)
    args = getargs(l, x.args);
  else
    args = x.args;
  if(type_of(fun) == type::LAMBDA)
    return cons(l, C_LAMBDA, cons(l, args, x.body));
  return cons(l, C_NLAMBDA, cons(l, args, x.body));
}

LISPT funeq(lisp& l, LISPT f1, LISPT f2)
{
  if(f1 == f2)
    return T;
  if(f1->lambda().count == f2->lambda().count)
  {
    LISPT t1 = f1->lambda().args;
    LISPT t2 = f2->lambda().args;
    LISPT tmp = equal(l, t1, t2);
    if(!is_NIL(tmp))
    {
      t1 = f1->lambda().body;
      t2 = f2->lambda().body;
      tmp = equal(l, t1, t2);
      if(!is_NIL(tmp))
        return T;
    }
  }
  return NIL;
}

LISPT checkfn(lisp& l, LISPT name, LISPT lam)
{
  if(type_of(name->value()) != type::UNBOUND)
    if(type_of(name->value()) == type::LAMBDA || type_of(name->value()) == type::NLAMBDA)
    {
      LISPT t = user::funeq(l, name->value(), lam);
      if(is_NIL(t))
      {
        putprop(l, name, C_OLDDEF, name->value());
        if(!is_NIL(l.verbose()))
          print(l, cons(l, name, cons(l, C_REDEFINED, NIL)));
      }
    }
  return NIL;
}

LISPT define(lisp& l, LISPT name, LISPT lam)
{
  check(name, type::SYMBOL);
  check(lam, type::LAMBDA, type::NLAMBDA);
  checkfn(l, name, lam);
  name->value(lam);
  return name;
}

LISPT defineq(lisp& l, LISPT defs)
{
  if(is_NIL(defs))
    return NIL;
  auto result = cons(l, NIL, NIL);
  auto r = result;
  for(auto d: defs)
  {
    auto name = car(l, d);
    auto lam = eval(l, cadr(l, d));
    auto def = cons(l, user::define(l, name, lam), NIL);
    rplacd(r, def);
    r = def;
  }
  return result->cdr();
}

namespace pn
{
inline constexpr auto DEFINE = "define";   // define function
inline constexpr auto DEFINEQ = "defineq"; // defineq function
inline constexpr auto GETREP = "getrep";   // get function representation
} // namespace pn

void init()
{
  // clang-format off
  mkprim(pn::DEFINE,  define,  subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::DEFINEQ, defineq, subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::GETREP,  getrep,  subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  // clang-format on
}

} // namespace lisp::user
