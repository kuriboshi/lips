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
#include "atoms.hh"
#include "check.hh"
#include "iter.hh"
#include "file.hh"
#include "predicate.hh"
#include "list.hh"
#include "property.hh"
#include "user.hh"

namespace lisp::details::user
{
using lisp::vm;

inline lisp_t getargs(const lisp_t& al)
{
  if(is_nil(al->cdr()))
    return al->car();
  return cons(al->car(), getargs(al->cdr()));
}

lisp_t getrep(const lisp_t& fun)
{
  lisp_t args;

  if(type_of(fun) != object::type::Lambda)
    return nil;
  const auto& x = fun->lambda();
  if(x.count == -1)
    args = x.args->car();
  else if(x.count < 0)
    args = getargs(x.args);
  else
    args = x.args;
  if(x.eval)
    return cons(atoms::LAMBDA, cons(args, x.body));
  return cons(atoms::NLAMBDA, cons(args, x.body));
}

lisp_t funeq(const lisp_t& f1, const lisp_t& f2)
{
  if(f1 == f2)
    return T;
  if(f1->lambda().count == f2->lambda().count)
  {
    lisp_t t1 = f1->lambda().args;
    lisp_t t2 = f2->lambda().args;
    lisp_t tmp = equal(t1, t2);
    if(!is_nil(tmp))
    {
      t1 = f1->lambda().body;
      t2 = f2->lambda().body;
      tmp = equal(t1, t2);
      if(!is_nil(tmp))
        return T;
    }
  }
  return nil;
}

inline lisp_t checkfn(const lisp_t& name, const lisp_t& lam)
{
  if(name->value() != atoms::UNBOUND)
    if(type_of(name->value()) == object::type::Lambda)
    {
      const lisp_t t = user::funeq(name->value(), lam);
      if(is_nil(t))
      {
        putprop(name, atoms::OLDDEF, name->value());
        if(!is_nil(vm::verbose()))
          print(cons(name, cons(atoms::REDEFINED, nil)));
      }
    }
  return nil;
}

lisp_t define(lisp_t name, const lisp_t& lam)
{
  check(name, object::type::Symbol);
  check(lam, object::type::Lambda);
  checkfn(name, lam);
  name->value(lam);
  return name;
}

lisp_t defineq(const lisp_t& defs)
{
  if(is_nil(defs))
    return nil;
  auto result = cons(nil, nil);
  auto r = result;
  for(auto d: defs)
  {
    auto name = car(d);
    auto lam = eval(cadr(d));
    auto def = cons(user::define(name, lam), nil);
    rplacd(r, def);
    r = def;
  }
  return result->cdr();
}

namespace pn
{
inline constexpr std::string_view DEFINE = "define";   // define function
inline constexpr std::string_view DEFINEQ = "defineq"; // defineq function
inline constexpr std::string_view GETREP = "getrep";   // get function representation
} // namespace pn

void init()
{
  // clang-format off
  mkprim(pn::DEFINE,  define,  subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::DEFINEQ, defineq, subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::GETREP,  getrep,  subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  // clang-format on
}

} // namespace lisp::details::user
