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
#include "iter.hh"

namespace lisp
{
inline lisp_t nth(const lisp_t& list, integer_t::value_type n)
{
  lisp_t ls;
  for(ls = list; n > 1 && !is_nil(ls); n--, ls = ls->cdr())
    ;
  return ls;
}
} // namespace lisp

namespace lisp::details::list
{
lisp_t car(const lisp_t& a)
{
  if(type_of(a) == object::type::Cons)
    return a->car();
  return nil;
}

lisp_t cdr(const lisp_t& a)
{
  if(type_of(a) == object::type::Cons)
    return a->cdr();
  return nil;
}

lisp_t cadr(const lisp_t& a)
{
  if(type_of(a) == object::type::Cons)
    return list::car(a->cdr());
  return nil;
}

lisp_t cdar(const lisp_t& a)
{
  if(type_of(a) == object::type::Cons)
    return list::cdr(a->car());
  return nil;
}

lisp_t caar(const lisp_t& a)
{
  if(type_of(a) == object::type::Cons)
    return list::car(a->car());
  return nil;
}

lisp_t cddr(const lisp_t& a)
{
  if(type_of(a) == object::type::Cons)
    return list::cdr(a->cdr());
  return nil;
}

lisp_t cdddr(const lisp_t& a)
{
  if(type_of(a) == object::type::Cons)
    return list::cdr(list::cdr(a->cdr()));
  return nil;
}

lisp_t caddr(const lisp_t& a)
{
  if(type_of(a) == object::type::Cons)
    return list::car(list::cdr(a->cdr()));
  return nil;
}

lisp_t cdadr(const lisp_t& a)
{
  if(type_of(a) == object::type::Cons)
    return list::cdr(list::car(a->cdr()));
  return nil;
}

lisp_t caadr(const lisp_t& a)
{
  if(type_of(a) == object::type::Cons)
    return list::car(list::car(a->cdr()));
  return nil;
}

lisp_t cddar(const lisp_t& a)
{
  if(type_of(a) == object::type::Cons)
    return list::cdr(list::cdr(a->car()));
  return nil;
}

lisp_t cadar(const lisp_t& a)
{
  if(type_of(a) == object::type::Cons)
    return list::car(list::cdr(a->car()));
  return nil;
}

lisp_t cdaar(const lisp_t& a)
{
  if(type_of(a) == object::type::Cons)
    return list::cdr(list::car(a->car()));
  return nil;
}

lisp_t caaar(const lisp_t& a)
{
  if(type_of(a) == object::type::Cons)
    return list::car(list::car(a->car()));
  return nil;
}

lisp_t rplaca(lisp_t x, const lisp_t& y)
{
  check(x, object::type::Cons);
  x->car(y);
  return x;
}

lisp_t rplacd(lisp_t x, const lisp_t& y)
{
  check(x, object::type::Cons);
  x->cdr(y);
  return x;
}

lisp_t nconc(lisp_t x)
{
  lisp_t cl;

  lisp_t newl = nil;
  lisp_t curp = newl;
  for(; !is_nil(x); x = x->cdr())
  {
    if(!is_nil(x->car()))
    {
      check(x->car(), object::type::Cons);
      if(is_nil(curp))
      {
        curp = x->car();
        newl = curp;
      }
      else
        list::rplacd(curp, x->car());
      for(cl = x->car(); !is_nil(cl->cdr()); cl = cl->cdr())
        ;
      curp = cl;
    }
  }
  return newl;
}

lisp_t tconc(lisp_t cell, const lisp_t& obj)
{
  if(is_nil(cell))
  {
    cell = cons(cons(obj, nil), nil);
    return list::rplacd(cell, cell->car());
  }
  check(cell, object::type::Cons);
  if(type_of(cell->car()) != object::type::Cons)
  {
    list::rplacd(cell, cons(obj, nil));
    return list::rplaca(cell, cell->cdr());
  }
  list::rplacd(cell->cdr(), cons(obj, nil));
  return list::rplacd(cell, cell->cdr()->cdr());
}

lisp_t attach(const lisp_t& obj, lisp_t list)
{
  if(is_nil(list))
    return cons(obj, nil);
  check(list, object::type::Cons);
  list::rplacd(list, cons(list->car(), list->cdr()));
  list->car(obj);
  return list;
}

lisp_t append(const lisp_t& x)
{
  const lisp_t newl = cons(nil, nil);
  lisp_t curp = newl;
  for(const auto& v: x)
  {
    if(!is_nil(v))
    {
      check(v, object::type::Cons);
      for(auto const& cl: v)
      {
        list::rplacd(curp, cons(cl, nil));
        curp = curp->cdr();
      }
    }
  }
  return newl->cdr();
}

lisp_t null(const lisp_t& a)
{
  if(is_nil(a))
    return T;
  return nil;
}

lisp_t list(const lisp_t& x) { return x; }

lisp_t length(const lisp_t& x)
{
  int i = 0;
  for(const auto& v: x)
    ++i;
  return mknumber(i);
}

lisp_t nth(const lisp_t& x, const lisp_t& p)
{
  check(p, object::type::Integer);
  if(is_nil(x))
    return nil;
  check(x, object::type::Cons);
  return nth(x, p->as_integer());
}

namespace pn
{
inline constexpr std::string_view CAR = "car";       // car
inline constexpr std::string_view CDR = "cdr";       // cdr
inline constexpr std::string_view CADR = "cadr";     // cadr
inline constexpr std::string_view CDAR = "cdar";     // cdar
inline constexpr std::string_view CAAR = "caar";     // caar
inline constexpr std::string_view CDDR = "cddr";     // cddr
inline constexpr std::string_view CDDDR = "cdddr";   // cdddr
inline constexpr std::string_view CADDR = "caddr";   // caddr
inline constexpr std::string_view CDADR = "cdadr";   // cdadr
inline constexpr std::string_view CAADR = "caadr";   // caadr
inline constexpr std::string_view CDDAR = "cddar";   // cddar
inline constexpr std::string_view CADAR = "cadar";   // cadar
inline constexpr std::string_view CDAAR = "cdaar";   // cdaar
inline constexpr std::string_view CAAAR = "caaar";   // caaar
inline constexpr std::string_view LENGTH = "length"; // length of list
inline constexpr std::string_view LIST = "list";     // make list of args
inline constexpr std::string_view NCONC = "nconc";   // destructive append
inline constexpr std::string_view NTH = "nth";       // nth car in list
inline constexpr std::string_view NULL_ = "null";    // t if nil
inline constexpr std::string_view RPLACA = "rplaca"; // replace car
inline constexpr std::string_view RPLACD = "rplacd"; // replace cdr
inline constexpr std::string_view TCONC = "tconc";   // add to end of list
inline constexpr std::string_view ATTACH = "attach"; // attach object at front of list
inline constexpr std::string_view APPEND = "append"; // append lists
} // namespace pn

void init()
{
  // clang-format off
  mkprim(pn::ATTACH,  attach,  subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::APPEND,  append,  subr_t::subr::EVAL,   subr_t::spread::NOSPREAD);
  mkprim(pn::CAR,     car,     subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::CDR,     cdr,     subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::CADR,    cadr,    subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::CDAR,    cdar,    subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::CAAR,    caar,    subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::CDDR,    cddr,    subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::CDDDR,   cdddr,   subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::CADDR,   caddr,   subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::CDADR,   cdadr,   subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::CAADR,   caadr,   subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::CDDAR,   cddar,   subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::CADAR,   cadar,   subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::CDAAR,   cdaar,   subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::CAAAR,   caaar,   subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::LENGTH,  length,  subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::LIST,    list,    subr_t::subr::EVAL,   subr_t::spread::NOSPREAD);
  mkprim(pn::NCONC,   nconc,   subr_t::subr::EVAL,   subr_t::spread::NOSPREAD);
  mkprim(pn::NTH,     nth,     subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::NULL_,   null,    subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::RPLACA,  rplaca,  subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::RPLACD,  rplacd,  subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::TCONC,   tconc,   subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  // clang-format on
}

} // namespace lisp::details::list
