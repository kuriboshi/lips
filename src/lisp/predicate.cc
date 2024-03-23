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

#include "predicate.hh"

#include "alloc.hh"
#include "atoms.hh"
#include "user.hh"

namespace lisp::details::predicate
{
lisp_t eq(const lisp_t& a, const lisp_t& b)
{
  if(a == b)
    return T;
  if(type_of(a) == object::type::Integer && type_of(b) == object::type::Integer && a->as_integer() == b->as_integer())
    return T;
  return nil;
}

lisp_t atom(const lisp_t& a)
{
  if(is_nil(a) || is_T(a) || type_of(a) == object::type::Symbol || type_of(a) == object::type::Integer
    || type_of(a) == object::type::Float)
    return T;
  return nil;
}

lisp_t numberp(const lisp_t& a)
{
  switch(type_of(a))
  {
    case object::type::Integer:
    case object::type::Float:
      return a;
    default:
      return nil;
  }
}

lisp_t listp(const lisp_t& a)
{
  if(type_of(a) == object::type::Cons)
    return a;
  return nil;
}

lisp_t memb(const lisp_t& x, const lisp_t& ls)
{
  auto list = ls;
  while(!is_nil(list))
  {
    if(predicate::eq(x, list->car()))
      return list;
    list = list->cdr();
  }
  return nil;
}

lisp_t equal(const lisp_t& l1, const lisp_t& l2)
{
  if(type_of(l1) != type_of(l2))
    return nil;
  if(l1 == l2)
    return T;
  switch(type_of(l1))
  {
    case object::type::Cons:
      while(type_of(l1) == type_of(l2))
      {
        if(predicate::equal(l1->car(), l2->car()) != nil)
          return predicate::equal(l1->cdr(), l2->cdr());
        return nil;
      }
    case object::type::String:
      return (l1->as_string() == l2->as_string()) ? T : nil;
    case object::type::Lambda:
      return user::funeq(l1, l2);
    case object::type::Integer:
      return (l1->as_integer() == l2->as_integer() ? T : nil);
    default:
      break;
  }
  return nil;
}

lisp_t nlistp(const lisp_t& a)
{
  if(a == nil)
    return T;
  if(type_of(a) != object::type::Cons)
    return a;
  return nil;
}

lisp_t neq(const lisp_t& a, const lisp_t& b)
{
  if(a != b)
    return T;
  return nil;
}

lisp_t boundp(const lisp_t& a)
{
  if(type_of(a) != object::type::Symbol)
    return nil;
  if(a->value() != atoms::UNBOUND)
    return T;
  return nil;
}

lisp_t litatom(const lisp_t& a)
{
  if(type_of(a) == object::type::Symbol || type_of(a) == object::type::Nil)
    return T;
  return nil;
}

lisp_t xtypeof(const lisp_t& a)
{
  switch(type_of(a))
  {
    case object::type::Symbol:
      return atoms::SYMBOL;
    case object::type::Integer:
      return atoms::INTEGER;
    case object::type::Float:
      return atoms::FLOAT;
    case object::type::Indirect:
      return atoms::INDIRECT;
    case object::type::Cons:
      return atoms::CONS;
    case object::type::String:
      return atoms::STRING;
    case object::type::Subr:
      if(a->subr().subr == subr_t::subr::EVAL)
        return atoms::SUBR;
      return atoms::FSUBR;
    case object::type::Lambda:
      if(a->lambda().eval)
        return atoms::LAMBDA;
      return atoms::NLAMBDA;
    case object::type::Closure:
      return atoms::CLOSURE;
    case object::type::Environ:
      return atoms::ENVIRON;
    case object::type::File:
      return atoms::FILE;
    case object::type::Cvariable:
      return atoms::CVARIABLE;
    case object::type::Nil:
      break;
  }
  // To avoid warning from gcc that this function doesn't return anything in
  // spite of handling all switch cases.
  return nil;
}

namespace pn
{
inline constexpr std::string_view ATOM = "atom";       // t if atom
inline constexpr std::string_view EQ = "eq";           // pointer equal
inline constexpr std::string_view LISTP = "listp";     // t if cons
inline constexpr std::string_view NLISTP = "nlistp";   // not listp
inline constexpr std::string_view NEQ = "neq";         // not eq
inline constexpr std::string_view NUMBERP = "numberp"; // integer of float
inline constexpr std::string_view MEMB = "memb";       // t if a in l
inline constexpr std::string_view EQUAL = "equal";     // equal
inline constexpr std::string_view BOUNDP = "boundp";   // t if var bound
inline constexpr std::string_view LITATOM = "litatom"; // t if is a literal atom (Interlisp)
inline constexpr std::string_view SYMBOLP = "symbolp"; // t if is a symbol (CL)
inline constexpr std::string_view TYPEOF = "typeof";   // return type as an atom
} // namespace pn

void init()
{
  // clang-format off
  mkprim(pn::ATOM,    atom,    subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::EQ,      eq,      subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::LISTP,   listp,   subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::NLISTP,  nlistp,  subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::NEQ,     neq,     subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::NUMBERP, numberp, subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::MEMB,    memb,    subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::EQUAL,   equal,   subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::BOUNDP,  boundp,  subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::LITATOM, litatom, subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::SYMBOLP, litatom, subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::TYPEOF,  xtypeof, subr_t::subr::EVAL, subr_t::spread::SPREAD);
  // clang-format on
}

} // namespace lisp::details::predicate
