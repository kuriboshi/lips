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
#include "pred.hh"
#include "prim.hh"
#include "user.hh"

namespace lisp::details::pred
{
lisp_t numberp(lisp_t a)
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

lisp_t listp(lisp_t a)
{
  if(type_of(a) == object::type::Cons)
    return a;
  return nil;
}

lisp_t memb(lisp_t x, lisp_t ls)
{
  while(!eq(ls, nil))
  {
    if(eq(x, ls->car()))
      return ls;
    ls = ls->cdr();
  }
  return nil;
}

lisp_t equal(lisp_t l1, lisp_t l2)
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
        if(pred::equal(l1->car(), l2->car()) != nil)
          return pred::equal(l1->cdr(), l2->cdr());
        return nil;
      }
    case object::type::String:
      return (l1->string() == l2->string()) ? T : nil;
    case object::type::Lambda:
      return user::funeq(l1, l2);
    case object::type::Integer:
      return (l1->intval() == l2->intval() ? T : nil);
    default:
      break;
  }
  return nil;
}

lisp_t nlistp(lisp_t a)
{
  if(a == nil)
    return T;
  if(type_of(a) != object::type::Cons)
    return a;
  return nil;
}

lisp_t neq(lisp_t a, lisp_t b)
{
  if(a != b)
    return T;
  return nil;
}

lisp_t boundp(lisp_t a)
{
  if(type_of(a) != object::type::Symbol)
    return nil;
  if(a->value() != C_UNBOUND)
    return T;
  return nil;
}

lisp_t litatom(lisp_t a)
{
  if(type_of(a) == object::type::Symbol || type_of(a) == object::type::Nil)
    return T;
  return nil;
}

lisp_t xtypeof(lisp_t a)
{
  switch(type_of(a))
  {
    case object::type::Symbol:
      return C_SYMBOL;
    case object::type::Integer:
      return C_INTEGER;
    case object::type::Float:
      return C_FLOAT;
    case object::type::Indirect:
      return C_INDIRECT;
    case object::type::Cons:
      return C_CONS;
    case object::type::String:
      return C_STRING;
    case object::type::Subr:
      if(a->subr().subr == subr_t::subr::EVAL)
        return C_SUBR;
      return C_FSUBR;
    case object::type::Lambda:
      if(a->lambda().eval)
        return C_LAMBDA;
      return C_NLAMBDA;
    case object::type::Closure:
      return C_CLOSURE;
    case object::type::Environ:
      return C_ENVIRON;
    case object::type::File:
      return C_FILE;
    case object::type::Cvariable:
      return C_CVARIABLE;
    case object::type::Nil:
      break;
  }
  // To avoid warning from gcc that this function doesn't return anything in
  // spite of handling all switch cases.
  return nil;
}

namespace pn
{
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

} // namespace lisp::details::pred
