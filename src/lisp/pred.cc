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

#include "alloc.hh"
#include "pred.hh"
#include "prim.hh"
#include "user.hh"

namespace lisp::details::pred
{
lisp_t numberp(context&, lisp_t a)
{
  switch(type_of(a))
  {
    case type::Integer:
    case type::Float:
      return a;
    default:
      return nil;
  }
}

lisp_t listp(context&, lisp_t a)
{
  if(type_of(a) == type::Cons)
    return a;
  return nil;
}

lisp_t memb(context&, lisp_t x, lisp_t ls)
{
  while(!eq(ls, nil))
  {
    if(eq(x, ls->car()))
      return ls;
    ls = ls->cdr();
  }
  return nil;
}

lisp_t equal(context& ctx, lisp_t l1, lisp_t l2)
{
  if(type_of(l1) != type_of(l2))
    return nil;
  if(l1 == l2)
    return T;
  switch(type_of(l1))
  {
    case type::Cons:
      while(type_of(l1) == type_of(l2))
      {
        if(pred::equal(ctx, l1->car(), l2->car()) != nil)
          return pred::equal(ctx, l1->cdr(), l2->cdr());
        return nil;
      }
    case type::String:
      return (l1->string() == l2->string()) ? T : nil;
    case type::Lambda:
      return user::funeq(ctx, l1, l2);
    case type::Integer:
      return (l1->intval() == l2->intval() ? T : nil);
    default:
      break;
  }
  return nil;
}

lisp_t nlistp(context&, lisp_t a)
{
  if(a == nil)
    return T;
  if(type_of(a) != type::Cons)
    return a;
  return nil;
}

lisp_t neq(context&, lisp_t a, lisp_t b)
{
  if(a != b)
    return T;
  return nil;
}

lisp_t boundp(context&, lisp_t a)
{
  if(type_of(a) != type::Symbol)
    return nil;
  if(type_of(a->value()) != type::Unbound)
    return T;
  return nil;
}

lisp_t litatom(context&, lisp_t a)
{
  if(type_of(a) == type::Symbol || type_of(a) == type::T)
    return T;
  return nil;
}

lisp_t xtypeof(context&, lisp_t a)
{
  switch(type_of(a))
  {
    case type::Symbol:
      return C_SYMBOL;
    case type::Integer:
      return C_INTEGER;
    case type::Float:
      return C_FLOAT;
    case type::Indirect:
      return C_INDIRECT;
    case type::Cons:
      return C_CONS;
    case type::String:
      return C_STRING;
    case type::Subr:
      if(a->subr().subr == subr_t::subr::EVAL)
        return C_SUBR;
      return C_FSUBR;
    case type::Lambda:
      if(a->lambda()->eval)
        return C_LAMBDA;
      return C_NLAMBDA;
    case type::Closure:
      return C_CLOSURE;
    case type::Unbound:
      return C_UNBOUND;
    case type::Environ:
      return C_ENVIRON;
    case type::T:
      return T;
    case type::Eof:
      return C_ENDOFFILE;
    case type::Error:
      return C_ERROR;
    case type::File:
      return C_FILE;
    case type::Cvariable:
      return C_CVARIABLE;
    case type::Nil:
      return nil;
  }
  // Never reached since all cases are handled in the switch statement.
}

namespace pn
{
inline constexpr auto LISTP = "listp";     // t if cons
inline constexpr auto NLISTP = "nlistp";   // not listp
inline constexpr auto NEQ = "neq";         // not eq
inline constexpr auto NUMBERP = "numberp"; // integer of float
inline constexpr auto MEMB = "memb";       // t if a in l
inline constexpr auto EQUAL = "equal";     // equal
inline constexpr auto BOUNDP = "boundp";   // t if var bound
inline constexpr auto LITATOM = "litatom"; // t if is a literal atom (Interlisp)
inline constexpr auto SYMBOLP = "symbolp"; // t if is a symbol (CL)
inline constexpr auto TYPEOF = "typeof";   // return type as an atom
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
