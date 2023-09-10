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
#include "check.hh"
#include "prim.hh"
#include "exit.hh"

namespace lisp::details::prim
{
/// @brief Make an indirect pointer to the object OBJ.
///
lisp_t mkindirect(lisp_t obj)
{
  // If already an indirect type object, return it.  We want all symbols that
  // we include in closures on the same level to refer to the same value.
  if(type_of(obj) == object::type::Indirect)
    return obj;
  // If it's a new object, cons up the storage for it wasting the car part.
  return getobject(indirect_t{obj});
}

/// @brief Builds a list of indirect pointers to the values of the symbols in
/// the list VARS. Used to construct a closure.
///
lisp_t closobj(lisp_t vars)
{
  if(is_nil(vars))
    return nil;
  check(vars, object::type::Cons);
  check(vars->car(), object::type::Symbol);
  return cons(mkindirect(vars->car()->value()), closobj(vars->cdr()));
}

lisp_t car(lisp_t a)
{
  if(type_of(a) == object::type::Cons)
    return a->car();
  return nil;
}

lisp_t cdr(lisp_t a)
{
  if(type_of(a) == object::type::Cons)
    return a->cdr();
  return nil;
}

lisp_t cadr(lisp_t a)
{
  if(type_of(a) == object::type::Cons)
    return prim::car(a->cdr());
  return nil;
}

lisp_t cdar(lisp_t a)
{
  if(type_of(a) == object::type::Cons)
    return prim::cdr(a->car());
  return nil;
}

lisp_t caar(lisp_t a)
{
  if(type_of(a) == object::type::Cons)
    return prim::car(a->car());
  return nil;
}

lisp_t cddr(lisp_t a)
{
  if(type_of(a) == object::type::Cons)
    return prim::cdr(a->cdr());
  return nil;
}

lisp_t cdddr(lisp_t a)
{
  if(type_of(a) == object::type::Cons)
    return prim::cdr(prim::cdr(a->cdr()));
  return nil;
}

lisp_t caddr(lisp_t a)
{
  if(type_of(a) == object::type::Cons)
    return prim::car(prim::cdr(a->cdr()));
  return nil;
}

lisp_t cdadr(lisp_t a)
{
  if(type_of(a) == object::type::Cons)
    return prim::cdr(prim::car(a->cdr()));
  return nil;
}

lisp_t caadr(lisp_t a)
{
  if(type_of(a) == object::type::Cons)
    return prim::car(prim::car(a->cdr()));
  return nil;
}

lisp_t cddar(lisp_t a)
{
  if(type_of(a) == object::type::Cons)
    return prim::cdr(prim::cdr(a->car()));
  return nil;
}

lisp_t cadar(lisp_t a)
{
  if(type_of(a) == object::type::Cons)
    return prim::car(prim::cdr(a->car()));
  return nil;
}

lisp_t cdaar(lisp_t a)
{
  if(type_of(a) == object::type::Cons)
    return prim::cdr(prim::car(a->car()));
  return nil;
}

lisp_t caaar(lisp_t a)
{
  if(type_of(a) == object::type::Cons)
    return prim::car(prim::car(a->car()));
  return nil;
}

lisp_t rplaca(lisp_t x, lisp_t y)
{
  check(x, object::type::Cons);
  x->car(y);
  return x;
}

lisp_t rplacd(lisp_t x, lisp_t y)
{
  check(x, object::type::Cons);
  x->cdr(y);
  return x;
}

lisp_t eq(lisp_t a, lisp_t b)
{
  if(a == b)
    return T;
  if(type_of(a) == object::type::Integer && type_of(b) == object::type::Integer && a->intval() == b->intval())
    return T;
  return nil;
}

lisp_t atom(lisp_t a)
{
  if(is_nil(a) || is_T(a) || type_of(a) == object::type::Symbol || type_of(a) == object::type::Integer
    || type_of(a) == object::type::Float)
    return T;
  return nil;
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
        prim::rplacd(curp, x->car());
      for(cl = x->car(); !is_nil(cl->cdr()); cl = cl->cdr())
        ;
      curp = cl;
    }
  }
  return newl;
}

lisp_t tconc(lisp_t cell, lisp_t obj)
{
  if(is_nil(cell))
  {
    cell = cons(cons(obj, nil), nil);
    return prim::rplacd(cell, cell->car());
  }
  check(cell, object::type::Cons);
  if(type_of(cell->car()) != object::type::Cons)
  {
    prim::rplacd(cell, cons(obj, nil));
    return prim::rplaca(cell, cell->cdr());
  }
  prim::rplacd(cell->cdr(), cons(obj, nil));
  return prim::rplacd(cell, cell->cdr()->cdr());
}

lisp_t attach(lisp_t obj, lisp_t list)
{
  if(is_nil(list))
    return cons(obj, nil);
  check(list, object::type::Cons);
  prim::rplacd(list, cons(list->car(), list->cdr()));
  return prim::rplaca(list, obj);
}

lisp_t append(lisp_t x)
{
  lisp_t cl;

  const lisp_t newl = cons(nil, nil);
  lisp_t curp = newl;
  for(; !is_nil(x); x = x->cdr())
  {
    if(!is_nil(x->car()))
    {
      check(x->car(), object::type::Cons);
      for(cl = x->car(); !is_nil(cl); cl = cl->cdr())
      {
        prim::rplacd(curp, cons(cl->car(), nil));
        curp = curp->cdr();
      }
    }
  }
  return newl->cdr();
}

lisp_t null(lisp_t a)
{
  if(is_nil(a))
    return T;
  return nil;
}

lisp_t quote(lisp_t x) { return x; }

lisp_t lambda(lisp_t x, lisp_t f) { return alloc::mklambda(x, f, true); }

lisp_t nlambda(lisp_t x, lisp_t f) { return alloc::mklambda(x, f, false); }

lisp_t list(lisp_t x) { return x; }

lisp_t length(lisp_t x)
{
  int i = 0;
  while(!is_nil(x) && type_of(x) == object::type::Cons)
  {
    x = x->cdr();
    i++;
  }
  return mknumber(i);
}

lisp_t closure(lisp_t fun, lisp_t vars)
{
  auto c = ref_closure_t::create();
  c->cfunction = fun;
  c->closed = vars;
  auto f = prim::length(vars);
  c->count = f->intval();
  f = closobj(vars);
  if(f == C_ERROR)
    return f;
  c->cvalues = f;
  return getobject(c);
}

inline lisp_t _nth(lisp_t list, std::int64_t n)
{
  lisp_t ls;
  for(ls = list; n > 1 && !is_nil(ls); n--, ls = ls->cdr())
    ;
  if(!is_nil(ls))
    return ls;
  return nil;
}

lisp_t nth(lisp_t x, lisp_t p)
{
  check(p, object::type::Integer);
  if(is_nil(x))
    return nil;
  check(x, object::type::Cons);
  return _nth(x, p->intval());
}

lisp_t error(lisp_t mess)
{
  check(mess, object::type::String);
  return error(error_errc::user_error, mess);
}

lisp_t exit(lisp_t status)
{
  if(is_nil(status))
    throw lisp_finish("exit called", 0);
  check(status, object::type::Integer);
  throw lisp_finish("exit called", status->intval());
}

namespace pn
{
inline constexpr std::string_view ATOM = "atom";       // t if atom
inline constexpr std::string_view CAR = "car";         // car
inline constexpr std::string_view CDR = "cdr";         // cdr
inline constexpr std::string_view CADR = "cadr";       // cadr
inline constexpr std::string_view CDAR = "cdar";       // cdar
inline constexpr std::string_view CAAR = "caar";       // caar
inline constexpr std::string_view CDDR = "cddr";       // cddr
inline constexpr std::string_view CDDDR = "cdddr";     // cdddr
inline constexpr std::string_view CADDR = "caddr";     // caddr
inline constexpr std::string_view CDADR = "cdadr";     // cdadr
inline constexpr std::string_view CAADR = "caadr";     // caadr
inline constexpr std::string_view CDDAR = "cddar";     // cddar
inline constexpr std::string_view CADAR = "cadar";     // cadar
inline constexpr std::string_view CDAAR = "cdaar";     // cdaar
inline constexpr std::string_view CAAAR = "caaar";     // caaar
inline constexpr std::string_view CLOSURE = "closure"; // create static environment
inline constexpr std::string_view EQ = "eq";           // pointer equal
inline constexpr std::string_view ERROR = "error";     // error
inline constexpr std::string_view LAMBDA = "lambda";   // create lambda object
inline constexpr std::string_view LENGTH = "length";   // length of list
inline constexpr std::string_view LIST = "list";       // make list of args
inline constexpr std::string_view NCONC = "nconc";     // destructive append
inline constexpr std::string_view NLAMBDA = "nlambda"; // make nlambda object
inline constexpr std::string_view NTH = "nth";         // nth car in list
inline constexpr std::string_view NULL_ = "null";      // t if nil
inline constexpr std::string_view QUOTE = "quote";     // don't eval arg
inline constexpr std::string_view RPLACA = "rplaca";   // replace car
inline constexpr std::string_view RPLACD = "rplacd";   // replace cdr
inline constexpr std::string_view TCONC = "tconc";     // add to end of list
inline constexpr std::string_view ATTACH = "attach";   // attach object at front of list
inline constexpr std::string_view APPEND = "append";   // append lists
inline constexpr std::string_view EXIT = "exit";       // exit lips
} // namespace pn

void init()
{
  // clang-format off
  mkprim(pn::ATOM,    atom,    subr_t::subr::EVAL,   subr_t::spread::SPREAD);
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
  mkprim(pn::CLOSURE, closure, subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::EQ,      eq,      subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::ERROR,   error,   subr_t::subr::EVAL,   subr_t::spread::NOSPREAD);
  mkprim(pn::EXIT,    exit,    subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::LAMBDA,  lambda,  subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::LENGTH,  length,  subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::LIST,    list,    subr_t::subr::EVAL,   subr_t::spread::NOSPREAD);
  mkprim(pn::NCONC,   nconc,   subr_t::subr::EVAL,   subr_t::spread::NOSPREAD);
  mkprim(pn::NLAMBDA, nlambda, subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::NTH,     nth,     subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::NULL_,   null,    subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::QUOTE,   quote,   subr_t::subr::NOEVAL, subr_t::spread::SPREAD);
  mkprim(pn::RPLACA,  rplaca,  subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::RPLACD,  rplacd,  subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::TCONC,   tconc,   subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  // clang-format on
}

} // namespace lisp::details::prim
