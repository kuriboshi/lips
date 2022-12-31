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
#include "check.hh"
#include "prim.hh"
#include "exit.hh"

namespace lisp
{
lisp_t C_ERROR;
lisp_t C_LAMBDA;
lisp_t C_NLAMBDA;
lisp_t C_QUOTE;
} // namespace lisp

namespace lisp::details::prim
{
/// @brief Make an indirect pointer to the object OBJ.
///
lisp_t mkindirect(lisp_t obj)
{
  // If already an indirect type object, return it.  We want all symbols that
  // we include in closures on the same level to refer to the same value.
  if(type_of(obj) == type::Indirect)
    return obj;
  // If it's a new object, cons up the storage for it wasting the car part.
  return getobject(indirect_t{obj});
}

/// @brief Builds a list of indirect pointers to the values of the symbols in
/// the list VARS. Used to construct a closure.
///
lisp_t closobj(context& ctx, lisp_t vars)
{
  if(is_nil(vars))
    return nil;
  check(vars, type::Cons);
  check(vars->car(), type::Symbol);
  return cons(mkindirect(vars->car()->value()), closobj(ctx, vars->cdr()));
}

lisp_t car(context&, lisp_t a)
{
  if(type_of(a) == type::Cons)
    return a->car();
  return nil;
}

lisp_t cdr(context&, lisp_t a)
{
  if(type_of(a) == type::Cons)
    return a->cdr();
  return nil;
}

lisp_t cadr(context& ctx, lisp_t a)
{
  if(type_of(a) == type::Cons)
    return prim::car(ctx, a->cdr());
  return nil;
}

lisp_t cdar(context& ctx, lisp_t a)
{
  if(type_of(a) == type::Cons)
    return prim::cdr(ctx, a->car());
  return nil;
}

lisp_t caar(context& ctx, lisp_t a)
{
  if(type_of(a) == type::Cons)
    return prim::car(ctx, a->car());
  return nil;
}

lisp_t cddr(context& ctx, lisp_t a)
{
  if(type_of(a) == type::Cons)
    return prim::cdr(ctx, a->cdr());
  return nil;
}

lisp_t cdddr(context& ctx, lisp_t a)
{
  if(type_of(a) == type::Cons)
    return prim::cdr(ctx, prim::cdr(ctx, a->cdr()));
  return nil;
}

lisp_t caddr(context& ctx, lisp_t a)
{
  if(type_of(a) == type::Cons)
    return prim::car(ctx, prim::cdr(ctx, a->cdr()));
  return nil;
}

lisp_t cdadr(context& ctx, lisp_t a)
{
  if(type_of(a) == type::Cons)
    return prim::cdr(ctx, prim::car(ctx, a->cdr()));
  return nil;
}

lisp_t caadr(context& ctx, lisp_t a)
{
  if(type_of(a) == type::Cons)
    return prim::car(ctx, prim::car(ctx, a->cdr()));
  return nil;
}

lisp_t cddar(context& ctx, lisp_t a)
{
  if(type_of(a) == type::Cons)
    return prim::cdr(ctx, prim::cdr(ctx, a->car()));
  return nil;
}

lisp_t cadar(context& ctx, lisp_t a)
{
  if(type_of(a) == type::Cons)
    return prim::car(ctx, prim::cdr(ctx, a->car()));
  return nil;
}

lisp_t cdaar(context& ctx, lisp_t a)
{
  if(type_of(a) == type::Cons)
    return prim::cdr(ctx, prim::car(ctx, a->car()));
  return nil;
}

lisp_t caaar(context& ctx, lisp_t a)
{
  if(type_of(a) == type::Cons)
    return prim::car(ctx, prim::car(ctx, a->car()));
  return nil;
}

lisp_t rplaca(context&, lisp_t x, lisp_t y)
{
  check(x, type::Cons);
  x->car(y);
  return x;
}

lisp_t rplacd(context&, lisp_t x, lisp_t y)
{
  check(x, type::Cons);
  x->cdr(y);
  return x;
}

lisp_t eq(context&, lisp_t a, lisp_t b)
{
  if(a == b)
    return T;
  if(type_of(a) == type::Integer && type_of(b) == type::Integer && a->intval() == b->intval())
    return T;
  return nil;
}

lisp_t atom(context&, lisp_t a)
{
  if(is_nil(a) || is_T(a) || type_of(a) == type::Symbol || type_of(a) == type::Integer || type_of(a) == type::Float)
    return T;
  return nil;
}

lisp_t nconc(context& ctx, lisp_t x)
{
  lisp_t cl;

  lisp_t newl = nil;
  lisp_t curp = newl;
  for(; !is_nil(x); x = x->cdr())
  {
    if(!is_nil(x->car()))
    {
      check(x->car(), type::Cons);
      if(is_nil(curp))
      {
        curp = x->car();
        newl = curp;
      }
      else
        prim::rplacd(ctx, curp, x->car());
      for(cl = x->car(); !is_nil(cl->cdr()); cl = cl->cdr())
        ;
      curp = cl;
    }
  }
  return newl;
}

lisp_t tconc(context& ctx, lisp_t cell, lisp_t obj)
{
  if(is_nil(cell))
  {
    cell = cons(cons(obj, nil), nil);
    return prim::rplacd(ctx, cell, cell->car());
  }
  check(cell, type::Cons);
  if(type_of(cell->car()) != type::Cons)
  {
    prim::rplacd(ctx, cell, cons(obj, nil));
    return prim::rplaca(ctx, cell, cell->cdr());
  }
  prim::rplacd(ctx, cell->cdr(), cons(obj, nil));
  return prim::rplacd(ctx, cell, cell->cdr()->cdr());
}

lisp_t attach(context& ctx, lisp_t obj, lisp_t list)
{
  if(is_nil(list))
    return cons(obj, nil);
  check(list, type::Cons);
  prim::rplacd(ctx, list, cons(list->car(), list->cdr()));
  return prim::rplaca(ctx, list, obj);
}

lisp_t append(context& ctx, lisp_t x)
{
  lisp_t cl;

  lisp_t newl = cons(nil, nil);
  lisp_t curp = newl;
  for(; !is_nil(x); x = x->cdr())
  {
    if(!is_nil(x->car()))
    {
      check(x->car(), type::Cons);
      for(cl = x->car(); !is_nil(cl); cl = cl->cdr())
      {
        prim::rplacd(ctx, curp, cons(cl->car(), nil));
        curp = curp->cdr();
      }
    }
  }
  return newl->cdr();
}

lisp_t null(context&, lisp_t a)
{
  if(is_nil(a))
    return T;
  return nil;
}

lisp_t quote(context&, lisp_t x) { return x; }

lisp_t lambda(context&, lisp_t x, lisp_t f) { return alloc::mklambda(x, f, true); }

lisp_t nlambda(context&, lisp_t x, lisp_t f) { return alloc::mklambda(x, f, false); }

lisp_t list(context&, lisp_t x) { return x; }

lisp_t length(context&, lisp_t x)
{
  int i = 0;
  while(!is_nil(x) && type_of(x) == type::Cons)
  {
    x = x->cdr();
    i++;
  }
  return mknumber(i);
}

lisp_t closure(context& ctx, lisp_t fun, lisp_t vars)
{
  auto c = ref_closure_t::create();
  c->cfunction = fun;
  c->closed = vars;
  auto f = prim::length(ctx, vars);
  c->count = f->intval();
  f = closobj(ctx, vars);
  if(f == C_ERROR)
    return f;
  c->cvalues = f;
  return getobject(c);
}

inline lisp_t _nth(context&, lisp_t list, int n)
{
  lisp_t ls;
  for(ls = list; n > 1 && !is_nil(ls); n--, ls = ls->cdr())
    ;
  if(!is_nil(ls))
    return ls;
  return nil;
}

lisp_t nth(context& ctx, lisp_t x, lisp_t p)
{
  check(p, type::Integer);
  if(is_nil(x))
    return nil;
  check(x, type::Cons);
  return _nth(ctx, x, p->intval());
}

lisp_t error(context& ctx, lisp_t mess)
{
  check(mess, type::String);
  return ctx.error(error_errc::user_error, mess);
}

lisp_t exit(context&, lisp_t status)
{
  if(is_nil(status))
    throw lisp_finish("exit called", 0);
  check(status, type::Integer);
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
  C_ERROR = intern(pn::ERROR);
  C_LAMBDA = intern(pn::LAMBDA);
  C_NLAMBDA = intern(pn::NLAMBDA);
  C_QUOTE = intern(pn::QUOTE);

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
