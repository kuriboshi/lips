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

#include "lisp/prim.hh"

namespace lisp
{
LISPT C_ERROR;
LISPT C_LAMBDA;
LISPT C_NLAMBDA;
LISPT C_QUOTE;
} // namespace lisp

namespace lisp::details::prim
{
/* 
 * mkindirect - makes an indirect pointer to the object OBJ. If already an 
 *              indirect object,  return it.
 */
static LISPT mkindirect(lisp& l, LISPT obj)
{
  /* If already an indirect type object, return it. */
  /* We want all symbols that we include in closures */
  /* on the same level to refer to the same value. */
  if(type_of(obj) == type::Indirect)
    return obj;
  /* If it's a new object, cons up the storage for it */
  /* wasting the car part. */
  auto iobj = alloc::getobject();
  iobj->set(indirect_t{obj});
  return iobj;
}

/* 
 * closobj - builds a list of indirect pointers to the values of the 
 *           symbols in the list VARS. Used to construct a closure.
 */
LISPT closobj(lisp& l, LISPT vars)
{
  if(is_NIL(vars))
    return NIL;
  check(vars, type::Cons);
  check(vars->car(), type::Symbol);
  return cons(mkindirect(l, vars->car()->value()), closobj(l, vars->cdr()));
}

LISPT car(lisp& l, LISPT a)
{
  if(type_of(a) == type::Cons)
    return a->car();
  return NIL;
}

LISPT cdr(lisp& l, LISPT a)
{
  if(type_of(a) == type::Cons)
    return a->cdr();
  return NIL;
}

LISPT cadr(lisp& l, LISPT a)
{
  if(type_of(a) == type::Cons)
    return prim::car(l, a->cdr());
  return NIL;
}

LISPT cdar(lisp& l, LISPT a)
{
  if(type_of(a) == type::Cons)
    return prim::cdr(l, a->car());
  return NIL;
}

LISPT caar(lisp& l, LISPT a)
{
  if(type_of(a) == type::Cons)
    return prim::car(l, a->car());
  return NIL;
}

LISPT cddr(lisp& l, LISPT a)
{
  if(type_of(a) == type::Cons)
    return prim::cdr(l, a->cdr());
  return NIL;
}

LISPT cdddr(lisp& l, LISPT a)
{
  if(type_of(a) == type::Cons)
    return prim::cdr(l, prim::cdr(l, a->cdr()));
  return NIL;
}

LISPT caddr(lisp& l, LISPT a)
{
  if(type_of(a) == type::Cons)
    return prim::car(l, prim::cdr(l, a->cdr()));
  return NIL;
}

LISPT cdadr(lisp& l, LISPT a)
{
  if(type_of(a) == type::Cons)
    return prim::cdr(l, prim::car(l, a->cdr()));
  return NIL;
}

LISPT caadr(lisp& l, LISPT a)
{
  if(type_of(a) == type::Cons)
    return prim::car(l, prim::car(l, a->cdr()));
  return NIL;
}

LISPT cddar(lisp& l, LISPT a)
{
  if(type_of(a) == type::Cons)
    return prim::cdr(l, prim::cdr(l, a->car()));
  return NIL;
}

LISPT cadar(lisp& l, LISPT a)
{
  if(type_of(a) == type::Cons)
    return prim::car(l, prim::cdr(l, a->car()));
  return NIL;
}

LISPT cdaar(lisp& l, LISPT a)
{
  if(type_of(a) == type::Cons)
    return prim::cdr(l, prim::car(l, a->car()));
  return NIL;
}

LISPT caaar(lisp& l, LISPT a)
{
  if(type_of(a) == type::Cons)
    return prim::car(l, prim::car(l, a->car()));
  return NIL;
}

LISPT rplaca(lisp& l, LISPT x, LISPT y)
{
  check(x, type::Cons);
  x->car(y);
  return x;
}

LISPT rplacd(lisp& l, LISPT x, LISPT y)
{
  check(x, type::Cons);
  x->cdr(y);
  return x;
}

LISPT eq(lisp& l, LISPT a, LISPT b)
{
  if(a == b)
    return T;
  if(type_of(a) == type::Integer && type_of(b) == type::Integer && a->intval() == b->intval())
    return T;
  return NIL;
}

LISPT atom(lisp& l, LISPT a)
{
  if(is_NIL(a) || is_T(a) || type_of(a) == type::Symbol || type_of(a) == type::Integer || type_of(a) == type::Float)
    return T;
  return NIL;
}

LISPT nconc(lisp& l, LISPT x)
{
  LISPT cl;

  LISPT newl = NIL;
  LISPT curp = newl;
  for(; !is_NIL(x); x = x->cdr())
  {
    if(!is_NIL(x->car()))
    {
      check(x->car(), type::Cons);
      if(is_NIL(curp))
      {
        curp = x->car();
        newl = curp;
      }
      else
        prim::rplacd(l, curp, x->car());
      for(cl = x->car(); !is_NIL(cl->cdr()); cl = cl->cdr())
        ;
      curp = cl;
    }
  }
  return newl;
}

LISPT tconc(lisp& l, LISPT cell, LISPT obj)
{
  if(is_NIL(cell))
  {
    cell = cons(cons(obj, NIL), NIL);
    return prim::rplacd(l, cell, cell->car());
  }
  check(cell, type::Cons);
  if(type_of(cell->car()) != type::Cons)
  {
    prim::rplacd(l, cell, cons(obj, NIL));
    return prim::rplaca(l, cell, cell->cdr());
  }
  prim::rplacd(l, cell->cdr(), cons(obj, NIL));
  return prim::rplacd(l, cell, cell->cdr()->cdr());
}

LISPT attach(lisp& l, LISPT obj, LISPT list)
{
  if(is_NIL(list))
    return cons(obj, NIL);
  check(list, type::Cons);
  prim::rplacd(l, list, cons(list->car(), list->cdr()));
  return prim::rplaca(l, list, obj);
}

LISPT append(lisp& l, LISPT x)
{
  LISPT cl;

  LISPT newl = cons(NIL, NIL);
  LISPT curp = newl;
  for(; !is_NIL(x); x = x->cdr())
  {
    if(!is_NIL(x->car()))
    {
      check(x->car(), type::Cons);
      for(cl = x->car(); !is_NIL(cl); cl = cl->cdr())
      {
        prim::rplacd(l, curp, cons(cl->car(), NIL));
        curp = curp->cdr();
      }
    }
  }
  return newl->cdr();
}

LISPT null(lisp& l, LISPT a)
{
  if(is_NIL(a))
    return T;
  return NIL;
}

LISPT quote(lisp& l, LISPT x) { return x; }

LISPT lambda(lisp& l, LISPT x, LISPT f) { return alloc::mklambda(x, f, type::Lambda); }

LISPT nlambda(lisp& l, LISPT x, LISPT f) { return alloc::mklambda(x, f, type::Nlambda); }

LISPT list(lisp& l, LISPT x) { return x; }

LISPT length(lisp& l, LISPT x)
{
  int i = 0;
  while(!is_NIL(x) && type_of(x) == type::Cons)
  {
    x = x->cdr();
    i++;
  }
  return mknumber(i);
}

LISPT closure(lisp& l, LISPT fun, LISPT vars)
{
  ref_closure_t c(new closure_t);
  c->cfunction = fun;
  c->closed = vars;
  auto f = prim::length(l, vars);
  c->count = f->intval();
  f = closobj(l, vars);
  if(type_of(f) == type::Error)
    return f;
  c->cvalues = f;
  auto clos = alloc::getobject();
  clos->set(c);
  return clos;
}

inline LISPT _nth(lisp& l, LISPT list, int n)
{
  LISPT ls;
  for(ls = list; n > 1 && !is_NIL(ls); n--, ls = ls->cdr())
    ;
  if(!is_NIL(ls))
    return ls;
  return NIL;
}

LISPT nth(lisp& l, LISPT x, LISPT p)
{
  check(p, type::Integer);
  if(is_NIL(x))
    return NIL;
  check(x, type::Cons);
  return _nth(l, x, p->intval());
}

LISPT error(lisp& l, LISPT mess)
{
  check(mess, type::String);
  return l.error(error_errc::user_error, mess);
}

LISPT uxexit(lisp& l, LISPT status)
{
  if(is_NIL(status))
    throw lisp_finish("exit called", 0);
  check(status, type::Integer);
  throw lisp_finish("exit called", status->intval());
}

namespace pn
{
inline constexpr auto ATOM = "atom";       // t if atom
inline constexpr auto CAR = "car";         // car
inline constexpr auto CDR = "cdr";         // cdr
inline constexpr auto CADR = "cadr";       // cadr
inline constexpr auto CDAR = "cdar";       // cdar
inline constexpr auto CAAR = "caar";       // caar
inline constexpr auto CDDR = "cddr";       // cddr
inline constexpr auto CDDDR = "cdddr";     // cdddr
inline constexpr auto CADDR = "caddr";     // caddr
inline constexpr auto CDADR = "cdadr";     // cdadr
inline constexpr auto CAADR = "caadr";     // caadr
inline constexpr auto CDDAR = "cddar";     // cddar
inline constexpr auto CADAR = "cadar";     // cadar
inline constexpr auto CDAAR = "cdaar";     // cdaar
inline constexpr auto CAAAR = "caaar";     // caaar
inline constexpr auto CLOSURE = "closure"; // create static environment
inline constexpr auto EQ = "eq";           // pointer equal
inline constexpr auto ERROR = "error";     // error
inline constexpr auto LAMBDA = "lambda";   // create lambda object
inline constexpr auto LENGTH = "length";   // length of list
inline constexpr auto LIST = "list";       // make list of args
inline constexpr auto NCONC = "nconc";     // destructive append
inline constexpr auto NLAMBDA = "nlambda"; // make nlambda object
inline constexpr auto NTH = "nth";         // nth car in list
inline constexpr auto NULL_ = "null";      // t if nil
inline constexpr auto QUOTE = "quote";     // don't eval arg
inline constexpr auto RPLACA = "rplaca";   // replace car
inline constexpr auto RPLACD = "rplacd";   // replace cdr
inline constexpr auto TCONC = "tconc";     // add to end of list
inline constexpr auto ATTACH = "attach";   // attach object at front of list
inline constexpr auto APPEND = "append";   // append lists
inline constexpr auto EXIT = "exit";       // exit lips
} // namespace pn

void init()
{
  C_ERROR = intern(pn::ERROR);
  C_ERROR->settype(type::Error);
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
  mkprim(pn::EXIT,    uxexit,  subr_t::subr::EVAL,   subr_t::spread::SPREAD);
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
