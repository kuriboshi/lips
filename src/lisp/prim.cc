/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include "prim.hh"
#include "alloc.hh"
#include "except.hh"
#include "io.hh"

namespace lisp
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
  if(type_of(obj) == type::INDIRECT)
    return obj;
  /* If it's a new object, cons up the storage for it */
  /* wasting the car part. */
  auto iobj = l.a().getobject();
  iobj->set(indirect_t{obj});
  return iobj;
}

/* 
 * closobj - builds a list of indirect pointers to the values of the 
 *           symbols in the list VARS. Used to construct a closure.
 */
LISPT prim::closobj(lisp& l, LISPT vars)
{
  if(is_NIL(vars))
    return NIL;
  check(vars, type::CONS);
  check(vars->car(), type::SYMBOL);
  return cons(l, mkindirect(l, vars->car()->symvalue()), closobj(l, vars->cdr()));
}

LISPT prim::car(lisp& l, LISPT a)
{
  if(type_of(a) == type::CONS)
    return a->car();
  return NIL;
}

LISPT prim::cdr(lisp& l, LISPT a)
{
  if(type_of(a) == type::CONS)
    return a->cdr();
  return NIL;
}

LISPT prim::cadr(lisp& l, LISPT a)
{
  if(type_of(a) == type::CONS)
    return car(l, a->cdr());
  return NIL;
}

LISPT prim::cdar(lisp& l, LISPT a)
{
  if(type_of(a) == type::CONS)
    return cdr(l, a->car());
  return NIL;
}

LISPT prim::caar(lisp& l, LISPT a)
{
  if(type_of(a) == type::CONS)
    return car(l, a->car());
  return NIL;
}

LISPT prim::cddr(lisp& l, LISPT a)
{
  if(type_of(a) == type::CONS)
    return cdr(l, a->cdr());
  return NIL;
}

LISPT prim::cdddr(lisp& l, LISPT a)
{
  if(type_of(a) == type::CONS)
    return cdr(l, cdr(l, a->cdr()));
  return NIL;
}

LISPT prim::caddr(lisp& l, LISPT a)
{
  if(type_of(a) == type::CONS)
    return car(l, cdr(l, a->cdr()));
  return NIL;
}

LISPT prim::cdadr(lisp& l, LISPT a)
{
  if(type_of(a) == type::CONS)
    return cdr(l, car(l, a->cdr()));
  return NIL;
}

LISPT prim::caadr(lisp& l, LISPT a)
{
  if(type_of(a) == type::CONS)
    return car(l, car(l, a->cdr()));
  return NIL;
}

LISPT prim::cddar(lisp& l, LISPT a)
{
  if(type_of(a) == type::CONS)
    return cdr(l, cdr(l, a->car()));
  return NIL;
}

LISPT prim::cadar(lisp& l, LISPT a)
{
  if(type_of(a) == type::CONS)
    return car(l, cdr(l, a->car()));
  return NIL;
}

LISPT prim::cdaar(lisp& l, LISPT a)
{
  if(type_of(a) == type::CONS)
    return cdr(l, car(l, a->car()));
  return NIL;
}

LISPT prim::caaar(lisp& l, LISPT a)
{
  if(type_of(a) == type::CONS)
    return car(l, car(l, a->car()));
  return NIL;
}

LISPT prim::rplaca(lisp& l, LISPT x, LISPT y)
{
  check(x, type::CONS);
  x->car(y);
  return x;
}

LISPT prim::rplacd(lisp& l, LISPT x, LISPT y)
{
  check(x, type::CONS);
  x->cdr(y);
  return x;
}

LISPT prim::eq(lisp& l, LISPT a, LISPT b)
{
  if(EQ(a, b))
    return T;
  if(type_of(a) == type::INTEGER && type_of(b) == type::INTEGER
    && a->intval() == b->intval())
    return T;
  return NIL;
}

LISPT prim::atom(lisp& l, LISPT a)
{
  if(is_NIL(a) || is_T(a) || type_of(a) == type::SYMBOL || type_of(a) == type::INTEGER || type_of(a) == type::FLOAT)
    return T;
  return NIL;
}

LISPT prim::nconc(lisp& l, LISPT x)
{
  LISPT cl;

  LISPT newl = NIL;
  LISPT curp = newl;
  for(; !is_NIL(x); x = x->cdr())
  {
    if(!is_NIL(x->car()))
    {
      check(x->car(), type::CONS);
      if(is_NIL(curp))
      {
        curp = x->car();
        newl = curp;
      }
      else
        rplacd(l, curp, x->car());
      for(cl = x->car(); !is_NIL(cl->cdr()); cl = cl->cdr())
        ;
      curp = cl;
    }
  }
  return newl;
}

LISPT prim::tconc(lisp& l, LISPT cell, LISPT obj)
{
  if(is_NIL(cell))
  {
    cell = cons(l, cons(l, obj, NIL), NIL);
    return rplacd(l, cell, cell->car());
  }
  check(cell, type::CONS);
  if(type_of(cell->car()) != type::CONS)
  {
    rplacd(l, cell, cons(l, obj, NIL));
    return rplaca(l, cell, cell->cdr());
  }
  rplacd(l, cell->cdr(), cons(l, obj, NIL));
  return rplacd(l, cell, cell->cdr()->cdr());
}

LISPT prim::attach(lisp& l, LISPT obj, LISPT list)
{
  if(is_NIL(list))
    return cons(l, obj, NIL);
  check(list, type::CONS);
  rplacd(l, list, cons(l, list->car(), list->cdr()));
  return rplaca(l, list, obj);
}

LISPT prim::append(lisp& l, LISPT x)
{
  LISPT cl;

  LISPT newl = cons(l, NIL, NIL);
  LISPT curp = newl;
  for(; !is_NIL(x); x = x->cdr())
  {
    if(!is_NIL(x->car()))
    {
      check(x->car(), type::CONS);
      for(cl = x->car(); !is_NIL(cl); cl = cl->cdr())
      {
        rplacd(l, curp, cons(l, cl->car(), NIL));
        curp = curp->cdr();
      }
    }
  }
  return newl->cdr();
}

LISPT prim::null(lisp& l, LISPT a)
{
  if(EQ(a, NIL))
    return T;
  return NIL;
}

LISPT prim::quote(lisp& l, LISPT x) { return x; }

LISPT prim::lambda(lisp& l, LISPT x, LISPT f) { return mklambda(l, x, f, type::LAMBDA); }

LISPT prim::nlambda(lisp& l, LISPT x, LISPT f) { return mklambda(l, x, f, type::NLAMBDA); }

LISPT prim::list(lisp& l, LISPT x) { return x; }

LISPT prim::length(lisp& l, LISPT x)
{
  int i = 0;
  while(!is_NIL(x) && type_of(x) == type::CONS)
  {
    x = x->cdr();
    i++;
  }
  return mknumber(l, i);
}

LISPT prim::closure(lisp& l, LISPT fun, LISPT vars)
{
  closure_t c;
  c.cfunction = fun;
  c.closed = vars;
  auto f = length(l, vars);
  c.count = f->intval();
  f = closobj(l, vars);
  if(type_of(f) == type::ERROR)
    return f;
  c.cvalues = f;
  auto clos = l.a().getobject();
  clos->set(c);
  return clos;
}

inline LISPT _nth(lisp& l, LISPT list, int n)
{
  LISPT ls;

  for(ls = list; n > 1 && !is_NIL(ls); n--, ls = ls->cdr())
    ;
  if(!is_NIL(ls))
    return ls->car();
  return NIL;
}

LISPT prim::nth(lisp& l, LISPT x, LISPT p)
{
  check(p, type::INTEGER);
  if(is_NIL(x))
    return NIL;
  check(x, type::CONS);
  return _nth(l, x, p->intval());
}

LISPT prim::nthd(lisp& l, LISPT list, LISPT pos)
{
  check(pos, type::INTEGER);
  int p = pos->intval();
  if(is_NIL(list))
    return NIL;
  check(list, type::CONS);
  LISPT ls;
  for(ls = list; type_of(ls) == type::CONS && p > 1; ls = ls->cdr())
  {
    --p;
  }
  return ls;
}

LISPT prim::error(lisp& l, LISPT mess)
{
  check(mess, type::STRING);
  return l.error(USER_ERROR, mess);
}

LISPT prim::uxexit(lisp& l, LISPT status)
{
  if(is_NIL(status))
    throw lisp_finish("exit called", 0);
  check(status, type::INTEGER);
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
inline constexpr auto NTHD = "nthd";       // return nth cdr of list
inline constexpr auto ATTACH = "attach";   // attach object at front of list
inline constexpr auto APPEND = "append";   // append lists
inline constexpr auto EXIT = "exit";       // exit lips
} // namespace pn

LISPT C_APPEND;
LISPT C_ERROR;
LISPT C_LAMBDA;
LISPT C_NLAMBDA;
LISPT C_QUOTE;

void prim::init()
{
  C_APPEND = intern(pn::APPEND);
  C_ERROR = intern(pn::ERROR);
  C_ERROR->settype(type::ERROR);
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
  mkprim(pn::NTHD,    nthd,    subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  // clang-format on
}

} // namespace lisp
