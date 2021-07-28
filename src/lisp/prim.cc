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
prim::prim(): base() {}
prim::prim(lisp& lisp): base(lisp) {}

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
LISPT prim::closobj(LISPT vars)
{
  if(is_NIL(vars))
    return NIL;
  l.check(vars, type::CONS);
  l.check(vars->car(), type::SYMBOL);
  return cons(l, mkindirect(l, vars->car()->symvalue()), closobj(vars->cdr()));
}

LISPT prim::car(LISPT a)
{
  if(type_of(a) == type::CONS)
    return a->car();
  return NIL;
}

LISPT prim::cdr(LISPT a)
{
  if(type_of(a) == type::CONS)
    return a->cdr();
  return NIL;
}

LISPT prim::cadr(LISPT a)
{
  if(type_of(a) == type::CONS)
    return car(a->cdr());
  return NIL;
}

LISPT prim::cdar(LISPT a)
{
  if(type_of(a) == type::CONS)
    return cdr(a->car());
  return NIL;
}

LISPT prim::caar(LISPT a)
{
  if(type_of(a) == type::CONS)
    return car(a->car());
  return NIL;
}

LISPT prim::cddr(LISPT a)
{
  if(type_of(a) == type::CONS)
    return cdr(a->cdr());
  return NIL;
}

LISPT prim::cdddr(LISPT a)
{
  if(type_of(a) == type::CONS)
    return cdr(cdr(a->cdr()));
  return NIL;
}

LISPT prim::caddr(LISPT a)
{
  if(type_of(a) == type::CONS)
    return car(cdr(a->cdr()));
  return NIL;
}

LISPT prim::cdadr(LISPT a)
{
  if(type_of(a) == type::CONS)
    return cdr(car(a->cdr()));
  return NIL;
}

LISPT prim::caadr(LISPT a)
{
  if(type_of(a) == type::CONS)
    return car(car(a->cdr()));
  return NIL;
}

LISPT prim::cddar(LISPT a)
{
  if(type_of(a) == type::CONS)
    return cdr(cdr(a->car()));
  return NIL;
}

LISPT prim::cadar(LISPT a)
{
  if(type_of(a) == type::CONS)
    return car(cdr(a->car()));
  return NIL;
}

LISPT prim::cdaar(LISPT a)
{
  if(type_of(a) == type::CONS)
    return cdr(car(a->car()));
  return NIL;
}

LISPT prim::caaar(LISPT a)
{
  if(type_of(a) == type::CONS)
    return car(car(a->car()));
  return NIL;
}

LISPT prim::rplaca(LISPT x, LISPT y)
{
  l.check(x, type::CONS);
  x->car(y);
  return x;
}

LISPT prim::rplacd(LISPT x, LISPT y)
{
  l.check(x, type::CONS);
  x->cdr(y);
  return x;
}

LISPT prim::eq(LISPT a, LISPT b)
{
  if(EQ(a, b))
    return T;
  if(type_of(a) == type::INTEGER && type_of(b) == type::INTEGER
    && a->intval() == b->intval())
    return T;
  return NIL;
}

LISPT prim::atom(LISPT a)
{
  if(is_NIL(a) || is_T(a) || type_of(a) == type::SYMBOL || type_of(a) == type::INTEGER || type_of(a) == type::FLOAT)
    return T;
  return NIL;
}

LISPT prim::nconc(LISPT x)
{
  LISPT cl;

  LISPT newl = NIL;
  LISPT curp = newl;
  for(; !is_NIL(x); x = x->cdr())
  {
    if(!is_NIL(x->car()))
    {
      l.check(x->car(), type::CONS);
      if(is_NIL(curp))
      {
        curp = x->car();
        newl = curp;
      }
      else
        rplacd(curp, x->car());
      for(cl = x->car(); !is_NIL(cl->cdr()); cl = cl->cdr())
        ;
      curp = cl;
    }
  }
  return newl;
}

LISPT prim::tconc(LISPT cell, LISPT obj)
{
  if(is_NIL(cell))
  {
    cell = cons(l, cons(l, obj, NIL), NIL);
    return rplacd(cell, cell->car());
  }
  l.check(cell, type::CONS);
  if(type_of(cell->car()) != type::CONS)
  {
    rplacd(cell, cons(l, obj, NIL));
    return rplaca(cell, cell->cdr());
  }
  rplacd(cell->cdr(), cons(l, obj, NIL));
  return rplacd(cell, cell->cdr()->cdr());
}

LISPT prim::attach(LISPT obj, LISPT list)
{
  if(is_NIL(list))
    return cons(l, obj, NIL);
  l.check(list, type::CONS);
  rplacd(list, cons(l, list->car(), list->cdr()));
  return rplaca(list, obj);
}

LISPT prim::append(LISPT x)
{
  LISPT cl;

  LISPT newl = cons(l, NIL, NIL);
  LISPT curp = newl;
  for(; !is_NIL(x); x = x->cdr())
  {
    if(!is_NIL(x->car()))
    {
      l.check(x->car(), type::CONS);
      for(cl = x->car(); !is_NIL(cl); cl = cl->cdr())
      {
        rplacd(curp, cons(l, cl->car(), NIL));
        curp = curp->cdr();
      }
    }
  }
  return newl->cdr();
}

LISPT prim::null(LISPT a)
{
  if(EQ(a, NIL))
    return T;
  return NIL;
}

LISPT prim::quote(LISPT x) { return x; }

LISPT prim::lambda(LISPT x, LISPT f) { return mklambda(l, x, f, type::LAMBDA); }

LISPT prim::nlambda(LISPT x, LISPT f) { return mklambda(l, x, f, type::NLAMBDA); }

LISPT prim::list(LISPT x) { return x; }

LISPT prim::length(LISPT x)
{
  int i = 0;
  while(!is_NIL(x) && type_of(x) == type::CONS)
  {
    x = x->cdr();
    i++;
  }
  return mknumber(l, i);
}

LISPT prim::closure(LISPT fun, LISPT vars)
{
  closure_t c;
  c.cfunction = fun;
  c.closed = vars;
  auto f = length(vars);
  c.count = f->intval();
  f = closobj(vars);
  if(type_of(f) == type::ERROR)
    return f;
  c.cvalues = f;
  auto clos = a.getobject();
  clos->set(c);
  return clos;
}

inline LISPT _nth(LISPT list, int n)
{
  LISPT l;

  for(l = list; n > 1 && !is_NIL(l); n--, l = l->cdr())
    ;
  if(!is_NIL(l))
    return l->car();
  else
    return NIL;
}

LISPT prim::nth(LISPT x, LISPT p)
{
  l.check(p, type::INTEGER);
  if(is_NIL(x))
    return NIL;
  l.check(x, type::CONS);
  return _nth(x, p->intval());
}

LISPT prim::nthd(LISPT list, LISPT pos)
{
  l.check(pos, type::INTEGER);
  int p = pos->intval();
  if(is_NIL(list))
    return NIL;
  l.check(list, type::CONS);
  LISPT l;
  for(l = list; type_of(l) == type::CONS && p > 1; l = l->cdr()) p--;
  return l;
}

LISPT prim::error(LISPT mess)
{
  l.check(mess, type::STRING);
  return l.error(USER_ERROR, mess);
}

LISPT prim::uxexit(LISPT status)
{
  if(is_NIL(status))
    throw lisp_finish("exit called", 0);
  l.check(status, type::INTEGER);
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
  mkprim(pn::ATOM,    ::lisp::atom,    subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::ATTACH,  ::lisp::attach,  subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::APPEND,  ::lisp::append,  subr_t::subr::EVAL,   subr_t::spread::NOSPREAD);
  mkprim(pn::CAR,     ::lisp::car,     subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::CDR,     ::lisp::cdr,     subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::CADR,    ::lisp::cadr,    subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::CDAR,    ::lisp::cdar,    subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::CAAR,    ::lisp::caar,    subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::CDDR,    ::lisp::cddr,    subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::CDDDR,   ::lisp::cdddr,   subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::CADDR,   ::lisp::caddr,   subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::CDADR,   ::lisp::cdadr,   subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::CAADR,   ::lisp::caadr,   subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::CDDAR,   ::lisp::cddar,   subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::CADAR,   ::lisp::cadar,   subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::CDAAR,   ::lisp::cdaar,   subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::CAAAR,   ::lisp::caaar,   subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::CLOSURE, ::lisp::closure, subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::EQ,      ::lisp::eq,      subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::ERROR,   ::lisp::error,   subr_t::subr::EVAL,   subr_t::spread::NOSPREAD);
  mkprim(pn::EXIT,    ::lisp::uxexit,  subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::LAMBDA,  ::lisp::lambda,  subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::LENGTH,  ::lisp::length,  subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::LIST,    ::lisp::list,    subr_t::subr::EVAL,   subr_t::spread::NOSPREAD);
  mkprim(pn::NCONC,   ::lisp::nconc,   subr_t::subr::EVAL,   subr_t::spread::NOSPREAD);
  mkprim(pn::NLAMBDA, ::lisp::nlambda, subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::NTH,     ::lisp::nth,     subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::NULL_,   ::lisp::null,    subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::QUOTE,   ::lisp::quote,   subr_t::subr::NOEVAL, subr_t::spread::SPREAD);
  mkprim(pn::RPLACA,  ::lisp::rplaca,  subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::RPLACD,  ::lisp::rplacd,  subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::TCONC,   ::lisp::tconc,   subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::NTHD,    ::lisp::nthd,    subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  // clang-format on
}

} // namespace lisp
