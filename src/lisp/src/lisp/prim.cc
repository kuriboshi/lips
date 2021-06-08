/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include "libisp.hh"
#include "except.hh"

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
  LISPT iobj;

  /* If already an indirect type object, return it. */
  /* We want all symbols that we include in closures */
  /* on the same level to refer to the same value. */
  if(type_of(obj) == lisp_type::INDIRECT)
    return obj;
  else
  /* If it's a new object, cons up the storage for it */
  /* wasting the car part. */
  {
    iobj = cons(l, C_NIL, obj);
    iobj->settype(lisp_type::INDIRECT);
  }
  return iobj;
}

/* 
 * closobj - builds a list of indirect pointers to the values of the 
 *           symbols in the list VARS. Used to construct a closure.
 */
LISPT prim::closobj(LISPT vars)
{
  if(is_NIL(vars))
    return C_NIL;
  l.check(vars, lisp_type::CONS);
  l.check(vars->car(), lisp_type::SYMBOL);
  return cons(l, mkindirect(l, vars->car()->symval().value), closobj(vars->cdr()));
}

PRIMITIVE prim::car(LISPT a)
{
  if(type_of(a) == lisp_type::CONS)
    return a->car();
  return C_NIL;
}

PRIMITIVE prim::cdr(LISPT a)
{
  if(type_of(a) == lisp_type::CONS)
    return a->cdr();
  return C_NIL;
}

PRIMITIVE prim::cadr(LISPT a)
{
  if(type_of(a) == lisp_type::CONS)
    return car(a->cdr());
  return C_NIL;
}

PRIMITIVE prim::cdar(LISPT a)
{
  if(type_of(a) == lisp_type::CONS)
    return cdr(a->car());
  return C_NIL;
}

PRIMITIVE prim::caar(LISPT a)
{
  if(type_of(a) == lisp_type::CONS)
    return car(a->car());
  return C_NIL;
}

PRIMITIVE prim::cddr(LISPT a)
{
  if(type_of(a) == lisp_type::CONS)
    return cdr(a->cdr());
  return C_NIL;
}

PRIMITIVE prim::cdddr(LISPT a)
{
  if(type_of(a) == lisp_type::CONS)
    return cdr(cdr(a->cdr()));
  return C_NIL;
}

PRIMITIVE prim::caddr(LISPT a)
{
  if(type_of(a) == lisp_type::CONS)
    return car(cdr(a->cdr()));
  return C_NIL;
}

PRIMITIVE prim::cdadr(LISPT a)
{
  if(type_of(a) == lisp_type::CONS)
    return cdr(car(a->cdr()));
  return C_NIL;
}

PRIMITIVE prim::caadr(LISPT a)
{
  if(type_of(a) == lisp_type::CONS)
    return car(car(a->cdr()));
  return C_NIL;
}

PRIMITIVE prim::cddar(LISPT a)
{
  if(type_of(a) == lisp_type::CONS)
    return cdr(cdr(a->car()));
  return C_NIL;
}

PRIMITIVE prim::cadar(LISPT a)
{
  if(type_of(a) == lisp_type::CONS)
    return car(cdr(a->car()));
  return C_NIL;
}

PRIMITIVE prim::cdaar(LISPT a)
{
  if(type_of(a) == lisp_type::CONS)
    return cdr(car(a->car()));
  return C_NIL;
}

PRIMITIVE prim::caaar(LISPT a)
{
  if(type_of(a) == lisp_type::CONS)
    return car(car(a->car()));
  return C_NIL;
}

PRIMITIVE prim::rplaca(LISPT x, LISPT y)
{
  l.check(x, lisp_type::CONS);
  x->car(y);
  return x;
}

PRIMITIVE prim::rplacd(LISPT x, LISPT y)
{
  l.check(x, lisp_type::CONS);
  x->cdr(y);
  return x;
}

PRIMITIVE prim::eq(LISPT a, LISPT b)
{
  if(EQ(a, b))
    return C_T;
  return C_NIL;
}

PRIMITIVE prim::atom(LISPT a)
{
  if(is_NIL(a) || is_T(a) || type_of(a) == lisp_type::SYMBOL || type_of(a) == lisp_type::INTEGER || type_of(a) == lisp_type::FLOAT)
    return C_T;
  return C_NIL;
}

PRIMITIVE prim::nconc(LISPT x)
{
  LISPT cl;

  LISPT newl = C_NIL;
  LISPT curp = newl;
  for(; !is_NIL(x); x = x->cdr())
  {
    if(!is_NIL(x->car()))
    {
      l.check(x->car(), lisp_type::CONS);
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

PRIMITIVE prim::tconc(LISPT cell, LISPT obj)
{
  if(is_NIL(cell))
  {
    cell = cons(l, cons(l, obj, C_NIL), C_NIL);
    return rplacd(cell, cell->car());
  }
  l.check(cell, lisp_type::CONS);
  if(type_of(cell->car()) != lisp_type::CONS)
  {
    rplacd(cell, cons(l, obj, C_NIL));
    return rplaca(cell, cell->cdr());
  }
  rplacd(cell->cdr(), cons(l, obj, C_NIL));
  return rplacd(cell, cell->cdr()->cdr());
}

PRIMITIVE prim::attach(LISPT obj, LISPT list)
{
  if(is_NIL(list))
    return cons(l, obj, C_NIL);
  l.check(list, lisp_type::CONS);
  rplacd(list, cons(l, list->car(), list->cdr()));
  return rplaca(list, obj);
}

PRIMITIVE prim::append(LISPT x)
{
  LISPT cl;

  LISPT newl = cons(l, C_NIL, C_NIL);
  a.save(newl);
  LISPT curp = newl;
  for(; !is_NIL(x); x = x->cdr())
  {
    if(!is_NIL(x->car()))
    {
      l.check(x->car(), lisp_type::CONS);
      for(cl = x->car(); !is_NIL(cl); cl = cl->cdr())
      {
        rplacd(curp, cons(l, cl->car(), C_NIL));
        curp = curp->cdr();
      }
    }
  }
  newl = a.unsave();
  return newl->cdr();
}

PRIMITIVE prim::null(LISPT a)
{
  if(EQ(a, C_NIL))
    return C_T;
  return C_NIL;
}

PRIMITIVE prim::quote(LISPT x) { return x; }

PRIMITIVE prim::lambda(LISPT x, LISPT f) { return mklambda(l, x, f, lisp_type::LAMBDA); }

PRIMITIVE prim::nlambda(LISPT x, LISPT f) { return mklambda(l, x, f, lisp_type::NLAMBDA); }

PRIMITIVE prim::list(LISPT x) { return x; }

PRIMITIVE prim::length(LISPT x)
{
  int i = 0;
  while(!is_NIL(x) && type_of(x) == lisp_type::CONS)
  {
    x = x->cdr();
    i++;
  }
  return mknumber(l, i);
}

PRIMITIVE prim::closure(LISPT fun, LISPT vars)
{
  a.save(fun);
  a.save(vars);
  LISPT c = a.getobject();
  c->closval().cfunction = fun;
  c->closval().closed = vars;
  LISPT f = length(vars);
  c->closval().count = f->intval();
  f = closobj(vars);
  if(type_of(f) == lisp_type::ERROR)
    return f;
  c->closval().cvalues = f;
  LISPT clos = nullptr;
  set(clos, lisp_type::CLOSURE, c);
  return clos;
}

/*
 * nth - Return the N'th element in the list LIST. If N is greater than the
 *       length of LIST, return NIL.
 */
static LISPT nth(LISPT list, int n)
{
  LISPT l;

  for(l = list; n > 1 && !is_NIL(l); n--, l = l->cdr())
    ;
  if(!is_NIL(l))
    return l->car();
  else
    return C_NIL;
}

PRIMITIVE prim::xnth(LISPT x, LISPT p)
{
  l.check(p, lisp_type::INTEGER);
  if(is_NIL(x))
    return C_NIL;
  l.check(x, lisp_type::CONS);
  return nth(x, p->intval());
}

PRIMITIVE prim::nthd(LISPT list, LISPT pos)
{
  l.check(pos, lisp_type::INTEGER);
  int p = pos->intval();
  if(is_NIL(list))
    return C_NIL;
  l.check(list, lisp_type::CONS);
  LISPT l;
  for(l = list; type_of(l) == lisp_type::CONS && p > 1; l = l->cdr()) p--;
  return l;
}

PRIMITIVE prim::xerror(LISPT mess)
{
  l.check(mess, lisp_type::STRING);
  return l.error(USER_ERROR, mess);
}

PRIMITIVE prim::uxexit(LISPT status)
{
  if(is_NIL(status))
    throw lisp_finish("prim::uxexit called", 0);
  l.check(status, lisp_type::INTEGER);
  throw lisp_finish("prim::uxexit called", status->intval());
  return C_NIL;
}

inline constexpr auto PN_ATOM = "atom";       // t if atom
inline constexpr auto PN_CAR = "car";         // car
inline constexpr auto PN_CDR = "cdr";         // cdr
inline constexpr auto PN_CADR = "cadr";       // cadr
inline constexpr auto PN_CDAR = "cdar";       // cdar
inline constexpr auto PN_CAAR = "caar";       // caar
inline constexpr auto PN_CDDR = "cddr";       // cddr
inline constexpr auto PN_CDDDR = "cdddr";     // cdddr
inline constexpr auto PN_CADDR = "caddr";     // caddr
inline constexpr auto PN_CDADR = "cdadr";     // cdadr
inline constexpr auto PN_CAADR = "caadr";     // caadr
inline constexpr auto PN_CDDAR = "cddar";     // cddar
inline constexpr auto PN_CADAR = "cadar";     // cadar
inline constexpr auto PN_CDAAR = "cdaar";     // cdaar
inline constexpr auto PN_CAAAR = "caaar";     // caaar
inline constexpr auto PN_CLOSURE = "closure"; // create static environment
inline constexpr auto PN_EQ = "eq";           // pointer equal
inline constexpr auto PN_ERROR = "error";     // error
inline constexpr auto PN_LAMBDA = "lambda";   // create lambda object
inline constexpr auto PN_LENGTH = "length";   // length of list
inline constexpr auto PN_LIST = "list";       // make list of args
inline constexpr auto PN_NCONC = "nconc";     // destructive append
inline constexpr auto PN_NLAMBDA = "nlambda"; // make nlambda object
inline constexpr auto PN_NTH = "nth";         // nth car in list
inline constexpr auto PN_NULL = "null";       // t if nil
inline constexpr auto PN_QUOTE = "quote";     // don't eval arg
inline constexpr auto PN_RPLACA = "rplaca";   // replace car
inline constexpr auto PN_RPLACD = "rplacd";   // replace cdr
inline constexpr auto PN_TCONC = "tconc";     // add to end of list
inline constexpr auto PN_NTHD = "nthd";       // return nth cdr of list
inline constexpr auto PN_ATTACH = "attach";   // attach object at front of list
inline constexpr auto PN_APPEND = "append";   // append lists
inline constexpr auto PN_EXIT = "exit";       // exit lips

LISPT C_APPEND;
LISPT C_ERROR;
LISPT C_LAMBDA;
LISPT C_NLAMBDA;
LISPT C_QUOTE;

void prim::init()
{
  C_APPEND = intern(PN_APPEND);
  C_ERROR = intern(PN_ERROR);
  C_ERROR->type = lisp_type::ERROR;
  C_LAMBDA = intern(PN_LAMBDA);
  C_NLAMBDA = intern(PN_NLAMBDA);
  C_QUOTE = intern(PN_QUOTE);

  // clang-format off
  mkprim(PN_ATOM,    ::lisp::atom,    subr_t::S_EVAL,   subr_t::S_NOSPREAD);
  mkprim(PN_ATTACH,  ::lisp::attach,  subr_t::S_EVAL,   subr_t::S_NOSPREAD);
  mkprim(PN_APPEND,  ::lisp::append,  subr_t::S_EVAL,   subr_t::S_SPREAD);
  mkprim(PN_CAR,     ::lisp::car,     subr_t::S_EVAL,   subr_t::S_NOSPREAD);
  mkprim(PN_CDR,     ::lisp::cdr,     subr_t::S_EVAL,   subr_t::S_NOSPREAD);
  mkprim(PN_CADR,    ::lisp::cadr,    subr_t::S_EVAL,   subr_t::S_NOSPREAD);
  mkprim(PN_CDAR,    ::lisp::cdar,    subr_t::S_EVAL,   subr_t::S_NOSPREAD);
  mkprim(PN_CAAR,    ::lisp::caar,    subr_t::S_EVAL,   subr_t::S_NOSPREAD);
  mkprim(PN_CDDR,    ::lisp::cddr,    subr_t::S_EVAL,   subr_t::S_NOSPREAD);
  mkprim(PN_CDDDR,   ::lisp::cdddr,   subr_t::S_EVAL,   subr_t::S_NOSPREAD);
  mkprim(PN_CADDR,   ::lisp::caddr,   subr_t::S_EVAL,   subr_t::S_NOSPREAD);
  mkprim(PN_CDADR,   ::lisp::cdadr,   subr_t::S_EVAL,   subr_t::S_NOSPREAD);
  mkprim(PN_CAADR,   ::lisp::caadr,   subr_t::S_EVAL,   subr_t::S_NOSPREAD);
  mkprim(PN_CDDAR,   ::lisp::cddar,   subr_t::S_EVAL,   subr_t::S_NOSPREAD);
  mkprim(PN_CADAR,   ::lisp::cadar,   subr_t::S_EVAL,   subr_t::S_NOSPREAD);
  mkprim(PN_CDAAR,   ::lisp::cdaar,   subr_t::S_EVAL,   subr_t::S_NOSPREAD);
  mkprim(PN_CAAAR,   ::lisp::caaar,   subr_t::S_EVAL,   subr_t::S_NOSPREAD);
  mkprim(PN_CLOSURE, ::lisp::closure, subr_t::S_EVAL,   subr_t::S_NOSPREAD);
  mkprim(PN_EQ,      ::lisp::eq,      subr_t::S_EVAL,   subr_t::S_NOSPREAD);
  mkprim(PN_ERROR,   ::lisp::xerror,  subr_t::S_EVAL,   subr_t::S_SPREAD);
  mkprim(PN_EXIT,    ::lisp::uxexit,  subr_t::S_EVAL,   subr_t::S_NOSPREAD);
  mkprim(PN_LAMBDA,  ::lisp::lambda,  subr_t::S_NOEVAL, subr_t::S_SPREAD);
  mkprim(PN_LENGTH,  ::lisp::length,  subr_t::S_EVAL,   subr_t::S_NOSPREAD);
  mkprim(PN_LIST,    ::lisp::list,    subr_t::S_EVAL,   subr_t::S_SPREAD);
  mkprim(PN_NCONC,   ::lisp::nconc,   subr_t::S_EVAL,   subr_t::S_SPREAD);
  mkprim(PN_NLAMBDA, ::lisp::nlambda, subr_t::S_NOEVAL, subr_t::S_SPREAD);
  mkprim(PN_NTH,     ::lisp::xnth,    subr_t::S_EVAL,   subr_t::S_NOSPREAD);
  mkprim(PN_NULL,    ::lisp::null,    subr_t::S_EVAL,   subr_t::S_NOSPREAD);
  mkprim(PN_QUOTE,   ::lisp::quote,   subr_t::S_NOEVAL, subr_t::S_NOSPREAD);
  mkprim(PN_RPLACA,  ::lisp::rplaca,  subr_t::S_EVAL,   subr_t::S_NOSPREAD);
  mkprim(PN_RPLACD,  ::lisp::rplacd,  subr_t::S_EVAL,   subr_t::S_NOSPREAD);
  mkprim(PN_TCONC,   ::lisp::tconc,   subr_t::S_EVAL,   subr_t::S_NOSPREAD);
  mkprim(PN_NTHD,    ::lisp::nthd,    subr_t::S_EVAL,   subr_t::S_NOSPREAD);
  // clang-format on
}

} // namespace lisp
