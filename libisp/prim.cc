/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include "libisp.hh"

extern void finish(int);

namespace lisp
{
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
  if(type_of(obj) == INDIRECT)
    return obj;
  else
  /* If it's a new object, cons up the storage for it */
  /* wasting the car part. */
  {
    iobj = cons(l, C_NIL, obj);
    iobj->settype(INDIRECT);
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
  check(vars, CONS);
  check(vars->car(), SYMBOL);
  return cons(_lisp, mkindirect(_lisp, vars->car()->symval().value), closobj(vars->cdr()));
}

PRIMITIVE prim::car(LISPT a)
{
  if(type_of(a) == CONS)
    return a->car();
  return C_NIL;
}

PRIMITIVE prim::cdr(LISPT a)
{
  if(type_of(a) == CONS)
    return a->cdr();
  return C_NIL;
}

PRIMITIVE prim::cadr(LISPT a)
{
  if(type_of(a) == CONS)
    return car(a->cdr());
  return C_NIL;
}

PRIMITIVE prim::cdar(LISPT a)
{
  if(type_of(a) == CONS)
    return cdr(a->car());
  return C_NIL;
}

PRIMITIVE prim::caar(LISPT a)
{
  if(type_of(a) == CONS)
    return car(a->car());
  return C_NIL;
}

PRIMITIVE prim::cddr(LISPT a)
{
  if(type_of(a) == CONS)
    return cdr(a->cdr());
  return C_NIL;
}

PRIMITIVE prim::cdddr(LISPT a)
{
  if(type_of(a) == CONS)
    return cdr(cdr(a->cdr()));
  return C_NIL;
}

PRIMITIVE prim::caddr(LISPT a)
{
  if(type_of(a) == CONS)
    return car(cdr(a->cdr()));
  return C_NIL;
}

PRIMITIVE prim::cdadr(LISPT a)
{
  if(type_of(a) == CONS)
    return cdr(car(a->cdr()));
  return C_NIL;
}

PRIMITIVE prim::caadr(LISPT a)
{
  if(type_of(a) == CONS)
    return car(car(a->cdr()));
  return C_NIL;
}

PRIMITIVE prim::cddar(LISPT a)
{
  if(type_of(a) == CONS)
    return cdr(cdr(a->car()));
  return C_NIL;
}

PRIMITIVE prim::cadar(LISPT a)
{
  if(type_of(a) == CONS)
    return car(cdr(a->car()));
  return C_NIL;
}

PRIMITIVE prim::cdaar(LISPT a)
{
  if(type_of(a) == CONS)
    return cdr(car(a->car()));
  return C_NIL;
}

PRIMITIVE prim::caaar(LISPT a)
{
  if(type_of(a) == CONS)
    return car(car(a->car()));
  return C_NIL;
}

PRIMITIVE prim::rplaca(LISPT x, LISPT y)
{
  check(x, CONS);
  x->car(y);
  return x;
}

PRIMITIVE prim::rplacd(LISPT x, LISPT y)
{
  check(x, CONS);
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
  if(is_NIL(a) || is_T(a) || type_of(a) == SYMBOL || type_of(a) == INTEGER || type_of(a) == FLOAT)
    return C_T;
  return C_NIL;
}

PRIMITIVE prim::nconc(LISPT l)
{
  LISPT cl;

  LISPT newl = C_NIL;
  LISPT curp = newl;
  for(; !is_NIL(l); l = l->cdr())
  {
    if(!is_NIL(l->car()))
    {
      check(l->car(), CONS);
      if(is_NIL(curp))
      {
        curp = l->car();
        newl = curp;
      }
      else
        rplacd(curp, l->car());
      for(cl = l->car(); !is_NIL(cl->cdr()); cl = cl->cdr())
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
    cell = cons(_lisp, cons(_lisp, obj, C_NIL), C_NIL);
    return rplacd(cell, cell->car());
  }
  check(cell, CONS);
  if(type_of(cell->car()) != CONS)
  {
    rplacd(cell, cons(_lisp, obj, C_NIL));
    return rplaca(cell, cell->cdr());
  }
  rplacd(cell->cdr(), cons(_lisp, obj, C_NIL));
  return rplacd(cell, cell->cdr()->cdr());
}

PRIMITIVE prim::attach(LISPT obj, LISPT list)
{
  if(is_NIL(list))
    return cons(_lisp, obj, C_NIL);
  check(list, CONS);
  rplacd(list, cons(_lisp, list->car(), list->cdr()));
  return rplaca(list, obj);
}

PRIMITIVE prim::append(LISPT l)
{
  LISPT cl;

  LISPT newl = cons(_lisp, C_NIL, C_NIL);
  a().save(newl);
  LISPT curp = newl;
  for(; !is_NIL(l); l = l->cdr())
  {
    if(!is_NIL(l->car()))
    {
      check(l->car(), CONS);
      for(cl = l->car(); !is_NIL(cl); cl = cl->cdr())
      {
        rplacd(curp, cons(_lisp, cl->car(), C_NIL));
        curp = curp->cdr();
      }
    }
  }
  newl = a().unsave();
  return newl->cdr();
}

PRIMITIVE prim::null(LISPT a)
{
  if(EQ(a, C_NIL))
    return C_T;
  return C_NIL;
}

PRIMITIVE prim::quote(LISPT a) { return a; }

PRIMITIVE prim::lambda(LISPT x, LISPT f) { return a().mklambda(x, f, LAMBDA); }

PRIMITIVE prim::nlambda(LISPT x, LISPT f) { return a().mklambda(x, f, NLAMBDA); }

PRIMITIVE prim::list(LISPT l) { return l; }

PRIMITIVE prim::length(LISPT l)
{
  int i = 0;
  while(!is_NIL(l) && type_of(l) == CONS)
  {
    l = l->cdr();
    i++;
  }
  return a().mknumber(i);
}

PRIMITIVE prim::closure(LISPT fun, LISPT vars)
{
  a().save(fun);
  a().save(vars);
  LISPT c = a().getobject();
  c->closval().cfunction = fun;
  c->closval().closed = vars;
  LISPT f = length(vars);
  c->closval().count = f->intval();
  f = closobj(vars);
  if(type_of(f) == ERROR)
    return f;
  c->closval().cvalues = f;
  LISPT clos = nullptr;
  set(clos, CLOSURE, c);
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

PRIMITIVE prim::xnth(LISPT l, LISPT p)
{
  check(p, INTEGER);
  if(is_NIL(l))
    return C_NIL;
  check(l, CONS);
  return nth(l, p->intval());
}

PRIMITIVE prim::nthd(LISPT list, LISPT pos)
{
  check(pos, INTEGER);
  int p = pos->intval();
  if(is_NIL(list))
    return C_NIL;
  check(list, CONS);
  LISPT l;
  for(l = list; type_of(l) == CONS && p > 1; l = l->cdr()) p--;
  return l;
}

PRIMITIVE prim::xerror(LISPT mess)
{
  check(mess, STRING);
  return error(USER_ERROR, mess);
}

PRIMITIVE prim::uxexit(LISPT status)
{
#if 0
  if(is_NIL(status))
    finish(0);
  check(status, INTEGER);
  finish(status->intval());
#endif
  return C_NIL;
}

prim::prim(lisp& lisp) : _lisp(lisp)
{
  a().mkprim(PN_ATOM, ::lisp::atom, 1, SUBR);
  a().mkprim(PN_ATTACH, ::lisp::attach, 2, SUBR);
  a().mkprim(PN_APPEND, ::lisp::append, -1, SUBR);
  a().mkprim(PN_CAR, ::lisp::car, 1, SUBR);
  a().mkprim(PN_CDR, ::lisp::cdr, 1, SUBR);
  a().mkprim(PN_CADR, ::lisp::cadr, 1, SUBR);
  a().mkprim(PN_CDAR, ::lisp::cdar, 1, SUBR);
  a().mkprim(PN_CAAR, ::lisp::caar, 1, SUBR);
  a().mkprim(PN_CDDR, ::lisp::cddr, 1, SUBR);
  a().mkprim(PN_CDDDR, ::lisp::cdddr, 1, SUBR);
  a().mkprim(PN_CADDR, ::lisp::caddr, 1, SUBR);
  a().mkprim(PN_CDADR, ::lisp::cdadr, 1, SUBR);
  a().mkprim(PN_CAADR, ::lisp::caadr, 1, SUBR);
  a().mkprim(PN_CDDAR, ::lisp::cddar, 1, SUBR);
  a().mkprim(PN_CADAR, ::lisp::cadar, 1, SUBR);
  a().mkprim(PN_CDAAR, ::lisp::cdaar, 1, SUBR);
  a().mkprim(PN_CAAAR, ::lisp::caaar, 1, SUBR);
  a().mkprim(PN_CLOSURE, ::lisp::closure, 2, SUBR);
  a().mkprim(PN_EQ, ::lisp::eq, 2, SUBR);
  a().mkprim(PN_ERROR, ::lisp::xerror, -1, SUBR);
  a().mkprim(PN_EXIT, ::lisp::uxexit, 1, SUBR);
  a().mkprim(PN_LAMBDA, ::lisp::lambda, -2, FSUBR);
  a().mkprim(PN_LENGTH, ::lisp::length, 1, SUBR);
  a().mkprim(PN_LIST, ::lisp::list, -1, SUBR);
  a().mkprim(PN_NCONC, ::lisp::nconc, -1, SUBR);
  a().mkprim(PN_NLAMBDA, ::lisp::nlambda, -2, FSUBR);
  a().mkprim(PN_NTH, ::lisp::xnth, 2, SUBR);
  a().mkprim(PN_NULL, ::lisp::null, 1, SUBR);
  a().mkprim(PN_QUOTE, ::lisp::quote, 1, FSUBR);
  a().mkprim(PN_RPLACA, ::lisp::rplaca, 2, SUBR);
  a().mkprim(PN_RPLACD, ::lisp::rplacd, 2, SUBR);
  a().mkprim(PN_TCONC, ::lisp::tconc, 2, SUBR);
  a().mkprim(PN_NTHD, ::lisp::nthd, 2, SUBR);
}

} // namespace lisp
