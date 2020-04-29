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
static LISPT mkindirect(LISPT obj)
{
  LISPT iobj;

  /* If already an indirect type object, return it. */
  /* We want all symbols that we include in closures */
  /* on the same level to refer to the same value. */
  if(TYPEOF(obj) == INDIRECT)
    return obj;
  else
  /* If it's a new object, cons up the storage for it */
  /* wasting the car part. */
  {
    iobj = cons(C_NIL, obj);
    SETTYPE(iobj, INDIRECT);
  }
  return iobj;
}

/* 
 * closobj - builds a list of indirect pointers to the values of the 
 *           symbols in the list VARS. Used to construct a closure.
 */
LISPT closobj(LISPT vars)
{
  if(ISNIL(vars))
    return C_NIL;
  check(vars, CONS);
  check(vars->car(), SYMBOL);
  return cons(mkindirect(vars->car()->symval().value), closobj(vars->cdr()));
}

PRIMITIVE car(LISPT a)
{
  if(TYPEOF(a) == CONS)
    return a->car();
  return C_NIL;
}

PRIMITIVE cdr(LISPT a)
{
  if(TYPEOF(a) == CONS)
    return a->cdr();
  return C_NIL;
}

PRIMITIVE cadr(LISPT a)
{
  if(TYPEOF(a) == CONS)
    return car(a->cdr());
  return C_NIL;
}

PRIMITIVE cdar(LISPT a)
{
  if(TYPEOF(a) == CONS)
    return cdr(a->car());
  return C_NIL;
}

PRIMITIVE caar(LISPT a)
{
  if(TYPEOF(a) == CONS)
    return car(a->car());
  return C_NIL;
}

PRIMITIVE cddr(LISPT a)
{
  if(TYPEOF(a) == CONS)
    return cdr(a->cdr());
  return C_NIL;
}

PRIMITIVE cdddr(LISPT a)
{
  if(TYPEOF(a) == CONS)
    return cdr(cdr(a->cdr()));
  return C_NIL;
}

PRIMITIVE caddr(LISPT a)
{
  if(TYPEOF(a) == CONS)
    return car(cdr(a->cdr()));
  return C_NIL;
}

PRIMITIVE cdadr(LISPT a)
{
  if(TYPEOF(a) == CONS)
    return cdr(car(a->cdr()));
  return C_NIL;
}

PRIMITIVE caadr(LISPT a)
{
  if(TYPEOF(a) == CONS)
    return car(car(a->cdr()));
  return C_NIL;
}

PRIMITIVE cddar(LISPT a)
{
  if(TYPEOF(a) == CONS)
    return cdr(cdr(a->car()));
  return C_NIL;
}

PRIMITIVE cadar(LISPT a)
{
  if(TYPEOF(a) == CONS)
    return car(cdr(a->car()));
  return C_NIL;
}

PRIMITIVE cdaar(LISPT a)
{
  if(TYPEOF(a) == CONS)
    return cdr(car(a->car()));
  return C_NIL;
}

PRIMITIVE caaar(LISPT a)
{
  if(TYPEOF(a) == CONS)
    return car(car(a->car()));
  return C_NIL;
}

PRIMITIVE rplaca(LISPT x, LISPT y)
{
  check(x, CONS);
  x->car(y);
  return x;
}

PRIMITIVE rplacd(LISPT x, LISPT y)
{
  check(x, CONS);
  x->cdr(y);
  return x;
}

PRIMITIVE eq(LISPT a, LISPT b)
{
  if(EQ(a, b))
    return C_T;
  return C_NIL;
}

PRIMITIVE atom(LISPT a)
{
  if(ISNIL(a) || IST(a) || TYPEOF(a) == SYMBOL || TYPEOF(a) == INTEGER || TYPEOF(a) == FLOAT)
    return C_T;
  return C_NIL;
}

PRIMITIVE nconc(LISPT l)
{
  LISPT cl;

  LISPT newl = C_NIL;
  LISPT curp = newl;
  for(; !ISNIL(l); l = l->cdr())
  {
    if(!ISNIL(l->car()))
    {
      check(l->car(), CONS);
      if(ISNIL(curp))
      {
        curp = l->car();
        newl = curp;
      }
      else
        rplacd(curp, l->car());
      for(cl = l->car(); !ISNIL(cl->cdr()); cl = cl->cdr())
        ;
      curp = cl;
    }
  }
  return newl;
}

PRIMITIVE tconc(LISPT cell, LISPT obj)
{
  if(ISNIL(cell))
  {
    cell = cons(cons(obj, C_NIL), C_NIL);
    return rplacd(cell, cell->car());
  }
  check(cell, CONS);
  if(TYPEOF(cell->car()) != CONS)
  {
    rplacd(cell, cons(obj, C_NIL));
    return rplaca(cell, cell->cdr());
  }
  rplacd(cell->cdr(), cons(obj, C_NIL));
  return rplacd(cell, cell->cdr()->cdr());
}

PRIMITIVE attach(LISPT obj, LISPT list)
{
  if(ISNIL(list))
    return cons(obj, C_NIL);
  check(list, CONS);
  rplacd(list, cons(list->car(), list->cdr()));
  return rplaca(list, obj);
}

PRIMITIVE append(LISPT l)
{
  LISPT cl;

  LISPT newl = cons(C_NIL, C_NIL);
  save(newl);
  LISPT curp = newl;
  for(; !ISNIL(l); l = l->cdr())
  {
    if(!ISNIL(l->car()))
    {
      check(l->car(), CONS);
      for(cl = l->car(); !ISNIL(cl); cl = cl->cdr())
      {
        rplacd(curp, cons(cl->car(), C_NIL));
        curp = curp->cdr();
      }
    }
  }
  lisp::unsave(newl);
  return newl->cdr();
}

PRIMITIVE null(LISPT a)
{
  if(EQ(a, C_NIL))
    return C_T;
  return C_NIL;
}

PRIMITIVE quote(LISPT a) { return a; }

PRIMITIVE lambda(LISPT a, LISPT f) { return alloc::mklambda(a, f, LAMBDA); }

PRIMITIVE nlambda(LISPT a, LISPT f) { return alloc::mklambda(a, f, NLAMBDA); }

PRIMITIVE list(LISPT l) { return l; }

PRIMITIVE length(LISPT l)
{
  int i = 0;
  while(!ISNIL(l) && TYPEOF(l) == CONS)
  {
    l = l->cdr();
    i++;
  }
  return mknumber(i);
}

PRIMITIVE closure(LISPT fun, LISPT vars)
{
  save(fun);
  save(vars);
  LISPT c = getobject();
  c->closval().cfunction = fun;
  c->closval().closed = vars;
  LISPT f = length(vars);
  c->closval().count = f->intval();
  f = closobj(vars);
  if(TYPEOF(f) == ERROR)
    return f;
  c->closval().cvalues = f;
  LISPT clos = nullptr;
  SET(clos, CLOSURE, c);
  return clos;
}

/*
 * nth - Return the N'th element in the list LIST. If N is greater than the
 *       length of LIST, return NIL.
 */
static LISPT nth(LISPT list, int n)
{
  LISPT l;

  for(l = list; n > 1 && !ISNIL(l); n--, l = l->cdr())
    ;
  if(!ISNIL(l))
    return l->car();
  else
    return C_NIL;
}

PRIMITIVE xnth(LISPT l, LISPT p)
{
  check(p, INTEGER);
  if(ISNIL(l))
    return C_NIL;
  check(l, CONS);
  return nth(l, p->intval());
}

PRIMITIVE nthd(LISPT list, LISPT pos)
{
  check(pos, INTEGER);
  int p = pos->intval();
  if(ISNIL(list))
    return C_NIL;
  check(list, CONS);
  LISPT l;
  for(l = list; TYPEOF(l) == CONS && p > 1; l = l->cdr()) p--;
  return l;
}

PRIMITIVE xerror(LISPT mess)
{
  check(mess, STRING);
  return error(USER_ERROR, mess);
}

PRIMITIVE uxexit(LISPT status)
{
  if(ISNIL(status))
    finish(0);
  check(status, INTEGER);
  finish(status->intval());
  return C_NIL;
}

prim::prim()
{
  mkprim(PN_ATOM, atom, 1, SUBR);
  mkprim(PN_ATTACH, attach, 2, SUBR);
  mkprim(PN_APPEND, append, -1, SUBR);
  mkprim(PN_CAR, car, 1, SUBR);
  mkprim(PN_CDR, cdr, 1, SUBR);
  mkprim(PN_CADR, cadr, 1, SUBR);
  mkprim(PN_CDAR, cdar, 1, SUBR);
  mkprim(PN_CAAR, caar, 1, SUBR);
  mkprim(PN_CDDR, cddr, 1, SUBR);
  mkprim(PN_CDDDR, cdddr, 1, SUBR);
  mkprim(PN_CADDR, caddr, 1, SUBR);
  mkprim(PN_CDADR, cdadr, 1, SUBR);
  mkprim(PN_CAADR, caadr, 1, SUBR);
  mkprim(PN_CDDAR, cddar, 1, SUBR);
  mkprim(PN_CADAR, cadar, 1, SUBR);
  mkprim(PN_CDAAR, cdaar, 1, SUBR);
  mkprim(PN_CAAAR, caaar, 1, SUBR);
  mkprim(PN_CLOSURE, closure, 2, SUBR);
  mkprim(PN_EQ, eq, 2, SUBR);
  mkprim(PN_ERROR, xerror, -1, SUBR);
  mkprim(PN_EXIT, uxexit, 1, SUBR);
  mkprim(PN_LAMBDA, lambda, -2, FSUBR);
  mkprim(PN_LENGTH, length, 1, SUBR);
  mkprim(PN_LIST, list, -1, SUBR);
  mkprim(PN_NCONC, nconc, -1, SUBR);
  mkprim(PN_NLAMBDA, nlambda, -2, FSUBR);
  mkprim(PN_NTH, xnth, 2, SUBR);
  mkprim(PN_NULL, null, 1, SUBR);
  mkprim(PN_QUOTE, quote, 1, FSUBR);
  mkprim(PN_RPLACA, rplaca, 2, SUBR);
  mkprim(PN_RPLACD, rplacd, 2, SUBR);
  mkprim(PN_TCONC, tconc, 2, SUBR);
  mkprim(PN_NTHD, nthd, 2, SUBR);
}

} // namespace lisp
