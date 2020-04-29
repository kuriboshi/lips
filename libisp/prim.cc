/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 * $Id$
 */

#include "libisp.hh"

extern void finish(int);

namespace lisp
{
static int count;

/*
 * mkprim - Define the primitive with print name PNAME, to be the
 *          C function FNAME with NRPAR number of parameters. TYPE
 *          is the type of function: SUBR or FSUBR. If npar is negative
 *          it means the function is halfspread.
 */
static LISPT mkprim_(const char* pname, short nrpar, lisp_type type)
{
  LISPT s = getobject();
  LISPT f = intern(pname);
  s->subrval().argcount = nrpar;
  SET(f->symval().value, type, s);
  return s;
}

void mkprim(const char* pname, LISPT (*fname)(), short nrpar, lisp_type type)
{
  mkprim_(pname, nrpar, type)->subrval().function0 = fname;
}

void mkprim(const char* pname, LISPT (*fname)(LISPT), short nrpar, lisp_type type)
{
  mkprim_(pname, nrpar, type)->subrval().function1 = fname;
}

void mkprim(const char* pname, LISPT (*fname)(LISPT, LISPT), short nrpar, lisp_type type)
{
  mkprim_(pname, nrpar, type)->subrval().function2 = fname;
}

void mkprim(const char* pname, LISPT (*fname)(LISPT, LISPT, LISPT), short nrpar, lisp_type type)
{
  mkprim_(pname, nrpar, type)->subrval().function3 = fname;
}

/* 
 * nth - Return the N'th element in the list LIST. If N is greater than 
 *       the length of LIST,  return NIL.
 */
LISPT nth(LISPT list, int n)
{
  LISPT l;

  for(l = list; n > 1 && !ISNIL(l); n--, l = l->cdr())
    ;
  if(!ISNIL(l))
    return l->car();
  else
    return C_NIL;
}

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

/* 
 * mkarglist - builds a list out of the argument list ALIST given in a 
 *             lambda definition. This list may end in an atom if the 
 *             function is halfspread, or it could be an atom for a 
 *             nospread function. COUNT is set to the number of arguments
 *             and is negative if halfspread or nospread.
 */
static LISPT mkarglis(LISPT alist)
{
  if(TYPEOF(alist) == CONS)
  {
    count++;
    return cons(alist->car(), mkarglis(alist->cdr()));
  }
  else if(EQ(alist, C_NIL))
    return C_NIL;
  else
  {
    count = -(count + 1);
    return cons(alist, C_NIL);
  }
}

/* 
 * mklambda - Make a lambda object with the argument ARGS and definition 
 *            DEF and the type TYPE,  wich is LAMBDA or NLAMBDA.
 */
LISPT mklambda(LISPT args, LISPT def, lisp_type type)
{
  SAVE(args);
  SAVE(def);
  LISPT s = getobject();
  s->lamval().lambdarep = def;
  count = 0;
  s->lamval().arglist = mkarglis(args);
  s->lamval().argcnt = count;
  LISPT t = nullptr;
  SET(t, type, s);
  UNSAVE(def);
  UNSAVE(args);
  return t;
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
  SAVE(newl);
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
  UNSAVE(newl);
  return newl->cdr();
}

PRIMITIVE null(LISPT a)
{
  if(EQ(a, C_NIL))
    return C_T;
  return C_NIL;
}

PRIMITIVE quote(LISPT a) { return a; }

PRIMITIVE lambda(LISPT a, LISPT f) { return mklambda(a, f, LAMBDA); }

PRIMITIVE nlambda(LISPT a, LISPT f) { return mklambda(a, f, NLAMBDA); }

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
  SAVE(fun);
  SAVE(vars);
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
  finish((int)status->intval());
  return C_NIL;
}

void init_prim()
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
