/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 * $Id$
 */

#include "libisp.hh"

extern void finish(int);

USESAVE

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
  SUBRVAL(s).argcount = nrpar;
  SET(SYMVAL(f).value, type, s);
  return s;
}

void mkprim(const char* pname, LISPT (*fname)(void), short nrpar, lisp_type type)
{
  SUBRVAL(mkprim_(pname, nrpar, type)).function0 = fname;
}

void mkprim(const char* pname, LISPT (*fname)(LISPT), short nrpar, lisp_type type)
{
  SUBRVAL(mkprim_(pname, nrpar, type)).function1 = fname;
}

void mkprim(const char* pname, LISPT (*fname)(LISPT, LISPT), short nrpar, lisp_type type)
{
  SUBRVAL(mkprim_(pname, nrpar, type)).function2 = fname;
}

void mkprim(const char* pname, LISPT (*fname)(LISPT, LISPT, LISPT), short nrpar, lisp_type type)
{
  SUBRVAL(mkprim_(pname, nrpar, type)).function3 = fname;
}

/* 
 * nth - Return the N'th element in the list LIST. If N is greater than 
 *       the length of LIST,  return NIL.
 */
LISPT nth(LISPT list, long n)
{
  LISPT l;

  for(l = list; n > 1 && !ISNIL(l); n--, l = CDR(l))
    ;
  if(!ISNIL(l))
    return CAR(l);
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
  CHECK(vars, CONS);
  CHECK(CAR(vars), SYMBOL);
  return cons(mkindirect(SYMVAL(CAR(vars)).value), closobj(CDR(vars)));
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
    return cons(CAR(alist), mkarglis(CDR(alist)));
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
  LISPT t;
  LISPT s;

  SAVE(args);
  SAVE(def);
  s = getobject();
  LAMVAL(s).lambdarep = def;
  count = 0;
  LAMVAL(s).arglist = mkarglis(args);
  LAMVAL(s).argcnt = count;
  SET(t, type, s);
  UNSAVE(def);
  UNSAVE(args);
  return t;
}

PRIMITIVE car(LISPT a)
{
  if(TYPEOF(a) == CONS)
    return CAR(a);
  else
    return C_NIL;
}

PRIMITIVE cdr(LISPT a)
{
  if(TYPEOF(a) == CONS)
    return CDR(a);
  else
    return C_NIL;
}

PRIMITIVE cadr(LISPT a)
{
  if(TYPEOF(a) == CONS)
    return car(CDR(a));
  else
    return C_NIL;
}

PRIMITIVE cdar(LISPT a)
{
  if(TYPEOF(a) == CONS)
    return cdr(CAR(a));
  else
    return C_NIL;
}

PRIMITIVE caar(LISPT a)
{
  if(TYPEOF(a) == CONS)
    return car(CAR(a));
  else
    return C_NIL;
}

PRIMITIVE cddr(LISPT a)
{
  if(TYPEOF(a) == CONS)
    return cdr(CDR(a));
  else
    return C_NIL;
}

PRIMITIVE cdddr(LISPT a)
{
  if(TYPEOF(a) == CONS)
    return cdr(cdr(CDR(a)));
  else
    return C_NIL;
}

PRIMITIVE caddr(LISPT a)
{
  if(TYPEOF(a) == CONS)
    return car(cdr(CDR(a)));
  else
    return C_NIL;
}

PRIMITIVE cdadr(LISPT a)
{
  if(TYPEOF(a) == CONS)
    return cdr(car(CDR(a)));
  else
    return C_NIL;
}

PRIMITIVE caadr(LISPT a)
{
  if(TYPEOF(a) == CONS)
    return car(car(CDR(a)));
  else
    return C_NIL;
}

PRIMITIVE cddar(LISPT a)
{
  if(TYPEOF(a) == CONS)
    return cdr(cdr(CAR(a)));
  else
    return C_NIL;
}

PRIMITIVE cadar(LISPT a)
{
  if(TYPEOF(a) == CONS)
    return car(cdr(CAR(a)));
  else
    return C_NIL;
}

PRIMITIVE cdaar(LISPT a)
{
  if(TYPEOF(a) == CONS)
    return cdr(car(CAR(a)));
  else
    return C_NIL;
}

PRIMITIVE caaar(LISPT a)
{
  if(TYPEOF(a) == CONS)
    return car(car(CAR(a)));
  else
    return C_NIL;
}

PRIMITIVE rplaca(LISPT x, LISPT y)
{
  CHECK(x, CONS);
  CAR(x) = y;
  return x;
}

PRIMITIVE rplacd(LISPT x, LISPT y)
{
  CHECK(x, CONS);
  CDR(x) = y;
  return x;
}

PRIMITIVE eq(LISPT a, LISPT b)
{
  if(EQ(a, b))
    return C_T;
  else
    return C_NIL;
}

PRIMITIVE atom(LISPT a)
{
  if(ISNIL(a) || IST(a) || TYPEOF(a) == SYMBOL || TYPEOF(a) == INTEGER || TYPEOF(a) == FLOAT)
    return C_T;
  else
    return C_NIL;
}

PRIMITIVE nconc(LISPT l)
{
  LISPT cl;

  LISPT newl = C_NIL;
  LISPT curp = newl;
  for(; !ISNIL(l); l = CDR(l))
  {
    if(!ISNIL(CAR(l)))
    {
      CHECK(CAR(l), CONS);
      if(ISNIL(curp))
      {
        curp = CAR(l);
        newl = curp;
      }
      else
        rplacd(curp, CAR(l));
      for(cl = CAR(l); !ISNIL(CDR(cl)); cl = CDR(cl))
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
    return rplacd(cell, CAR(cell));
  }
  CHECK(cell, CONS);
  if(TYPEOF(CAR(cell)) != CONS)
  {
    rplacd(cell, cons(obj, C_NIL));
    return rplaca(cell, CDR(cell));
  }
  rplacd(CDR(cell), cons(obj, C_NIL));
  return rplacd(cell, CDR(CDR(cell)));
}

PRIMITIVE attach(LISPT obj, LISPT list)
{
  if(ISNIL(list))
    return cons(obj, C_NIL);
  CHECK(list, CONS);
  rplacd(list, cons(CAR(list), CDR(list)));
  return rplaca(list, obj);
}

PRIMITIVE append(LISPT l)
{
  LISPT cl;

  LISPT newl = cons(C_NIL, C_NIL);
  SAVE(newl);
  LISPT curp = newl;
  for(; !ISNIL(l); l = CDR(l))
  {
    if(!ISNIL(CAR(l)))
    {
      CHECK(CAR(l), CONS);
      for(cl = CAR(l); !ISNIL(cl); cl = CDR(cl))
      {
        rplacd(curp, cons(CAR(cl), C_NIL));
        curp = CDR(curp);
      }
    }
  }
  UNSAVE(newl);
  return CDR(newl);
}

PRIMITIVE null(LISPT a)
{
  if(EQ(a, C_NIL))
    return C_T;
  else
    return C_NIL;
}

PRIMITIVE quote(LISPT a)
{
  return a;
}

PRIMITIVE lambda(LISPT a, LISPT f)
{
  return mklambda(a, f, LAMBDA);
}

PRIMITIVE nlambda(LISPT a, LISPT f)
{
  return mklambda(a, f, NLAMBDA);
}

PRIMITIVE list(LISPT l)
{
  return l;
}

PRIMITIVE length(LISPT l)
{
  int i = 0;
  while(!ISNIL(l) && TYPEOF(l) == CONS)
  {
    l = CDR(l);
    i++;
  }
  return mknumber((long)i);
}

PRIMITIVE closure(LISPT fun, LISPT vars)
{
  SAVE(fun);
  SAVE(vars);
  LISPT c = getobject();
  CLOSVAL(c).cfunction = fun;
  CLOSVAL(c).closed = vars;
  LISPT f = length(vars);
  CLOSVAL(c).count = INTVAL(f);
  f = closobj(vars);
  if(TYPEOF(f) == ERROR)
    return f;
  CLOSVAL(c).cvalues = f;
  LISPT clos;
  SET(clos, CLOSURE, c);
  return clos;
}

PRIMITIVE xnth(LISPT l, LISPT p)
{
  CHECK(p, INTEGER);
  if(ISNIL(l))
    return C_NIL;
  CHECK(l, CONS);
  return nth(l, INTVAL(p));
}

PRIMITIVE nthd(LISPT list, LISPT pos)
{
  CHECK(pos, INTEGER);
  int p = INTVAL(pos);
  if(ISNIL(list))
    return C_NIL;
  CHECK(list, CONS);
  LISPT l;
  for(l = list; TYPEOF(l) == CONS && p > 1; l = CDR(l)) p--;
  return l;
}

PRIMITIVE xerror(LISPT mess)
{
  CHECK(mess, STRING);
  return error(USER_ERROR, mess);
}

PRIMITIVE uxexit(LISPT status)
{
  if(ISNIL(status))
    finish(0);
  CHECK(status, INTEGER);
  finish((int)INTVAL(status));
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
