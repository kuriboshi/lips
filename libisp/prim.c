/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 * $Id$
 */
#include "lisp.h"

#ifndef lint
static char rcsid[] = "$Id$";
#endif

extern void finish();

USESAVE

static int count;

/*
 * mkprim - Define the primitive with print name PNAME, to be the
 *          C function FNAME with NRPAR number of parameters. TYPE
 *          is the type of function: SUBR or FSUBR. If npar is negative
 *          it means the function is halfspread.
 */
void
mkprim(pname, fname, nrpar, type)
  char *pname;
  LISPT (*fname)();
  short nrpar;
  char type;
{
  LISPT s;
  LISPT f;
  
  f = intern(pname);
  s = getobject ();
  SUBRVAL(s).function = fname;
  SUBRVAL(s).argcount = nrpar;
  SET(SYMVAL(f).value, type, s);
}

/* 
 * nth - Return the N'th element in the list LIST. If N is greater than 
 *       the length of LIST,  return NIL.
 */
LISPT
nth(list, n)
  LISPT list;
  long n;
{
  LISPT l;

  for (l = list; n > 1 && !ISNIL(l); n--, l = CDR(l));
  if (!ISNIL(l))
    return CAR(l);
  else
    return C_NIL;
}

/* 
 * mkindirect - makes an indirect pointer to the object OBJ. If already an 
 *              indirect object,  return it.
 */
static LISPT
mkindirect(obj)
  LISPT obj;
{
  LISPT iobj;

  /* If already an indirect type object, return it. */
  /* We want all symbols that we include in closures */
  /* on the same level to refer to the same value. */
  if (TYPEOF(obj) == INDIRECT)
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
LISPT
closobj(vars)
  register LISPT vars;
{
  if (ISNIL(vars))
    return C_NIL;
  CHECK(vars, CONS);
  CHECK(CAR(vars), SYMBOL);
  return cons(mkindirect(SYMVAL(CAR(vars)).value),
              closobj(CDR(vars)));
}

/* 
 * mkarglist - builds a list out of the argument list ALIST given in a 
 *             lambda definition. This list may end in an atom if the 
 *             function is halfspread, or it could be an atom for a 
 *             nospread function. COUNT is set to the number of arguments
 *             and is negative if halfspread or nospread.
 */
static LISPT
mkarglis(alist)
  LISPT alist;
{
  if (TYPEOF(alist) == CONS)
    {
      count++;
      return cons(CAR(alist), mkarglis(CDR(alist)));
    }
  else if(EQ(alist, C_NIL))
    return C_NIL;
  else
    {
      count = - (count + 1);
      return cons(alist, C_NIL);
    }
}

/* 
 * mklambda - Make a lambda object with the argument ARGS and definition 
 *            DEF and the type TYPE,  wich is LAMBDA or NLAMBDA.
 */
LISPT
mklambda(args, def, type)
  LISPT args, def;
  int type;
{
  register LISPT t;
  register LISPT s;

  SAVE(args);
  SAVE(def);
  s = getobject ();
  LAMVAL(s).lambdarep = def;
  count = 0;
  LAMVAL(s).arglist = mkarglis(args);
  LAMVAL(s).argcnt = count;
  SET(t, type, s);
  UNSAVE(def);
  UNSAVE(args);
  return t;
}

PRIMITIVE car(a)
  LISPT a;
{
  if (TYPEOF(a) == CONS)
    return CAR(a);
  else
    return C_NIL;
}

PRIMITIVE cdr(a)
  LISPT a;
{
  if (TYPEOF(a) == CONS)
    return CDR(a);
  else
    return C_NIL;
}

PRIMITIVE cadr(a)
  LISPT a;
{
  if (TYPEOF(a) == CONS)
    return car(CDR(a));
  else
    return C_NIL;
}

PRIMITIVE cdar(a)
  LISPT a;
{
  if (TYPEOF(a) == CONS)
    return cdr(CAR(a));
  else
    return C_NIL;
}

PRIMITIVE caar(a)
  LISPT a;
{
  if (TYPEOF(a) == CONS)
    return car(CAR(a));
  else
    return C_NIL;
}

PRIMITIVE cddr(a)
  LISPT a;
{
  if (TYPEOF(a) == CONS)
    return cdr(CDR(a));
  else
    return C_NIL;
}

PRIMITIVE cdddr(a)
  LISPT a;
{
  if (TYPEOF(a) == CONS)
    return cdr(cdr(CDR(a)));
  else
    return C_NIL;
}

PRIMITIVE caddr(a)
  LISPT a;
{
  if (TYPEOF(a) == CONS)
    return car(cdr(CDR(a)));
  else
    return C_NIL;
}

PRIMITIVE cdadr(a)
  LISPT a;
{
  if (TYPEOF(a) == CONS)
    return cdr(car(CDR(a)));
  else
    return C_NIL;
}

PRIMITIVE caadr(a)
  LISPT a;
{
  if (TYPEOF(a) == CONS)
    return car(car(CDR(a)));
  else
    return C_NIL;
}

PRIMITIVE cddar(a)
  LISPT a;
{
  if (TYPEOF(a) == CONS)
    return cdr(cdr(CAR(a)));
  else
    return C_NIL;
}

PRIMITIVE cadar(a)
  LISPT a;
{
  if (TYPEOF(a) == CONS)
    return car(cdr(CAR(a)));
  else
    return C_NIL;
}

PRIMITIVE cdaar(a)
  LISPT a;
{
  if (TYPEOF(a) == CONS)
    return cdr(car(CAR(a)));
  else
    return C_NIL;
}

PRIMITIVE caaar(a)
  LISPT a;
{
  if (TYPEOF(a) == CONS)
    return car(car(CAR(a)));
  else
    return C_NIL;
}

PRIMITIVE rplaca(x, y)
  LISPT x, y;
{
  CHECK(x, CONS);
  CAR(x) = y;
  return x;
}

PRIMITIVE rplacd(x, y)
  LISPT x, y;
{
  CHECK(x, CONS);
  CDR(x) = y;
  return x;
}

PRIMITIVE eq(a, b)
  LISPT a, b;
{
  if (EQ(a, b))
    return C_T;
  else
    return C_NIL;
}

PRIMITIVE atom(a)
  LISPT a;
{
  if (ISNIL(a) || IST(a) || TYPEOF(a) == SYMBOL
      || TYPEOF(a) == INTEGER || TYPEOF(a) == FLOAT)
    return C_T;
  else
    return C_NIL;
}

PRIMITIVE nconc(l)
  LISPT l;
{
  LISPT newl, curp;
  LISPT cl;

  newl = C_NIL;
  curp = newl;
  for (; !ISNIL(l); l = CDR(l))
    {
      if (!ISNIL(CAR(l)))
        {
          CHECK(CAR(l), CONS);
          if (ISNIL(curp))
            {
              curp = CAR(l);
              newl = curp;
            }
          else
            (void) rplacd(curp,CAR(l));
          for (cl = CAR(l); !ISNIL(CDR(cl)); cl = CDR(cl))
            ;
          curp = cl;
        }
    }
  return newl;
}

PRIMITIVE tconc(cell, obj)
  LISPT cell, obj;
{
  if (ISNIL(cell))
    {
      cell = cons(cons(obj, C_NIL), C_NIL);
      return rplacd(cell, CAR(cell));
    }
  CHECK(cell, CONS);
  if (TYPEOF(CAR(cell)) != CONS)
    {
      (void) rplacd(cell, cons(obj, C_NIL));
      return rplaca(cell, CDR(cell));
    }
  (void) rplacd(CDR(cell), cons(obj, C_NIL));
  return rplacd(cell, CDR(CDR(cell)));
}

PRIMITIVE attach(obj, list)
  LISPT obj, list;
{
  if (ISNIL(list)) return cons(obj, C_NIL);
  CHECK(list, CONS);
  (void) rplacd(list, cons(CAR(list), CDR(list)));
  return rplaca(list, obj);
}

PRIMITIVE append(l)
  LISPT l;
{
  LISPT newl, curp;
  LISPT cl;

  newl = cons(C_NIL,C_NIL);
  SAVE(newl);
  curp = newl;
  for (; !ISNIL(l); l = CDR(l))
    {
      if (!ISNIL(CAR(l)))
        {
          CHECK(CAR(l),CONS);
          for (cl = CAR(l); !ISNIL(cl); cl = CDR(cl))
            {
              (void) rplacd(curp,cons(CAR(cl),C_NIL));
              curp = CDR(curp);
            }
        }
    }
  UNSAVE(newl);
  return CDR(newl);
}

PRIMITIVE null(a)
  LISPT a;
{
  if (EQ(a,C_NIL))
    return C_T;
  else
    return C_NIL;
}

PRIMITIVE quote(a)
  LISPT a;
{
  return a;
}

PRIMITIVE lambda(a,f)
  LISPT a,f;
{
  return mklambda(a,f,LAMBDA);
}  

PRIMITIVE nlambda(a,f)
  LISPT a,f;
{
  return mklambda(a,f,NLAMBDA);
}

PRIMITIVE list(l)
  LISPT l;
{
  return l;
}

PRIMITIVE length(l)
  LISPT l;
{
  int i;
  
  i = 0;
  while (!ISNIL(l) && TYPEOF(l) == CONS)
    {
      l = CDR(l);
      i++;
    }
  return mknumber((long)i);
}

PRIMITIVE closure(fun,vars)
  LISPT fun,vars;
{
  LISPT clos;
  LISPT f;
  LISPT c;

  SAVE(fun);
  SAVE(vars);
  c = getobject ();
  CLOSVAL(c).cfunction = fun;
  CLOSVAL(c).closed = vars;
  f = length(vars);
  CLOSVAL(c).count = INTVAL(f);
  f = closobj(vars);
  if (TYPEOF(f) == ERROR)
    return f;
  CLOSVAL(c).cvalues = f;
  SET(clos, CLOSURE, c);
  return clos;
}

PRIMITIVE xnth(l, p)
  LISPT l, p;
{
  CHECK(p, INTEGER);
  if (ISNIL(l))
    return C_NIL;
  CHECK(l, CONS);
  return nth(l, INTVAL(p));
}

PRIMITIVE nthd(list, pos)
  LISPT list, pos;
{
  LISPT l;
  int p;

  CHECK(pos, INTEGER);
  p = INTVAL(pos);
  if (ISNIL(list))
    return C_NIL;
  CHECK(list, CONS);
  for (l = list; TYPEOF(l) == CONS && p > 1; l = CDR(l))
    p--;
  return l;
}

PRIMITIVE xerror(mess)
  LISPT mess;
{
  CHECK(mess, STRING);
  return error(USER_ERROR, mess);
}

PRIMITIVE uxexit(status)
  LISPT status;
{
  if (ISNIL(status))
    finish(0);
  CHECK(status,INTEGER);
  finish((int)INTVAL(status));
  return C_NIL;
}

void
init_prim()
{
  mkprim(PN_ATOM,    atom,     1, SUBR);
  mkprim(PN_ATTACH,  attach,   2, SUBR);
  mkprim(PN_APPEND,  append,  -1, SUBR);
  mkprim(PN_CAR,     car,      1, SUBR);
  mkprim(PN_CDR,     cdr,      1, SUBR);
  mkprim(PN_CADR,    cadr,     1, SUBR);
  mkprim(PN_CDAR,    cdar,     1, SUBR);
  mkprim(PN_CAAR,    caar,     1, SUBR);
  mkprim(PN_CDDR,    cddr,     1, SUBR);
  mkprim(PN_CDDDR,   cdddr,    1, SUBR);
  mkprim(PN_CADDR,   caddr,    1, SUBR);
  mkprim(PN_CDADR,   cdadr,    1, SUBR);
  mkprim(PN_CAADR,   caadr,    1, SUBR);
  mkprim(PN_CDDAR,   cddar,    1, SUBR);
  mkprim(PN_CADAR,   cadar,    1, SUBR);
  mkprim(PN_CDAAR,   cdaar,    1, SUBR);
  mkprim(PN_CAAAR,   caaar,    1, SUBR);
  mkprim(PN_CLOSURE, closure,  2, SUBR);
  mkprim(PN_EQ,      eq,       2, SUBR);
  mkprim(PN_ERROR,   xerror,  -1, SUBR);
  mkprim(PN_EXIT,    uxexit,   1, SUBR);
  mkprim(PN_LAMBDA,  lambda,  -2, FSUBR);
  mkprim(PN_LENGTH,  length,   1, SUBR);
  mkprim(PN_LIST,    list,    -1, SUBR);
  mkprim(PN_NCONC,   nconc,   -1, SUBR);
  mkprim(PN_NLAMBDA, nlambda, -2, FSUBR);
  mkprim(PN_NTH,     xnth,     2, SUBR);
  mkprim(PN_NULL,    null,     1, SUBR);
  mkprim(PN_QUOTE,   quote,    1, FSUBR);
  mkprim(PN_RPLACA,  rplaca,   2, SUBR);
  mkprim(PN_RPLACD,  rplacd,   2, SUBR);
  mkprim(PN_TCONC,   tconc,    2, SUBR);
  mkprim(PN_NTHD,    nthd,     2, SUBR);
}
