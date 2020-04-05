/*
 * Lips, lisp shell.
 * Copyright 1988, Krister Joas
 *
 * $Id$
 */
#include "lisp.h"

#ifndef lint
static char rcsid[] = "$Id$";
#endif

PRIMITIVE numberp(a)
  LISPT a;
{
  switch (TYPEOF(a))
    {
    case INTEGER:
    case FLOAT:
    case BIGNUM:
#ifdef FLOATING
    case LONG:
      return a;
#endif /* FLOATING */
    default:
      return C_NIL;
    }
}

PRIMITIVE listp(a)
  LISPT a;
{
  if (TYPEOF(a) == CONS)
    return a;
  else
    return C_NIL;
}

PRIMITIVE memb(x,l)
  LISPT x,l;
{
  while (!EQ(l,C_NIL))
    {
      if (EQ(x,CAR(l)))
        return l;
      l = CDR(l);
    }
  return C_NIL;
}

PRIMITIVE equal(l1,l2)
  LISPT l1,l2;
{
  LISPT x;

  if (TYPEOF(l1)!=TYPEOF(l2)) return C_NIL;
  if (EQ(l1,l2)) return C_T;
  switch (TYPEOF(l1))
    {
    case CONS:
      while (!EQ(l1,C_NIL) && !EQ(l2,C_NIL))
        {
          x = equal(CAR(l1),CAR(l2));
          if (EQ(x,C_T))
            {
              l1 = CDR(l1);
              l2 = CDR(l2);
            }
          else
            return C_NIL;
        } 
      return x;
    case STRING:
      return (!strcmp(STRINGVAL(l1),STRINGVAL(l2)))?C_T:C_NIL;
      break;
    case LAMBDA:
    case NLAMBDA:
      return funeq(l1, l2);
      break;
    default:
      break;
    }
  return C_NIL;
}

PRIMITIVE nlistp(a)
  LISPT a;
{
  if (TYPEOF(a) != CONS)
    return a;
  else
    return C_NIL;
}

PRIMITIVE neq(a,b)
  LISPT a,b;
{
  if (!EQ(a,b))
    return C_T;
  else
    return C_NIL;
}

PRIMITIVE boundp(a)
  LISPT a;
{
  if (TYPEOF(a) != SYMBOL)
    return C_NIL;
  else if (TYPEOF(SYMVAL(a).value) != UNBOUND)
    return C_T;
  else
    return C_NIL;
}

PRIMITIVE litatom(a)
  LISPT a;
{
  if (TYPEOF(a) == SYMBOL)
    return C_T;
  else
    return C_NIL;
}

PRIMITIVE xtypeof(a)
  LISPT a;
{
  switch(TYPEOF(a))
    {
    case NIL:
      return C_NIL;
    case SYMBOL:
      return C_SYMBOL;
    case INTEGER:
      return C_INTEGER;
    case BIGNUM:
      return C_BIGNUM;
    case FLOAT:
      return C_FLOAT;
    case INDIRECT:
      return C_INDIRECT;
    case CONS:
      return C_CONS;
    case STRING:
      return C_STRING;
    case SUBR:
      return C_SUBR;
    case FSUBR:
      return C_FSUBR;
    case LAMBDA:
      return C_LAMBDA;
    case NLAMBDA:
      return C_NLAMBDA;
    case CLOSURE:
      return C_CLOSURE;
    case UNBOUND:
      return C_UNBOUND;
    case ENVIRON:
      return C_ENVIRON;
    case TRUE:
      return C_T;
    case FREE:
      return C_FREE;
    case ENDOFFILE:
      return C_ENDOFFILE;
    case ERROR:
      return C_ERROR;
    case FILET:
      return C_FILE;
    }
  return C_NIL;
}

public void init_pred()
{
  mkprim(PN_LISTP,   listp,    1, SUBR);
  mkprim(PN_NLISTP,  nlistp,   1, SUBR);
  mkprim(PN_NEQ,     neq,      2, SUBR);
  mkprim(PN_NUMBERP, numberp,  1, SUBR);
  mkprim(PN_MEMB,    memb,     2, SUBR);
  mkprim(PN_EQUAL,   equal,    2, SUBR);
  mkprim(PN_BOUNDP,  boundp,   1, SUBR);
  mkprim(PN_LITATOM, litatom,  1, SUBR);
  mkprim(PN_TYPEOF, xtypeof,  1, SUBR);
}
