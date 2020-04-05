/*
 * Lips, lisp shell.
 * Copyright 1988, Krister Joas
 *
 * $Id$
 *
 */
#include "lisp.h"

#ifndef lint
static char rcsid[] = "$Id$";
#endif

/*
 * Function with an i as a prefix are for integer arithmetic and those
 * whith an f are for floats. Without a prefix, the functions
 * are generic and converts arguments to the apropriate type.
 * Any float as an argument to a generic function results in a
 * float.
 */

PRIMITIVE plus(l)
  LISPT l;
{
  double fsum = 0.0;
  long sum = 0;
  int f = 0;
  
  while (TYPEOF(l) == CONS)
    {
      if (f)
        {
          if (TYPEOF(CAR(l)) == INTEGER)
            fsum += (double) INTVAL(CAR(l));
          else if (TYPEOF(CAR(l)) == FLOAT)
            fsum += FLOATVAL(CAR(l));
          else
            return error(ILLEGAL_ARG, CAR(l));
        }
      else
        if (TYPEOF(CAR(l)) == INTEGER)
          sum += INTVAL(CAR(l));
        else if (TYPEOF(CAR(l)) == FLOAT)
          {
            f = 1;
            fsum = FLOATVAL(CAR(l)) + (double)sum;
          }
        else
          return error(ILLEGAL_ARG, CAR(l));
      l = CDR(l);
    }
  if (f)
    return mkfloat(fsum);
  else
    return mknumber(sum);
}

PRIMITIVE iplus(l)
  LISPT l;
{
  long sum;
  
  CHECK(CAR(l), INTEGER);
  sum = INTVAL(CAR(l));
  l = CDR(l);
  while (TYPEOF(l) == CONS)
    {
      CHECK(CAR(l), INTEGER);
      sum = sum + INTVAL(CAR(l));
      l = CDR(l);
    }
  return mknumber(sum);
}

PRIMITIVE fplus(l)
  LISPT l;
{
  double sum;
  
  CHECK(CAR(l), FLOAT);
  sum = FLOATVAL(CAR(l));
  l = CDR(l);
  while (TYPEOF(l) == CONS)
    {
      CHECK(CAR(l), FLOAT);
      sum = sum + FLOATVAL(CAR(l));
      l = CDR(l);
    }
  return mkfloat(sum);
}

PRIMITIVE difference(a, b)
  LISPT a, b;
{
  CHECK2(a, INTEGER, FLOAT);
  CHECK2(b, INTEGER, FLOAT);
  if (TYPEOF(a) == INTEGER)
    if (TYPEOF(b) == INTEGER)
      return mknumber(INTVAL(a) - INTVAL(b));
    else
      return mkfloat((double)INTVAL(a) - FLOATVAL(b));
  else
    if (TYPEOF(b) == INTEGER)
      return mkfloat(FLOATVAL(a) - (double)INTVAL(b));
    else
      return mkfloat(FLOATVAL(a) - FLOATVAL(b));
  /*NOTREACHED*/
}

PRIMITIVE idifference(a, b)
  LISPT a, b;
{
  CHECK(a, INTEGER);
  CHECK(b, INTEGER);
  return mknumber(INTVAL(a) - INTVAL(b));
}

PRIMITIVE fdifference(a, b)
  LISPT a, b;
{
  CHECK(a, FLOAT);
  CHECK(b, FLOAT);
  return mkfloat(FLOATVAL(a) - FLOATVAL(b));
}

PRIMITIVE ltimes(l)
  LISPT l;
{
  double fprod = 1.0;
  long prod = 1;
  int f = 0;
  
  while (TYPEOF(l) == CONS)
    {
      if (f)
        {
          if (TYPEOF(CAR(l)) == INTEGER)
            fprod *= (double) INTVAL(CAR(l));
          else if (TYPEOF(CAR(l)) == FLOAT)
            fprod *= FLOATVAL(CAR(l));
          else
            return error(ILLEGAL_ARG, CAR(l));
        }
      else
        if (TYPEOF(CAR(l)) == INTEGER)
          prod *= INTVAL(CAR(l));
        else if (TYPEOF(CAR(l)) == FLOAT)
          {
            f = 1;
            fprod = FLOATVAL(CAR(l)) * (double) prod;
          }
        else
          return error(ILLEGAL_ARG, CAR(l));
      l = CDR(l);
    }
  if (f)
    return mkfloat(fprod);
  else
    return mknumber(prod);
}

PRIMITIVE itimes(l)
  LISPT l;
{
  long prod;
  
  CHECK(CAR(l), INTEGER);
  prod = INTVAL(CAR(l));
  l = CDR(l);
  while (TYPEOF(l) == CONS)
    {
      CHECK(CAR(l), INTEGER);
      prod = prod * INTVAL(CAR(l));
      l = CDR(l);
    }
  return mknumber(prod);
}

PRIMITIVE ftimes(l)
  LISPT l;
{
  double prod;
  
  CHECK(CAR(l), FLOAT);
  prod = FLOATVAL(CAR(l));
  l = CDR(l);
  while (TYPEOF(l) == CONS)
    {
      CHECK(CAR(l), FLOAT);
      prod = prod * FLOATVAL(CAR(l));
      l = CDR(l);
    }
  return mkfloat(prod);
}
  
PRIMITIVE divide(a, b)
  LISPT a, b;
{
  CHECK2(a, INTEGER, FLOAT);
  CHECK2(b, INTEGER, FLOAT);
  if (TYPEOF(a) == INTEGER)
    if (TYPEOF(b) == INTEGER)
      return mknumber(INTVAL(a) / INTVAL(b));
    else
      return mkfloat((double)INTVAL(a) / FLOATVAL(b));
  else
    if (TYPEOF(b) == INTEGER)
      return mkfloat(FLOATVAL(a) / (double)INTVAL(b));
    else
      return mkfloat(FLOATVAL(a) / FLOATVAL(b));
  /*NOTREACHED*/
}

PRIMITIVE iquotient(a, b)
  LISPT a, b;
{
  CHECK(a, INTEGER);
  CHECK(b, INTEGER);
  if (INTVAL(b) == 0)
    return error(DIVIDE_ZERO, C_NIL);
  return mknumber(INTVAL(a) / INTVAL(b));
}

PRIMITIVE iremainder(a, b)
  LISPT a, b;
{
  CHECK(a, INTEGER);
  CHECK(b, INTEGER);
  if (INTVAL(b) == 0)
    return error(DIVIDE_ZERO, C_NIL);
  return mknumber(INTVAL(a) % INTVAL(b));
}

PRIMITIVE fdivide(a, b)
  LISPT a, b;
{
  CHECK(a, FLOAT);
  CHECK(b, FLOAT);
  if (FLOATVAL(b) == 0.0)
    return error(DIVIDE_ZERO, C_NIL);
  return mkfloat(FLOATVAL(a) / FLOATVAL(b));
}

PRIMITIVE minus(a)
  LISPT a;
{
  CHECK2(a, FLOAT, INTEGER);
  if (TYPEOF(a) == INTEGER)
    return mknumber(-INTVAL(a));
  else
    return mkfloat(-FLOATVAL(a));
}

PRIMITIVE iminus(a)
  LISPT a;
{
  CHECK(a, INTEGER);
  return mknumber(-INTVAL(a));
}

PRIMITIVE absval(i)
  LISPT i;
{
  int sign;

  CHECK(i, INTEGER);
  if (INTVAL(i) < 0) sign = -1;
  else sign = 1;
  return mknumber(INTVAL(i) * sign);
}

PRIMITIVE itof(i)
  LISPT i;
{
  CHECK(i, INTEGER);
  return mkfloat((double) INTVAL(i));
}

PRIMITIVE add1(a)
  LISPT a;
{
  CHECK(a, INTEGER);
  return mknumber(INTVAL(a) + 1);
}

PRIMITIVE sub1(a)
  LISPT a;
{
  CHECK(a, INTEGER);
  return mknumber(INTVAL(a) - 1);
}

#define FLOATFLOAT 0                    /* Both arguments are float */
#define FLOATINT   1                    /* One float and one int */
#define INTFLOAT   2                    /* One int and one float */
#define INTINT     3                    /* Both are ints */
#define ILLEGAL1   4                    /* First argument is illegal */
#define ILLEGAL2   5                    /* Second argument is illegal */

/* 
 * The result is one of the above constants depending on the types of the 
 * arguments.
 */
#define NUMTYPE(x, y) \
  (TYPEOF(x) == FLOAT) \
    ? ( (TYPEOF(y) == FLOAT) \
      ? FLOATFLOAT \
      : (TYPEOF(y) == INTEGER) ? FLOATINT : ILLEGAL2 ) \
    : (TYPEOF(x) == INTEGER) \
      ? ( (TYPEOF(y) == FLOAT)\
        ? INTFLOAT \
        : (TYPEOF(y) == INTEGER) ? INTINT : ILLEGAL2 ) \
      : ILLEGAL1

#define DOCHECK(x, y, cmp) \
  if (x cmp y) return C_T; \
  else return C_NIL; \

#define ILLEGALRETURN(a) \
  return error(ILLEGAL_ARG, a);

#define NUMCHECK(x, y, cmp) \
  switch(NUMTYPE(x, y)) \
    { \
    case FLOATFLOAT: \
      DOCHECK(FLOATVAL(x), FLOATVAL(y), cmp); \
    case FLOATINT: \
      DOCHECK(FLOATVAL(x), (double)INTVAL(y), cmp); \
    case INTFLOAT: \
      DOCHECK((double)INTVAL(x), FLOATVAL(y), cmp); \
    case INTINT: \
      DOCHECK(INTVAL(x), INTVAL(y), cmp); \
    case ILLEGAL1: \
      ILLEGALRETURN(x); \
    case ILLEGAL2: \
      ILLEGALRETURN(y); \
    default: \
      return error(BUG, C_NIL); \
    }

PRIMITIVE greaterp(x, y)
  LISPT x, y;
{
  NUMCHECK(x, y, >);
}

PRIMITIVE lessp(x, y)
  LISPT x, y;
{
  NUMCHECK(x, y, <);
}

PRIMITIVE eqp(x, y)
  LISPT x, y;
{
  NUMCHECK(x, y, ==);
}

PRIMITIVE geq(x, y)
  LISPT x, y;
{
  NUMCHECK(x, y, >=);
}

PRIMITIVE leq(x, y)
  LISPT x, y;
{
  NUMCHECK(x, y, <=);
}

PRIMITIVE neqp(x, y)
  LISPT x, y;
{
  NUMCHECK(x, y, !=);
}

PRIMITIVE zerop(x)
  LISPT x;
{
  if (TYPEOF(x) == INTEGER && INTVAL(x) == 0)
    return C_T;
  else
    return C_NIL;
}

PRIMITIVE minusp(x)
  LISPT x;
{
  if (TYPEOF(x) == FLOAT)
    if (FLOATVAL(x) < 0.0)
      return C_T;
    else
      return C_NIL;
  else if (TYPEOF(x) == INTEGER)
    if (INTVAL(x) < 0)
      return C_T;
    else
      return C_NIL;
  else
    return error(ILLEGAL_ARG, x);
}

public void
init_arith()
{
  mkprim(PN_PLUS,        plus,        -1, SUBR);
  mkprim(PN_DIFFERENCE,  difference,   2, SUBR);
  mkprim(PN_TIMES,       ltimes,      -1, SUBR);
  mkprim(PN_DIVIDE,      divide,       2, SUBR);
  mkprim(PN_IPLUS,       iplus,       -1, SUBR);
  mkprim(PN_IDIFFERENCE, idifference,  2, SUBR);
  mkprim(PN_ITIMES,      itimes,      -1, SUBR);
  mkprim(PN_IQUOTIENT,   iquotient,    2, SUBR);
  mkprim(PN_IREMAINDER,  iremainder,   2, SUBR);
  mkprim(PN_IMINUS,      iminus,       1, SUBR);
  mkprim(PN_MINUS,       minus,        1, SUBR);
  mkprim(PN_ADD1,        add1,         1, SUBR);
  mkprim(PN_SUB1,        sub1,         1, SUBR);
  mkprim(PN_FPLUS,       fplus,       -1, SUBR);
  mkprim(PN_FDIFFERENCE, fdifference,  2, SUBR);
  mkprim(PN_FTIMES,      ftimes,      -1, SUBR);
  mkprim(PN_FDIVIDE,     fdivide,      2, SUBR);
  mkprim(PN_ITOF,        itof,         1, SUBR);
  mkprim(PN_GREATERP,    greaterp,     2, SUBR);
  mkprim(PN_GEQ,         geq,          2, SUBR);
  mkprim(PN_LESSP,       lessp,        2, SUBR);
  mkprim(PN_LEQ,         leq,          2, SUBR);
  mkprim(PN_ZEROP,       zerop,        1, SUBR);
  mkprim(PN_EQP,         eqp,          2, SUBR);
  mkprim(PN_NEQP,        neqp,         2, SUBR);
  mkprim(PN_MINUSP,      minusp,       1, SUBR);
  mkprim(PN_ABS,         absval,       1, SUBR);
}
