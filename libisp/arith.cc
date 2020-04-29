/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include "libisp.hh"

namespace lisp
{
/*
 * Function with an i as a prefix are for integer arithmetic and those
 * whith an f are for floats. Without a prefix, the functions
 * are generic and converts arguments to the apropriate type.
 * Any float as an argument to a generic function results in a
 * float.
 */

PRIMITIVE plus(LISPT l)
{
  double fsum = 0.0;
  int sum = 0;
  int f = 0;

  while(TYPEOF(l) == CONS)
  {
    if(f)
    {
      if(TYPEOF(l->car()) == INTEGER)
        fsum += (double)l->car()->intval();
      else if(TYPEOF(l->car()) == FLOAT)
        fsum += l->car()->floatval();
      else
        return error(ILLEGAL_ARG, l->car());
    }
    else if(TYPEOF(l->car()) == INTEGER)
      sum += l->car()->intval();
    else if(TYPEOF(l->car()) == FLOAT)
    {
      f = 1;
      fsum = l->car()->floatval() + (double)sum;
    }
    else
      return error(ILLEGAL_ARG, l->car());
    l = l->cdr();
  }
  if(f)
    return mkfloat(fsum);
  return mknumber(sum);
}

PRIMITIVE iplus(LISPT l)
{
  int sum = 0;
  for(auto i = lisp::begin(l); i != lisp::end(l); ++i)
  {
    check(*i, INTEGER);
    sum += (**i).intval();
  }
  return mknumber(sum);
}

PRIMITIVE fplus(LISPT l)
{
  check(l->car(), FLOAT);
  auto sum = l->car()->floatval();
  l = l->cdr();
  while(TYPEOF(l) == CONS)
  {
    check(l->car(), FLOAT);
    sum = sum + l->car()->floatval();
    l = l->cdr();
  }
  return mkfloat(sum);
}

PRIMITIVE difference(LISPT a, LISPT b)
{
  check2(a, INTEGER, FLOAT);
  check2(b, INTEGER, FLOAT);
  if(TYPEOF(a) == INTEGER)
    if(TYPEOF(b) == INTEGER)
      return mknumber(a->intval() - b->intval());
    else
      return mkfloat((double)a->intval() - b->floatval());
  else if(TYPEOF(b) == INTEGER)
    return mkfloat(a->floatval() - (double)b->intval());
  return mkfloat(a->floatval() - b->floatval());
}

PRIMITIVE idifference(LISPT a, LISPT b)
{
  check(a, INTEGER);
  check(b, INTEGER);
  return mknumber(a->intval() - b->intval());
}

PRIMITIVE fdifference(LISPT a, LISPT b)
{
  check(a, FLOAT);
  check(b, FLOAT);
  return mkfloat(a->floatval() - b->floatval());
}

PRIMITIVE ltimes(LISPT l)
{
  double fprod = 1.0;
  int prod = 1;
  int f = 0;

  while(TYPEOF(l) == CONS)
  {
    if(f)
    {
      if(TYPEOF(l->car()) == INTEGER)
        fprod *= (double)l->car()->intval();
      else if(TYPEOF(l->car()) == FLOAT)
        fprod *= l->car()->floatval();
      else
        return error(ILLEGAL_ARG, l->car());
    }
    else if(TYPEOF(l->car()) == INTEGER)
      prod *= l->car()->intval();
    else if(TYPEOF(l->car()) == FLOAT)
    {
      f = 1;
      fprod = l->car()->floatval() * (double)prod;
    }
    else
      return error(ILLEGAL_ARG, l->car());
    l = l->cdr();
  }
  if(f)
    return mkfloat(fprod);
  return mknumber(prod);
}

PRIMITIVE itimes(LISPT l)
{
  check(l->car(), INTEGER);
  auto prod = l->car()->intval();
  l = l->cdr();
  while(TYPEOF(l) == CONS)
  {
    check(l->car(), INTEGER);
    prod = prod * l->car()->intval();
    l = l->cdr();
  }
  return mknumber(prod);
}

PRIMITIVE ftimes(LISPT l)
{
  check(l->car(), FLOAT);
  auto prod = l->car()->floatval();
  l = l->cdr();
  while(TYPEOF(l) == CONS)
  {
    check(l->car(), FLOAT);
    prod = prod * l->car()->floatval();
    l = l->cdr();
  }
  return mkfloat(prod);
}

PRIMITIVE divide(LISPT a, LISPT b)
{
  check2(a, INTEGER, FLOAT);
  check2(b, INTEGER, FLOAT);
  if(TYPEOF(a) == INTEGER)
    if(TYPEOF(b) == INTEGER)
      return mknumber(a->intval() / b->intval());
    else
      return mkfloat((double)a->intval() / b->floatval());
  else if(TYPEOF(b) == INTEGER)
    return mkfloat(a->floatval() / (double)b->intval());
  else
    return mkfloat(a->floatval() / b->floatval());
  /*NOTREACHED*/
}

PRIMITIVE iquotient(LISPT a, LISPT b)
{
  check(a, INTEGER);
  check(b, INTEGER);
  if(b->intval() == 0)
    return error(DIVIDE_ZERO, C_NIL);
  return mknumber(a->intval() / b->intval());
}

PRIMITIVE iremainder(LISPT a, LISPT b)
{
  check(a, INTEGER);
  check(b, INTEGER);
  if(b->intval() == 0)
    return error(DIVIDE_ZERO, C_NIL);
  return mknumber(a->intval() % b->intval());
}

PRIMITIVE fdivide(LISPT a, LISPT b)
{
  check(a, FLOAT);
  check(b, FLOAT);
  if(b->floatval() == 0.0)
    return error(DIVIDE_ZERO, C_NIL);
  return mkfloat(a->floatval() / b->floatval());
}

PRIMITIVE minus(LISPT a)
{
  check2(a, FLOAT, INTEGER);
  if(TYPEOF(a) == INTEGER)
    return mknumber(-a->intval());
  else
    return mkfloat(-a->floatval());
}

PRIMITIVE iminus(LISPT a)
{
  check(a, INTEGER);
  return mknumber(-a->intval());
}

PRIMITIVE absval(LISPT i)
{
  int sign;

  check(i, INTEGER);
  if(i->intval() < 0)
    sign = -1;
  else
    sign = 1;
  return mknumber(i->intval() * sign);
}

PRIMITIVE itof(LISPT i)
{
  check(i, INTEGER);
  return mkfloat((double)i->intval());
}

PRIMITIVE add1(LISPT a)
{
  check(a, INTEGER);
  return mknumber(a->intval() + 1);
}

PRIMITIVE sub1(LISPT a)
{
  check(a, INTEGER);
  return mknumber(a->intval() - 1);
}

#define FLOATFLOAT 0 /* Both arguments are float */
#define FLOATINT 1   /* One float and one int */
#define INTFLOAT 2   /* One int and one float */
#define INTINT 3     /* Both are ints */
#define ILLEGAL1 4   /* First argument is illegal */
#define ILLEGAL2 5   /* Second argument is illegal */

/* 
 * The result is one of the above constants depending on the types of the 
 * arguments.
 */
#define NUMTYPE(x, y) \
  (TYPEOF(x) == FLOAT) \
    ? ((TYPEOF(y) == FLOAT) ? FLOATFLOAT : (TYPEOF(y) == INTEGER) ? FLOATINT : ILLEGAL2) \
    : (TYPEOF(x) == INTEGER) ? ((TYPEOF(y) == FLOAT) ? INTFLOAT : (TYPEOF(y) == INTEGER) ? INTINT : ILLEGAL2) \
                             : ILLEGAL1

#define DOCHECK(x, y, cmp) \
  if(x cmp y) \
    return C_T; \
  else \
    return C_NIL;

#define ILLEGALRETURN(a) return error(ILLEGAL_ARG, a);

#define NUMCHECK(x, y, cmp) \
  switch(NUMTYPE(x, y)) \
  { \
    case FLOATFLOAT: \
      DOCHECK(x->floatval(), y->floatval(), cmp); \
    case FLOATINT: \
      DOCHECK(x->floatval(), (double)y->intval(), cmp); \
    case INTFLOAT: \
      DOCHECK((double)x->intval(), y->floatval(), cmp); \
    case INTINT: \
      DOCHECK(x->intval(), y->intval(), cmp); \
    case ILLEGAL1: \
      ILLEGALRETURN(x); \
    case ILLEGAL2: \
      ILLEGALRETURN(y); \
    default: \
      return error(BUG, C_NIL); \
  }

PRIMITIVE greaterp(LISPT x, LISPT y) { NUMCHECK(x, y, >); }

PRIMITIVE lessp(LISPT x, LISPT y) { NUMCHECK(x, y, <); }

PRIMITIVE eqp(LISPT x, LISPT y) { NUMCHECK(x, y, ==); }

PRIMITIVE geq(LISPT x, LISPT y) { NUMCHECK(x, y, >=); }

PRIMITIVE leq(LISPT x, LISPT y) { NUMCHECK(x, y, <=); }

PRIMITIVE neqp(LISPT x, LISPT y) { NUMCHECK(x, y, !=); }

PRIMITIVE zerop(LISPT x)
{
  if(TYPEOF(x) == INTEGER && x->intval() == 0)
    return C_T;
  return C_NIL;
}

PRIMITIVE minusp(LISPT x)
{
  if(TYPEOF(x) == FLOAT)
  {
    if(x->floatval() < 0.0)
      return C_T;
    else
      return C_NIL;
  }
  else if(TYPEOF(x) == INTEGER)
  {
    if(x->intval() < 0)
      return C_T;
    else
      return C_NIL;
  }
  return error(ILLEGAL_ARG, x);
}

arith::arith()
{
  mkprim(PN_PLUS, plus, -1, SUBR);
  mkprim(PN_DIFFERENCE, difference, 2, SUBR);
  mkprim(PN_TIMES, ltimes, -1, SUBR);
  mkprim(PN_DIVIDE, divide, 2, SUBR);
  mkprim(PN_IPLUS, iplus, -1, SUBR);
  mkprim(PN_IDIFFERENCE, idifference, 2, SUBR);
  mkprim(PN_ITIMES, itimes, -1, SUBR);
  mkprim(PN_IQUOTIENT, iquotient, 2, SUBR);
  mkprim(PN_IREMAINDER, iremainder, 2, SUBR);
  mkprim(PN_IMINUS, iminus, 1, SUBR);
  mkprim(PN_MINUS, minus, 1, SUBR);
  mkprim(PN_ADD1, add1, 1, SUBR);
  mkprim(PN_SUB1, sub1, 1, SUBR);
  mkprim(PN_FPLUS, fplus, -1, SUBR);
  mkprim(PN_FDIFFERENCE, fdifference, 2, SUBR);
  mkprim(PN_FTIMES, ftimes, -1, SUBR);
  mkprim(PN_FDIVIDE, fdivide, 2, SUBR);
  mkprim(PN_ITOF, itof, 1, SUBR);
  mkprim(PN_GREATERP, greaterp, 2, SUBR);
  mkprim(PN_GEQ, geq, 2, SUBR);
  mkprim(PN_LESSP, lessp, 2, SUBR);
  mkprim(PN_LEQ, leq, 2, SUBR);
  mkprim(PN_ZEROP, zerop, 1, SUBR);
  mkprim(PN_EQP, eqp, 2, SUBR);
  mkprim(PN_NEQP, neqp, 2, SUBR);
  mkprim(PN_MINUSP, minusp, 1, SUBR);
  mkprim(PN_ABS, absval, 1, SUBR);
}

} // namespace lisp
