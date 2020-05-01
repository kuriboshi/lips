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

PRIMITIVE arith::plus(LISPT l)
{
  double fsum = 0.0;
  int sum = 0;
  int f = 0;

  while(type_of(l) == CONS)
  {
    if(f)
    {
      if(type_of(l->car()) == INTEGER)
        fsum += (double)l->car()->intval();
      else if(type_of(l->car()) == FLOAT)
        fsum += l->car()->floatval();
      else
        return error(ILLEGAL_ARG, l->car());
    }
    else if(type_of(l->car()) == INTEGER)
      sum += l->car()->intval();
    else if(type_of(l->car()) == FLOAT)
    {
      f = 1;
      fsum = l->car()->floatval() + (double)sum;
    }
    else
      return error(ILLEGAL_ARG, l->car());
    l = l->cdr();
  }
  if(f)
    return mkfloat(_lisp, fsum);
  return mknumber(_lisp, sum);
}

PRIMITIVE arith::iplus(LISPT l)
{
  int sum = 0;
  for(auto i = begin(l); i != end(l); ++i)
  {
    check(*i, INTEGER);
    sum += (**i).intval();
  }
  return a().mknumber(sum);
}

PRIMITIVE arith::fplus(LISPT l)
{
  check(l->car(), FLOAT);
  auto sum = l->car()->floatval();
  l = l->cdr();
  while(type_of(l) == CONS)
  {
    check(l->car(), FLOAT);
    sum = sum + l->car()->floatval();
    l = l->cdr();
  }
  return a().mkfloat(sum);
}

PRIMITIVE arith::difference(LISPT a, LISPT b)
{
  check2(a, INTEGER, FLOAT);
  check2(b, INTEGER, FLOAT);
  if(type_of(a) == INTEGER)
    if(type_of(b) == INTEGER)
      return mknumber(_lisp, a->intval() - b->intval());
    else
      return mkfloat(_lisp, (double)a->intval() - b->floatval());
  else if(type_of(b) == INTEGER)
    return mkfloat(_lisp, a->floatval() - (double)b->intval());
  return mkfloat(_lisp, a->floatval() - b->floatval());
}

PRIMITIVE arith::idifference(LISPT a, LISPT b)
{
  check(a, INTEGER);
  check(b, INTEGER);
  return mknumber(_lisp, a->intval() - b->intval());
}

PRIMITIVE arith::fdifference(LISPT a, LISPT b)
{
  check(a, FLOAT);
  check(b, FLOAT);
  return mkfloat(_lisp, a->floatval() - b->floatval());
}

PRIMITIVE arith::ltimes(LISPT l)
{
  double fprod = 1.0;
  int prod = 1;
  int f = 0;

  while(type_of(l) == CONS)
  {
    if(f)
    {
      if(type_of(l->car()) == INTEGER)
        fprod *= (double)l->car()->intval();
      else if(type_of(l->car()) == FLOAT)
        fprod *= l->car()->floatval();
      else
        return error(ILLEGAL_ARG, l->car());
    }
    else if(type_of(l->car()) == INTEGER)
      prod *= l->car()->intval();
    else if(type_of(l->car()) == FLOAT)
    {
      f = 1;
      fprod = l->car()->floatval() * (double)prod;
    }
    else
      return error(ILLEGAL_ARG, l->car());
    l = l->cdr();
  }
  if(f)
    return mkfloat(_lisp, fprod);
  return mknumber(_lisp, prod);
}

PRIMITIVE arith::itimes(LISPT l)
{
  check(l->car(), INTEGER);
  auto prod = l->car()->intval();
  l = l->cdr();
  while(type_of(l) == CONS)
  {
    check(l->car(), INTEGER);
    prod = prod * l->car()->intval();
    l = l->cdr();
  }
  return mknumber(_lisp, prod);
}

PRIMITIVE arith::ftimes(LISPT l)
{
  check(l->car(), FLOAT);
  auto prod = l->car()->floatval();
  l = l->cdr();
  while(type_of(l) == CONS)
  {
    check(l->car(), FLOAT);
    prod = prod * l->car()->floatval();
    l = l->cdr();
  }
  return mkfloat(_lisp, prod);
}

PRIMITIVE arith::divide(LISPT a, LISPT b)
{
  check2(a, INTEGER, FLOAT);
  check2(b, INTEGER, FLOAT);
  if(type_of(a) == INTEGER)
    if(type_of(b) == INTEGER)
      return mknumber(_lisp, a->intval() / b->intval());
    else
      return mkfloat(_lisp, (double)a->intval() / b->floatval());
  else if(type_of(b) == INTEGER)
    return mkfloat(_lisp, a->floatval() / (double)b->intval());
  else
    return mkfloat(_lisp, a->floatval() / b->floatval());
  /*NOTREACHED*/
}

PRIMITIVE arith::iquotient(LISPT a, LISPT b)
{
  check(a, INTEGER);
  check(b, INTEGER);
  if(b->intval() == 0)
    return error(DIVIDE_ZERO, C_NIL);
  return mknumber(_lisp, a->intval() / b->intval());
}

PRIMITIVE arith::iremainder(LISPT a, LISPT b)
{
  check(a, INTEGER);
  check(b, INTEGER);
  if(b->intval() == 0)
    return error(DIVIDE_ZERO, C_NIL);
  return mknumber(_lisp, a->intval() % b->intval());
}

PRIMITIVE arith::fdivide(LISPT a, LISPT b)
{
  check(a, FLOAT);
  check(b, FLOAT);
  if(b->floatval() == 0.0)
    return error(DIVIDE_ZERO, C_NIL);
  return mkfloat(_lisp, a->floatval() / b->floatval());
}

PRIMITIVE arith::minus(LISPT a)
{
  check2(a, FLOAT, INTEGER);
  if(type_of(a) == INTEGER)
    return mknumber(_lisp, -a->intval());
  return mkfloat(_lisp, -a->floatval());
}

PRIMITIVE arith::iminus(LISPT a)
{
  check(a, INTEGER);
  return mknumber(_lisp, -a->intval());
}

PRIMITIVE arith::absval(LISPT i)
{
  int sign;

  check(i, INTEGER);
  if(i->intval() < 0)
    sign = -1;
  else
    sign = 1;
  return mknumber(_lisp, i->intval() * sign);
}

PRIMITIVE arith::itof(LISPT i)
{
  check(i, INTEGER);
  return mkfloat(_lisp, (double)i->intval());
}

PRIMITIVE arith::add1(LISPT a)
{
  check(a, INTEGER);
  return mknumber(_lisp, a->intval() + 1);
}

PRIMITIVE arith::sub1(LISPT a)
{
  check(a, INTEGER);
  return mknumber(_lisp, a->intval() - 1);
}

enum class num_type
{
  FLOATFLOAT = 0, // Both arguments are float
  FLOATINT = 1,   // One float and one int
  INTFLOAT = 2,   // One int and one float
  INTINT = 3,     // Both are ints
  ILLEGAL1 = 4,   // First argument is illegal
  ILLEGAL2 = 5    // Second argument is illegal
};

inline num_type numtype(LISPT x, LISPT y)
{
  if(type_of(x) == FLOAT)
  {
    if(type_of(y) == FLOAT)
      return num_type::FLOATFLOAT;
    else if(type_of(y) == INTEGER)
      return num_type::FLOATINT;
    return num_type::ILLEGAL2;
  }
  else if(type_of(x) == INTEGER)
  {
    if(type_of(y) == FLOAT)
      return num_type::INTFLOAT;
    else if(type_of(y) == INTEGER)
      return num_type::INTINT;
    return num_type::ILLEGAL2;
  }
  return num_type::ILLEGAL1;
}

template<typename T, typename C>
inline LISPT docheck(T x, T y, C cmp)
{
  if(cmp(x, y))
    return C_T;
  return C_NIL;
}

inline void illegalreturn(LISPT a) { error(ILLEGAL_ARG, a); }

template<template<typename> typename Comparer>
inline LISPT numcheck(LISPT x, LISPT y)
{
  switch(numtype(x, y))
  {
    case num_type::FLOATFLOAT:
      return docheck(x->floatval(), y->floatval(), Comparer<double>());
    case num_type::FLOATINT:
      return docheck(x->floatval(), (double)y->intval(), Comparer<double>());
    case num_type::INTFLOAT:
      return docheck((double)x->intval(), y->floatval(), Comparer<double>());
    case num_type::INTINT:
      return docheck(x->intval(), y->intval(), Comparer<int>());
    case num_type::ILLEGAL1:
      illegalreturn(x);
      break;
    case num_type::ILLEGAL2:
      illegalreturn(y);
      break;
    default:
      break;
  }
  return error(BUG, C_NIL);
}

PRIMITIVE arith::greaterp(LISPT x, LISPT y) { return numcheck<std::greater>(x, y); }

PRIMITIVE arith::lessp(LISPT x, LISPT y) { return numcheck<std::less>(x, y); }

PRIMITIVE arith::eqp(LISPT x, LISPT y) { return numcheck<std::equal_to>(x, y); }

PRIMITIVE arith::geq(LISPT x, LISPT y) { return numcheck<std::greater_equal>(x, y); }

PRIMITIVE arith::leq(LISPT x, LISPT y) { return numcheck<std::less_equal>(x, y); }

PRIMITIVE arith::neqp(LISPT x, LISPT y) { return numcheck<std::not_equal_to>(x, y); }

PRIMITIVE arith::zerop(LISPT x)
{
  if(type_of(x) == INTEGER && x->intval() == 0)
    return C_T;
  return C_NIL;
}

PRIMITIVE arith::minusp(LISPT x)
{
  if(type_of(x) == FLOAT)
  {
    if(x->floatval() < 0.0)
      return C_T;
    else
      return C_NIL;
  }
  else if(type_of(x) == INTEGER)
  {
    if(x->intval() < 0)
      return C_T;
    else
      return C_NIL;
  }
  return error(ILLEGAL_ARG, x);
}

arith::arith(lisp& lisp) : base(lisp)
{
  mkprim(PN_PLUS, ::lisp::plus, -1, SUBR);
  mkprim(PN_DIFFERENCE, ::lisp::difference, 2, SUBR);
  mkprim(PN_TIMES, ::lisp::ltimes, -1, SUBR);
  mkprim(PN_DIVIDE, ::lisp::divide, 2, SUBR);
  mkprim(PN_IPLUS, ::lisp::iplus, -1, SUBR);
  mkprim(PN_IDIFFERENCE, ::lisp::idifference, 2, SUBR);
  mkprim(PN_ITIMES, ::lisp::itimes, -1, SUBR);
  mkprim(PN_IQUOTIENT, ::lisp::iquotient, 2, SUBR);
  mkprim(PN_IREMAINDER, ::lisp::iremainder, 2, SUBR);
  mkprim(PN_IMINUS, ::lisp::iminus, 1, SUBR);
  mkprim(PN_MINUS, ::lisp::minus, 1, SUBR);
  mkprim(PN_ADD1, ::lisp::add1, 1, SUBR);
  mkprim(PN_SUB1, ::lisp::sub1, 1, SUBR);
  mkprim(PN_FPLUS, ::lisp::fplus, -1, SUBR);
  mkprim(PN_FDIFFERENCE, ::lisp::fdifference, 2, SUBR);
  mkprim(PN_FTIMES, ::lisp::ftimes, -1, SUBR);
  mkprim(PN_FDIVIDE, ::lisp::fdivide, 2, SUBR);
  mkprim(PN_ITOF, ::lisp::itof, 1, SUBR);
  mkprim(PN_GREATERP, ::lisp::greaterp, 2, SUBR);
  mkprim(PN_GEQ, ::lisp::geq, 2, SUBR);
  mkprim(PN_LESSP, ::lisp::lessp, 2, SUBR);
  mkprim(PN_LEQ, ::lisp::leq, 2, SUBR);
  mkprim(PN_ZEROP, ::lisp::zerop, 1, SUBR);
  mkprim(PN_EQP, ::lisp::eqp, 2, SUBR);
  mkprim(PN_NEQP, ::lisp::neqp, 2, SUBR);
  mkprim(PN_MINUSP, ::lisp::minusp, 1, SUBR);
  mkprim(PN_ABS, ::lisp::absval, 1, SUBR);
}

} // namespace lisp
