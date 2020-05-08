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

PRIMITIVE arith::plus(LISPT x)
{
  double fsum = 0.0;
  int sum = 0;
  int f = 0;

  while(type_of(x) == CONS)
  {
    if(f)
    {
      if(type_of(x->car()) == INTEGER)
        fsum += (double)x->car()->intval();
      else if(type_of(x->car()) == FLOAT)
        fsum += x->car()->floatval();
      else
        return l.error(ILLEGAL_ARG, x->car());
    }
    else if(type_of(x->car()) == INTEGER)
      sum += x->car()->intval();
    else if(type_of(x->car()) == FLOAT)
    {
      f = 1;
      fsum = x->car()->floatval() + (double)sum;
    }
    else
      return l.error(ILLEGAL_ARG, x->car());
    x = x->cdr();
  }
  if(f)
    return mkfloat(l, fsum);
  return mknumber(l, sum);
}

PRIMITIVE arith::iplus(LISPT x)
{
  int sum = 0;
  for(auto i = begin(x); i != end(x); ++i)
  {
    l.check(*i, INTEGER);
    sum += (**i).intval();
  }
  return mknumber(l, sum);
}

PRIMITIVE arith::fplus(LISPT x)
{
  l.check(x->car(), FLOAT);
  auto sum = x->car()->floatval();
  x = x->cdr();
  while(type_of(x) == CONS)
  {
    l.check(x->car(), FLOAT);
    sum = sum + x->car()->floatval();
    x = x->cdr();
  }
  return mkfloat(l, sum);
}

PRIMITIVE arith::difference(LISPT x, LISPT y)
{
  l.check(x, INTEGER, FLOAT);
  l.check(y, INTEGER, FLOAT);
  if(type_of(x) == INTEGER)
    if(type_of(y) == INTEGER)
      return mknumber(l, x->intval() - y->intval());
    else
      return mkfloat(l, (double)x->intval() - y->floatval());
  else if(type_of(y) == INTEGER)
    return mkfloat(l, x->floatval() - (double)y->intval());
  return mkfloat(l, x->floatval() - y->floatval());
}

PRIMITIVE arith::idifference(LISPT x, LISPT y)
{
  l.check(x, INTEGER);
  l.check(y, INTEGER);
  return mknumber(l, x->intval() - y->intval());
}

PRIMITIVE arith::fdifference(LISPT x, LISPT y)
{
  l.check(x, FLOAT);
  l.check(y, FLOAT);
  return mkfloat(l, x->floatval() - y->floatval());
}

PRIMITIVE arith::ltimes(LISPT x)
{
  double fprod = 1.0;
  int prod = 1;
  int f = 0;

  while(type_of(x) == CONS)
  {
    if(f)
    {
      if(type_of(x->car()) == INTEGER)
        fprod *= (double)x->car()->intval();
      else if(type_of(x->car()) == FLOAT)
        fprod *= x->car()->floatval();
      else
        return l.error(ILLEGAL_ARG, x->car());
    }
    else if(type_of(x->car()) == INTEGER)
      prod *= x->car()->intval();
    else if(type_of(x->car()) == FLOAT)
    {
      f = 1;
      fprod = x->car()->floatval() * (double)prod;
    }
    else
      return l.error(ILLEGAL_ARG, x->car());
    x = x->cdr();
  }
  if(f)
    return mkfloat(l, fprod);
  return mknumber(l, prod);
}

PRIMITIVE arith::itimes(LISPT x)
{
  l.check(x->car(), INTEGER);
  auto prod = x->car()->intval();
  x = x->cdr();
  while(type_of(x) == CONS)
  {
    l.check(x->car(), INTEGER);
    prod = prod * x->car()->intval();
    x = x->cdr();
  }
  return mknumber(l, prod);
}

PRIMITIVE arith::ftimes(LISPT x)
{
  l.check(x->car(), FLOAT);
  auto prod = x->car()->floatval();
  x = x->cdr();
  while(type_of(x) == CONS)
  {
    l.check(x->car(), FLOAT);
    prod = prod * x->car()->floatval();
    x = x->cdr();
  }
  return mkfloat(l, prod);
}

PRIMITIVE arith::divide(LISPT x, LISPT y)
{
  l.check(x, INTEGER, FLOAT);
  l.check(y, INTEGER, FLOAT);
  if(type_of(x) == INTEGER)
    if(type_of(y) == INTEGER)
      return mknumber(l, x->intval() / y->intval());
    else
      return mkfloat(l, (double)x->intval() / y->floatval());
  else if(type_of(y) == INTEGER)
    return mkfloat(l, x->floatval() / (double)y->intval());
  return mkfloat(l, x->floatval() / y->floatval());
}

PRIMITIVE arith::iquotient(LISPT x, LISPT y)
{
  l.check(x, INTEGER);
  l.check(y, INTEGER);
  if(y->intval() == 0)
    return l.error(DIVIDE_ZERO, C_NIL);
  return mknumber(l, x->intval() / y->intval());
}

PRIMITIVE arith::iremainder(LISPT x, LISPT y)
{
  l.check(x, INTEGER);
  l.check(y, INTEGER);
  if(y->intval() == 0)
    return l.error(DIVIDE_ZERO, C_NIL);
  return mknumber(l, x->intval() % y->intval());
}

PRIMITIVE arith::fdivide(LISPT x, LISPT y)
{
  l.check(x, FLOAT);
  l.check(y, FLOAT);
  if(y->floatval() == 0.0)
    return l.error(DIVIDE_ZERO, C_NIL);
  return mkfloat(l, x->floatval() / y->floatval());
}

PRIMITIVE arith::minus(LISPT x)
{
  l.check(x, FLOAT, INTEGER);
  if(type_of(x) == INTEGER)
    return mknumber(l, -x->intval());
  return mkfloat(l, -x->floatval());
}

PRIMITIVE arith::iminus(LISPT x)
{
  l.check(x, INTEGER);
  return mknumber(l, -x->intval());
}

PRIMITIVE arith::absval(LISPT x)
{
  int sign;

  l.check(x, INTEGER);
  if(x->intval() < 0)
    sign = -1;
  else
    sign = 1;
  return mknumber(l, x->intval() * sign);
}

PRIMITIVE arith::itof(LISPT x)
{
  l.check(x, INTEGER);
  return mkfloat(l, (double)x->intval());
}

PRIMITIVE arith::add1(LISPT x)
{
  l.check(x, INTEGER);
  return mknumber(l, x->intval() + 1);
}

PRIMITIVE arith::sub1(LISPT x)
{
  l.check(x, INTEGER);
  return mknumber(l, x->intval() - 1);
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

inline void illegalreturn(lisp& l, LISPT x) { l.error(ILLEGAL_ARG, x); }

template<template<typename> typename Comparer>
inline LISPT numcheck(lisp& l, LISPT x, LISPT y)
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
      illegalreturn(l, x);
      break;
    case num_type::ILLEGAL2:
      illegalreturn(l, y);
      break;
    default:
      break;
  }
  return l.error(BUG, C_NIL);
}

PRIMITIVE arith::greaterp(LISPT x, LISPT y) { return numcheck<std::greater>(l, x, y); }

PRIMITIVE arith::lessp(LISPT x, LISPT y) { return numcheck<std::less>(l, x, y); }

PRIMITIVE arith::eqp(LISPT x, LISPT y) { return numcheck<std::equal_to>(l, x, y); }

PRIMITIVE arith::geq(LISPT x, LISPT y) { return numcheck<std::greater_equal>(l, x, y); }

PRIMITIVE arith::leq(LISPT x, LISPT y) { return numcheck<std::less_equal>(l, x, y); }

PRIMITIVE arith::neqp(LISPT x, LISPT y) { return numcheck<std::not_equal_to>(l, x, y); }

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
  return l.error(ILLEGAL_ARG, x);
}

arith::arith(lisp& lisp): base(lisp) {}

void arith::init()
{
  // clang-format off
  mkprim(PN_PLUS,        ::lisp::plus,        1, subr_t::S_EVAL, subr_t::S_SPREAD);
  mkprim(PN_DIFFERENCE,  ::lisp::difference,  2, subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_TIMES,       ::lisp::ltimes,      1, subr_t::S_EVAL, subr_t::S_SPREAD);
  mkprim(PN_DIVIDE,      ::lisp::divide,      2, subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_IPLUS,       ::lisp::iplus,       1, subr_t::S_EVAL, subr_t::S_SPREAD);
  mkprim(PN_IDIFFERENCE, ::lisp::idifference, 2, subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_ITIMES,      ::lisp::itimes,      1, subr_t::S_EVAL, subr_t::S_SPREAD);
  mkprim(PN_IQUOTIENT,   ::lisp::iquotient,   2, subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_IREMAINDER,  ::lisp::iremainder,  2, subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_IMINUS,      ::lisp::iminus,      1, subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_MINUS,       ::lisp::minus,       1, subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_ADD1,        ::lisp::add1,        1, subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_SUB1,        ::lisp::sub1,        1, subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_FPLUS,       ::lisp::fplus,       1, subr_t::S_EVAL, subr_t::S_SPREAD);
  mkprim(PN_FDIFFERENCE, ::lisp::fdifference, 2, subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_FTIMES,      ::lisp::ftimes,      1, subr_t::S_EVAL, subr_t::S_SPREAD);
  mkprim(PN_FDIVIDE,     ::lisp::fdivide,     2, subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_ITOF,        ::lisp::itof,        1, subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_GREATERP,    ::lisp::greaterp,    2, subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_GEQ,         ::lisp::geq,         2, subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_LESSP,       ::lisp::lessp,       2, subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_LEQ,         ::lisp::leq,         2, subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_ZEROP,       ::lisp::zerop,       1, subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_EQP,         ::lisp::eqp,         2, subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_NEQP,        ::lisp::neqp,        2, subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_MINUSP,      ::lisp::minusp,      1, subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_ABS,         ::lisp::absval,      1, subr_t::S_EVAL, subr_t::S_NOSPREAD);
  // clang-format on
}

} // namespace lisp
