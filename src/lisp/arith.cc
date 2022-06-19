//
// Lips, lisp shell.
// Copyright 1988, 2020-2022 Krister Joas
//

#include "arith.hh"

namespace lisp::arith
{
//
// Functions with an i as a prefix are for integer arithmetic and those whith
// an f are for floats.  Without a prefix, the functions are generic and
// converts arguments to the apropriate type.  Any float as an argument to a
// generic function results in a float.
//

LISPT plus(lisp& l, LISPT x)
{
  double fsum = 0.0;
  int sum = 0;
  bool f = false;

  while(type_of(x) == type::CONS)
  {
    if(f)
    {
      if(type_of(x->car()) == type::INTEGER)
        fsum += static_cast<double>(x->car()->intval());
      else if(type_of(x->car()) == type::FLOAT)
        fsum += x->car()->floatval();
      else
        return l.error(ILLEGAL_ARG, x->car());
    }
    else
    {
      if(type_of(x->car()) == type::INTEGER)
        sum += x->car()->intval();
      else if(type_of(x->car()) == type::FLOAT)
      {
        f = true;
        fsum = x->car()->floatval() + static_cast<double>(sum);
      }
      else
        return l.error(ILLEGAL_ARG, x->car());
    }
    x = x->cdr();
  }
  if(f)
    return mkfloat(l, fsum);
  return mknumber(l, sum);
}

LISPT iplus(lisp& l, LISPT x)
{
  int sum = 0;
  for(auto i = begin(x); i != end(x); ++i)
  {
    check(*i, type::INTEGER);
    sum += (**i).intval();
  }
  return mknumber(l, sum);
}

LISPT fplus(lisp& l, LISPT x)
{
  check(x->car(), type::FLOAT);
  auto fsum = x->car()->floatval();
  x = x->cdr();
  while(type_of(x) == type::CONS)
  {
    check(x->car(), type::FLOAT);
    fsum = fsum + x->car()->floatval();
    x = x->cdr();
  }
  return mkfloat(l, fsum);
}

LISPT difference(lisp& l, LISPT x, LISPT y)
{
  check(x, type::INTEGER, type::FLOAT);
  check(y, type::INTEGER, type::FLOAT);
  if(type_of(x) == type::INTEGER)
  {
    if(type_of(y) == type::INTEGER)
      return mknumber(l, x->intval() - y->intval());
    return mkfloat(l, static_cast<double>(x->intval()) - y->floatval());
  }
  if(type_of(y) == type::INTEGER)
    return mkfloat(l, x->floatval() - static_cast<double>(y->intval()));
  return mkfloat(l, x->floatval() - y->floatval());
}

LISPT idifference(lisp& l, LISPT x, LISPT y)
{
  check(x, type::INTEGER);
  check(y, type::INTEGER);
  return mknumber(l, x->intval() - y->intval());
}

LISPT fdifference(lisp& l, LISPT x, LISPT y)
{
  check(x, type::FLOAT);
  check(y, type::FLOAT);
  return mkfloat(l, x->floatval() - y->floatval());
}

LISPT ltimes(lisp& l, LISPT x)
{
  double fprod = 1.0;
  int prod = 1;
  int f = 0;

  while(type_of(x) == type::CONS)
  {
    if(f)
    {
      if(type_of(x->car()) == type::INTEGER)
        fprod *= (double)x->car()->intval();
      else if(type_of(x->car()) == type::FLOAT)
        fprod *= x->car()->floatval();
      else
        return l.error(ILLEGAL_ARG, x->car());
    }
    else if(type_of(x->car()) == type::INTEGER)
      prod *= x->car()->intval();
    else if(type_of(x->car()) == type::FLOAT)
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

LISPT itimes(lisp& l, LISPT x)
{
  check(x->car(), type::INTEGER);
  auto prod = x->car()->intval();
  x = x->cdr();
  while(type_of(x) == type::CONS)
  {
    check(x->car(), type::INTEGER);
    prod = prod * x->car()->intval();
    x = x->cdr();
  }
  return mknumber(l, prod);
}

LISPT ftimes(lisp& l, LISPT x)
{
  check(x->car(), type::FLOAT);
  auto prod = x->car()->floatval();
  x = x->cdr();
  while(type_of(x) == type::CONS)
  {
    check(x->car(), type::FLOAT);
    prod = prod * x->car()->floatval();
    x = x->cdr();
  }
  return mkfloat(l, prod);
}

LISPT divide(lisp& l, LISPT x, LISPT y)
{
  check(x, type::INTEGER, type::FLOAT);
  check(y, type::INTEGER, type::FLOAT);
  if(type_of(x) == type::INTEGER)
    if(type_of(y) == type::INTEGER)
    {
      if(y->intval() == 0)
        return l.error(DIVIDE_ZERO, NIL);
      return mknumber(l, x->intval() / y->intval());
    }
    else
    {
      if(y->floatval() == 0.0)
        return l.error(DIVIDE_ZERO, NIL);
      return mkfloat(l, (double)x->intval() / y->floatval());
    }
  else if(type_of(y) == type::INTEGER)
  {
    if(y->intval() == 0)
      return l.error(DIVIDE_ZERO, NIL);
    return mkfloat(l, x->floatval() / static_cast<double>(y->intval()));
  }
  if(y->floatval() == 0.0)
    return l.error(DIVIDE_ZERO, NIL);
  return mkfloat(l, x->floatval() / y->floatval());
}

LISPT iquotient(lisp& l, LISPT x, LISPT y)
{
  check(x, type::INTEGER);
  check(y, type::INTEGER);
  if(y->intval() == 0)
    return l.error(DIVIDE_ZERO, NIL);
  return mknumber(l, x->intval() / y->intval());
}

LISPT iremainder(lisp& l, LISPT x, LISPT y)
{
  check(x, type::INTEGER);
  check(y, type::INTEGER);
  if(y->intval() == 0)
    return l.error(DIVIDE_ZERO, NIL);
  return mknumber(l, x->intval() % y->intval());
}

LISPT fdivide(lisp& l, LISPT x, LISPT y)
{
  check(x, type::FLOAT);
  check(y, type::FLOAT);
  if(y->floatval() == 0.0)
    return l.error(DIVIDE_ZERO, NIL);
  return mkfloat(l, x->floatval() / y->floatval());
}

LISPT minus(lisp& l, LISPT x)
{
  check(x, type::FLOAT, type::INTEGER);
  if(type_of(x) == type::INTEGER)
    return mknumber(l, -x->intval());
  return mkfloat(l, -x->floatval());
}

LISPT iminus(lisp& l, LISPT x)
{
  check(x, type::INTEGER);
  return mknumber(l, -x->intval());
}

LISPT absval(lisp& l, LISPT x)
{
  int sign;

  check(x, type::INTEGER);
  if(x->intval() < 0)
    sign = -1;
  else
    sign = 1;
  return mknumber(l, x->intval() * sign);
}

LISPT itof(lisp& l, LISPT x)
{
  check(x, type::INTEGER);
  return mkfloat(l, static_cast<double>(x->intval()));
}

LISPT add1(lisp& l, LISPT x)
{
  check(x, type::INTEGER);
  return mknumber(l, x->intval() + 1);
}

LISPT sub1(lisp& l, LISPT x)
{
  check(x, type::INTEGER);
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
  if(type_of(x) == type::FLOAT)
  {
    if(type_of(y) == type::FLOAT)
      return num_type::FLOATFLOAT;
    if(type_of(y) == type::INTEGER)
      return num_type::FLOATINT;
    return num_type::ILLEGAL2;
  }
  if(type_of(x) == type::INTEGER)
  {
    if(type_of(y) == type::FLOAT)
      return num_type::INTFLOAT;
    if(type_of(y) == type::INTEGER)
      return num_type::INTINT;
    return num_type::ILLEGAL2;
  }
  return num_type::ILLEGAL1;
}

template<typename Type, typename Comparor>
inline LISPT docheck(Type x, Type y, Comparor cmp)
{
  if(cmp(x, y))
    return T;
  return NIL;
}

inline LISPT illegalreturn(lisp& l, LISPT x) { return l.error(ILLEGAL_ARG, x); }

template<template<typename> typename Comparer>
inline LISPT numcheck(lisp& l, LISPT x, LISPT y)
{
  switch(numtype(x, y))
  {
    case num_type::FLOATFLOAT:
      return docheck(x->floatval(), y->floatval(), Comparer<double>());
    case num_type::FLOATINT:
      return docheck(x->floatval(), static_cast<double>(y->intval()), Comparer<double>());
    case num_type::INTFLOAT:
      return docheck(static_cast<double>(x->intval()), y->floatval(), Comparer<double>());
    case num_type::INTINT:
      return docheck(x->intval(), y->intval(), Comparer<int>());
    case num_type::ILLEGAL1:
      return illegalreturn(l, x);
    default:
      return illegalreturn(l, y);
  }
}

LISPT greaterp(lisp& l, LISPT x, LISPT y) { return numcheck<std::greater>(l, x, y); }

LISPT lessp(lisp& l, LISPT x, LISPT y) { return numcheck<std::less>(l, x, y); }

LISPT eqp(lisp& l, LISPT x, LISPT y) { return numcheck<std::equal_to>(l, x, y); }

LISPT geq(lisp& l, LISPT x, LISPT y) { return numcheck<std::greater_equal>(l, x, y); }

LISPT leq(lisp& l, LISPT x, LISPT y) { return numcheck<std::less_equal>(l, x, y); }

LISPT neqp(lisp& l, LISPT x, LISPT y) { return numcheck<std::not_equal_to>(l, x, y); }

LISPT zerop(lisp& l, LISPT x)
{
  if(type_of(x) == type::INTEGER && x->intval() == 0)
    return T;
  return NIL;
}

LISPT minusp(lisp& l, LISPT x)
{
  if(type_of(x) == type::FLOAT)
  {
    if(x->floatval() < 0.0)
      return T;
    return NIL;
  }
  if(type_of(x) == type::INTEGER)
  {
    if(x->intval() < 0)
      return T;
    return NIL;
  }
  return l.error(ILLEGAL_ARG, x);
}

namespace pn
{
inline constexpr auto PLUS = "plus";               // addition
inline constexpr auto DIFFERENCE = "difference";   // subtraction
inline constexpr auto TIMES = "times";             // multiplication
inline constexpr auto DIVIDE = "divide";           // division
inline constexpr auto IPLUS = "iplus";             // integer addition
inline constexpr auto IDIFFERENCE = "idifference"; // integer subtraction
inline constexpr auto ITIMES = "itimes";           // integer multiplication
inline constexpr auto IQUOTIENT = "iquotient";     // integer division
inline constexpr auto IREMAINDER = "iremainder";   // integer mod
inline constexpr auto IMINUS = "iminus";           // integer change sign
inline constexpr auto MINUS = "minus";             // change sign mixed
inline constexpr auto ADD1 = "add1";               // add one
inline constexpr auto SUB1 = "sub1";               // subtract one
inline constexpr auto ABS = "abs";                 // absolute value
inline constexpr auto FPLUS = "fplus";             // float addition
inline constexpr auto FDIFFERENCE = "fdifference"; // float subtraction
inline constexpr auto FTIMES = "ftimes";           // float multiplication
inline constexpr auto FDIVIDE = "fdivide";         // float division
inline constexpr auto ITOF = "itof";               // integer to float
inline constexpr auto GREATERP = "greaterp";       // t if greater than
inline constexpr auto GEQ = "geq";                 // t if greater or eq
inline constexpr auto LESSP = "lessp";             // t if less than
inline constexpr auto LEQ = "leq";                 // t if less or eq
inline constexpr auto ZEROP = "zerop";             // t if eq to 0
inline constexpr auto EQP = "eqp";                 // t if eq or numerically equal
inline constexpr auto NEQP = "neqp";               // not eqp
inline constexpr auto MINUSP = "minusp";           // t if negative
} // namespace pn

void init()
{
  // clang-format off
  mkprim(pn::PLUS,        plus,        subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::DIFFERENCE,  difference,  subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::TIMES,       ltimes,      subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::DIVIDE,      divide,      subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::IPLUS,       iplus,       subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::IDIFFERENCE, idifference, subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::ITIMES,      itimes,      subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::IQUOTIENT,   iquotient,   subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::IREMAINDER,  iremainder,  subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::IMINUS,      iminus,      subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::MINUS,       minus,       subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::ADD1,        add1,        subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::SUB1,        sub1,        subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::FPLUS,       fplus,       subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::FDIFFERENCE, fdifference, subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::FTIMES,      ftimes,      subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::FDIVIDE,     fdivide,     subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::ITOF,        itof,        subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::GREATERP,    greaterp,    subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::GEQ,         geq,         subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::LESSP,       lessp,       subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::LEQ,         leq,         subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::ZEROP,       zerop,       subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::EQP,         eqp,         subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::NEQP,        neqp,        subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::MINUSP,      minusp,      subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::ABS,         absval,      subr_t::subr::EVAL, subr_t::spread::SPREAD);
  // clang-format on
}

} // namespace lisp::arith
