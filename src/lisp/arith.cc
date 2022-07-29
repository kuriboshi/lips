//
// Lips, lisp shell.
// Copyright 1988, 2020-2022 Krister Joas
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//

#include "arith.hh"

namespace lisp::details::arith
{
//
// Functions with an i as a prefix are for integer arithmetic and those whith
// an f are for floats.  Without a prefix, the functions are generic and
// converts arguments to the apropriate type.  Any float as an argument to a
// generic function results in a float.
//

LISPT plus(context& ctx, LISPT x)
{
  double fsum = 0.0;
  int sum = 0;
  bool f = false;

  while(type_of(x) == type::Cons)
  {
    if(f)
    {
      if(type_of(x->car()) == type::Integer)
        fsum += static_cast<double>(x->car()->intval());
      else if(type_of(x->car()) == type::Float)
        fsum += x->car()->floatval();
      else
        ctx.error(error_errc::illegal_arg, x->car());
    }
    else
    {
      if(type_of(x->car()) == type::Integer)
        sum += x->car()->intval();
      else if(type_of(x->car()) == type::Float)
      {
        f = true;
        fsum = x->car()->floatval() + static_cast<double>(sum);
      }
      else
        ctx.error(error_errc::illegal_arg, x->car());
    }
    x = x->cdr();
  }
  if(f)
    return mkfloat(fsum);
  return mknumber(sum);
}

LISPT iplus(context& ctx, LISPT x)
{
  int sum = 0;
  for(auto i = begin(x); i != end(x); ++i)
  {
    check(*i, type::Integer);
    sum += (**i).intval();
  }
  return mknumber(sum);
}

LISPT fplus(context& ctx, LISPT x)
{
  check(x->car(), type::Float);
  auto fsum = x->car()->floatval();
  x = x->cdr();
  while(type_of(x) == type::Cons)
  {
    check(x->car(), type::Float);
    fsum = fsum + x->car()->floatval();
    x = x->cdr();
  }
  return mkfloat(fsum);
}

LISPT difference(context& ctx, LISPT x, LISPT y)
{
  check(x, type::Integer, type::Float);
  check(y, type::Integer, type::Float);
  if(type_of(x) == type::Integer)
  {
    if(type_of(y) == type::Integer)
      return mknumber(x->intval() - y->intval());
    return mkfloat(static_cast<double>(x->intval()) - y->floatval());
  }
  if(type_of(y) == type::Integer)
    return mkfloat(x->floatval() - static_cast<double>(y->intval()));
  return mkfloat(x->floatval() - y->floatval());
}

LISPT idifference(context& ctx, LISPT x, LISPT y)
{
  check(x, type::Integer);
  check(y, type::Integer);
  return mknumber(x->intval() - y->intval());
}

LISPT fdifference(context& ctx, LISPT x, LISPT y)
{
  check(x, type::Float);
  check(y, type::Float);
  return mkfloat(x->floatval() - y->floatval());
}

LISPT ltimes(context& ctx, LISPT x)
{
  double fprod = 1.0;
  int prod = 1;
  int f = 0;

  while(type_of(x) == type::Cons)
  {
    if(f != 0)
    {
      if(type_of(x->car()) == type::Integer)
        fprod *= (double)x->car()->intval();
      else if(type_of(x->car()) == type::Float)
        fprod *= x->car()->floatval();
      else
        ctx.error(error_errc::illegal_arg, x->car());
    }
    else if(type_of(x->car()) == type::Integer)
      prod *= x->car()->intval();
    else if(type_of(x->car()) == type::Float)
    {
      f = 1;
      fprod = x->car()->floatval() * (double)prod;
    }
    else
      ctx.error(error_errc::illegal_arg, x->car());
    x = x->cdr();
  }
  if(f != 0)
    return mkfloat(fprod);
  return mknumber(prod);
}

LISPT itimes(context& ctx, LISPT x)
{
  check(x->car(), type::Integer);
  auto prod = x->car()->intval();
  x = x->cdr();
  while(type_of(x) == type::Cons)
  {
    check(x->car(), type::Integer);
    prod = prod * x->car()->intval();
    x = x->cdr();
  }
  return mknumber(prod);
}

LISPT ftimes(context& ctx, LISPT x)
{
  check(x->car(), type::Float);
  auto prod = x->car()->floatval();
  x = x->cdr();
  while(type_of(x) == type::Cons)
  {
    check(x->car(), type::Float);
    prod = prod * x->car()->floatval();
    x = x->cdr();
  }
  return mkfloat(prod);
}

LISPT divide(context& ctx, LISPT x, LISPT y)
{
  check(x, type::Integer, type::Float);
  check(y, type::Integer, type::Float);
  if(type_of(x) == type::Integer)
    if(type_of(y) == type::Integer)
    {
      if(y->intval() == 0)
        ctx.error(error_errc::divide_by_zero, NIL);
      return mknumber(x->intval() / y->intval());
    }
    else
    {
      if(y->floatval() == 0.0)
        ctx.error(error_errc::divide_by_zero, NIL);
      return mkfloat((double)x->intval() / y->floatval());
    }
  else if(type_of(y) == type::Integer)
  {
    if(y->intval() == 0)
      ctx.error(error_errc::divide_by_zero, NIL);
    return mkfloat(x->floatval() / static_cast<double>(y->intval()));
  }
  if(y->floatval() == 0.0)
    ctx.error(error_errc::divide_by_zero, NIL);
  return mkfloat(x->floatval() / y->floatval());
}

LISPT iquotient(context& ctx, LISPT x, LISPT y)
{
  check(x, type::Integer);
  check(y, type::Integer);
  if(y->intval() == 0)
    ctx.error(error_errc::divide_by_zero, NIL);
  return mknumber(x->intval() / y->intval());
}

LISPT iremainder(context& ctx, LISPT x, LISPT y)
{
  check(x, type::Integer);
  check(y, type::Integer);
  if(y->intval() == 0)
    ctx.error(error_errc::divide_by_zero, NIL);
  return mknumber(x->intval() % y->intval());
}

LISPT fdivide(context& ctx, LISPT x, LISPT y)
{
  check(x, type::Float);
  check(y, type::Float);
  if(y->floatval() == 0.0)
    ctx.error(error_errc::divide_by_zero, NIL);
  return mkfloat(x->floatval() / y->floatval());
}

LISPT minus(context& ctx, LISPT x)
{
  check(x, type::Float, type::Integer);
  if(type_of(x) == type::Integer)
    return mknumber(-x->intval());
  return mkfloat(-x->floatval());
}

LISPT iminus(context& ctx, LISPT x)
{
  check(x, type::Integer);
  return mknumber(-x->intval());
}

LISPT abs(context& ctx, LISPT x)
{
  check(x, type::Integer);
  if(x->intval() < 0)
    return mknumber(-x->intval());
  return mknumber(x->intval());
}

LISPT itof(context& ctx, LISPT x)
{
  check(x, type::Integer);
  return mkfloat(static_cast<double>(x->intval()));
}

LISPT add1(context& ctx, LISPT x)
{
  check(x, type::Integer);
  return mknumber(x->intval() + 1);
}

LISPT sub1(context& ctx, LISPT x)
{
  check(x, type::Integer);
  return mknumber(x->intval() - 1);
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
  if(type_of(x) == type::Float)
  {
    if(type_of(y) == type::Float)
      return num_type::FLOATFLOAT;
    if(type_of(y) == type::Integer)
      return num_type::FLOATINT;
    return num_type::ILLEGAL2;
  }
  if(type_of(x) == type::Integer)
  {
    if(type_of(y) == type::Float)
      return num_type::INTFLOAT;
    if(type_of(y) == type::Integer)
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

inline LISPT illegalreturn(context& ctx, LISPT x) { return ctx.error(error_errc::illegal_arg, x); }

template<template<typename> typename Comparer>
inline LISPT numcheck(context& ctx, LISPT x, LISPT y)
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
      return illegalreturn(ctx, x);
    default:
      return illegalreturn(ctx, y);
  }
}

LISPT greaterp(context& ctx, LISPT x, LISPT y) { return numcheck<std::greater>(ctx, x, y); }

LISPT lessp(context& ctx, LISPT x, LISPT y) { return numcheck<std::less>(ctx, x, y); }

LISPT eqp(context& ctx, LISPT x, LISPT y) { return numcheck<std::equal_to>(ctx, x, y); }

LISPT geq(context& ctx, LISPT x, LISPT y) { return numcheck<std::greater_equal>(ctx, x, y); }

LISPT leq(context& ctx, LISPT x, LISPT y) { return numcheck<std::less_equal>(ctx, x, y); }

LISPT neqp(context& ctx, LISPT x, LISPT y) { return numcheck<std::not_equal_to>(ctx, x, y); }

LISPT zerop(context& ctx, LISPT x)
{
  if(type_of(x) == type::Integer && x->intval() == 0)
    return T;
  return NIL;
}

LISPT minusp(context& ctx, LISPT x)
{
  if(type_of(x) == type::Float)
  {
    if(x->floatval() < 0.0)
      return T;
    return NIL;
  }
  if(type_of(x) == type::Integer)
  {
    if(x->intval() < 0)
      return T;
    return NIL;
  }
  return ctx.error(error_errc::illegal_arg, x);
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
  mkprim(pn::ABS,         abs,         subr_t::subr::EVAL, subr_t::spread::SPREAD);
  // clang-format on
}

} // namespace lisp::details::arith
