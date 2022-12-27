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

#include "alloc.hh"
#include "arith.hh"
#include "check.hh"
#include "iter.hh"

namespace lisp::details::arith
{
//
// Functions with an i as a prefix are for integer arithmetic and those whith
// an f are for floats.  Without a prefix, the functions are generic and
// converts arguments to the apropriate type.  Any float as an argument to a
// generic function results in a float.
//

lisp_t plus(context& ctx, lisp_t x)
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

lisp_t iplus(context&, lisp_t x)
{
  int sum = 0;
  for(auto i: x)
  {
    check(i, type::Integer);
    sum += i->intval();
  }
  return mknumber(sum);
}

lisp_t fplus(context&, lisp_t x)
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

lisp_t difference(context&, lisp_t x, lisp_t y)
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

lisp_t idifference(context&, lisp_t x, lisp_t y)
{
  check(x, type::Integer);
  check(y, type::Integer);
  return mknumber(x->intval() - y->intval());
}

lisp_t fdifference(context&, lisp_t x, lisp_t y)
{
  check(x, type::Float);
  check(y, type::Float);
  return mkfloat(x->floatval() - y->floatval());
}

lisp_t ltimes(context& ctx, lisp_t x)
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

lisp_t itimes(context&, lisp_t x)
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

lisp_t ftimes(context&, lisp_t x)
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

lisp_t divide(context& ctx, lisp_t x, lisp_t y)
{
  check(x, type::Integer, type::Float);
  check(y, type::Integer, type::Float);
  if(type_of(x) == type::Integer)
    if(type_of(y) == type::Integer)
    {
      if(y->intval() == 0)
        ctx.error(error_errc::divide_by_zero, nil);
      return mknumber(x->intval() / y->intval());
    }
    else
    {
      if(y->floatval() == 0.0)
        ctx.error(error_errc::divide_by_zero, nil);
      return mkfloat((double)x->intval() / y->floatval());
    }
  else if(type_of(y) == type::Integer)
  {
    if(y->intval() == 0)
      ctx.error(error_errc::divide_by_zero, nil);
    return mkfloat(x->floatval() / static_cast<double>(y->intval()));
  }
  if(y->floatval() == 0.0)
    ctx.error(error_errc::divide_by_zero, nil);
  return mkfloat(x->floatval() / y->floatval());
}

lisp_t iquotient(context& ctx, lisp_t x, lisp_t y)
{
  check(x, type::Integer);
  check(y, type::Integer);
  if(y->intval() == 0)
    ctx.error(error_errc::divide_by_zero, nil);
  return mknumber(x->intval() / y->intval());
}

lisp_t iremainder(context& ctx, lisp_t x, lisp_t y)
{
  check(x, type::Integer);
  check(y, type::Integer);
  if(y->intval() == 0)
    ctx.error(error_errc::divide_by_zero, nil);
  return mknumber(x->intval() % y->intval());
}

lisp_t fdivide(context& ctx, lisp_t x, lisp_t y)
{
  check(x, type::Float);
  check(y, type::Float);
  if(y->floatval() == 0.0)
    ctx.error(error_errc::divide_by_zero, nil);
  return mkfloat(x->floatval() / y->floatval());
}

lisp_t minus(context&, lisp_t x)
{
  check(x, type::Float, type::Integer);
  if(type_of(x) == type::Integer)
    return mknumber(-x->intval());
  return mkfloat(-x->floatval());
}

lisp_t iminus(context&, lisp_t x)
{
  check(x, type::Integer);
  return mknumber(-x->intval());
}

lisp_t abs(context&, lisp_t x)
{
  check(x, type::Integer);
  if(x->intval() < 0)
    return mknumber(-x->intval());
  return mknumber(x->intval());
}

lisp_t itof(context&, lisp_t x)
{
  check(x, type::Integer);
  return mkfloat(static_cast<double>(x->intval()));
}

lisp_t add1(context&, lisp_t x)
{
  check(x, type::Integer);
  return mknumber(x->intval() + 1);
}

lisp_t sub1(context&, lisp_t x)
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

inline num_type numtype(lisp_t x, lisp_t y)
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
inline lisp_t docheck(Type x, Type y, Comparor cmp)
{
  if(cmp(x, y))
    return T;
  return nil;
}

inline lisp_t illegalreturn(context& ctx, lisp_t x) { return ctx.error(error_errc::illegal_arg, x); }

template<template<typename> typename Comparer>
inline lisp_t numcheck(context& ctx, lisp_t x, lisp_t y)
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

lisp_t greaterp(context& ctx, lisp_t x, lisp_t y) { return numcheck<std::greater>(ctx, x, y); }

lisp_t lessp(context& ctx, lisp_t x, lisp_t y) { return numcheck<std::less>(ctx, x, y); }

lisp_t eqp(context& ctx, lisp_t x, lisp_t y) { return numcheck<std::equal_to>(ctx, x, y); }

lisp_t geq(context& ctx, lisp_t x, lisp_t y) { return numcheck<std::greater_equal>(ctx, x, y); }

lisp_t leq(context& ctx, lisp_t x, lisp_t y) { return numcheck<std::less_equal>(ctx, x, y); }

lisp_t neqp(context& ctx, lisp_t x, lisp_t y) { return numcheck<std::not_equal_to>(ctx, x, y); }

lisp_t zerop(context&, lisp_t x)
{
  if(type_of(x) == type::Integer && x->intval() == 0)
    return T;
  return nil;
}

lisp_t minusp(context& ctx, lisp_t x)
{
  if(type_of(x) == type::Float)
  {
    if(x->floatval() < 0.0)
      return T;
    return nil;
  }
  if(type_of(x) == type::Integer)
  {
    if(x->intval() < 0)
      return T;
    return nil;
  }
  return ctx.error(error_errc::illegal_arg, x);
}

namespace pn
{
inline constexpr std::string_view PLUS = "plus";               // addition
inline constexpr std::string_view DIFFERENCE = "difference";   // subtraction
inline constexpr std::string_view TIMES = "times";             // multiplication
inline constexpr std::string_view DIVIDE = "divide";           // division
inline constexpr std::string_view IPLUS = "iplus";             // integer addition
inline constexpr std::string_view IDIFFERENCE = "idifference"; // integer subtraction
inline constexpr std::string_view ITIMES = "itimes";           // integer multiplication
inline constexpr std::string_view IQUOTIENT = "iquotient";     // integer division
inline constexpr std::string_view IREMAINDER = "iremainder";   // integer mod
inline constexpr std::string_view IMINUS = "iminus";           // integer change sign
inline constexpr std::string_view MINUS = "minus";             // change sign mixed
inline constexpr std::string_view ADD1 = "add1";               // add one
inline constexpr std::string_view SUB1 = "sub1";               // subtract one
inline constexpr std::string_view ABS = "abs";                 // absolute value
inline constexpr std::string_view FPLUS = "fplus";             // float addition
inline constexpr std::string_view FDIFFERENCE = "fdifference"; // float subtraction
inline constexpr std::string_view FTIMES = "ftimes";           // float multiplication
inline constexpr std::string_view FDIVIDE = "fdivide";         // float division
inline constexpr std::string_view ITOF = "itof";               // integer to float
inline constexpr std::string_view GREATERP = "greaterp";       // t if greater than
inline constexpr std::string_view GEQ = "geq";                 // t if greater or eq
inline constexpr std::string_view LESSP = "lessp";             // t if less than
inline constexpr std::string_view LEQ = "leq";                 // t if less or eq
inline constexpr std::string_view ZEROP = "zerop";             // t if eq to 0
inline constexpr std::string_view EQP = "eqp";                 // t if eq or numerically equal
inline constexpr std::string_view NEQP = "neqp";               // not eqp
inline constexpr std::string_view MINUSP = "minusp";           // t if negative
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
