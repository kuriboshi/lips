//
// Lips, lisp shell.
// Copyright 1988, 2020-2023 Krister Joas
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

lisp_t plus(lisp_t x)
{
  double fsum = 0.0;
  std::int64_t sum = 0;
  bool f = false;

  while(type_of(x) == object::type::Cons)
  {
    if(f)
    {
      if(type_of(x->car()) == object::type::Integer)
        fsum += static_cast<double>(x->car()->as_integer());
      else if(type_of(x->car()) == object::type::Float)
        fsum += x->car()->as_double();
      else
        error(error_errc::illegal_arg, x->car());
    }
    else
    {
      if(type_of(x->car()) == object::type::Integer)
        sum += x->car()->as_integer();
      else if(type_of(x->car()) == object::type::Float)
      {
        f = true;
        fsum = x->car()->as_double() + static_cast<double>(sum);
      }
      else
        error(error_errc::illegal_arg, x->car());
    }
    x = x->cdr();
  }
  if(f)
    return mkfloat(fsum);
  return mknumber(sum);
}

lisp_t iplus(lisp_t x)
{
  std::int64_t sum = 0;
  for(auto i: x)
  {
    check(i, object::type::Integer);
    sum += i->as_integer();
  }
  return mknumber(sum);
}

lisp_t fplus(lisp_t x)
{
  check(x->car(), object::type::Float);
  auto fsum = x->car()->as_double();
  x = x->cdr();
  while(type_of(x) == object::type::Cons)
  {
    check(x->car(), object::type::Float);
    fsum = fsum + x->car()->as_double();
    x = x->cdr();
  }
  return mkfloat(fsum);
}

lisp_t difference(lisp_t x, lisp_t y)
{
  check(x, object::type::Integer, object::type::Float);
  check(y, object::type::Integer, object::type::Float);
  if(type_of(x) == object::type::Integer)
  {
    if(type_of(y) == object::type::Integer)
      return mknumber(x->as_integer() - y->as_integer());
    return mkfloat(static_cast<double>(x->as_integer()) - y->as_double());
  }
  if(type_of(y) == object::type::Integer)
    return mkfloat(x->as_double() - static_cast<double>(y->as_integer()));
  return mkfloat(x->as_double() - y->as_double());
}

lisp_t idifference(lisp_t x, lisp_t y)
{
  check(x, object::type::Integer);
  check(y, object::type::Integer);
  return mknumber(x->as_integer() - y->as_integer());
}

lisp_t fdifference(lisp_t x, lisp_t y)
{
  check(x, object::type::Float);
  check(y, object::type::Float);
  return mkfloat(x->as_double() - y->as_double());
}

lisp_t times(lisp_t x)
{
  double fprod = 1.0;
  std::int64_t prod = 1;
  bool f = false;

  while(type_of(x) == object::type::Cons)
  {
    if(f)
    {
      if(type_of(x->car()) == object::type::Integer)
        fprod *= (double)x->car()->as_integer();
      else if(type_of(x->car()) == object::type::Float)
        fprod *= x->car()->as_double();
      else
        error(error_errc::illegal_arg, x->car());
    }
    else if(type_of(x->car()) == object::type::Integer)
      prod *= x->car()->as_integer();
    else if(type_of(x->car()) == object::type::Float)
    {
      f = true;
      fprod = x->car()->as_double() * (double)prod;
    }
    else
      error(error_errc::illegal_arg, x->car());
    x = x->cdr();
  }
  if(f)
    return mkfloat(fprod);
  return mknumber(prod);
}

lisp_t itimes(lisp_t x)
{
  check(x->car(), object::type::Integer);
  auto prod = x->car()->as_integer();
  x = x->cdr();
  while(type_of(x) == object::type::Cons)
  {
    check(x->car(), object::type::Integer);
    prod = prod * x->car()->as_integer();
    x = x->cdr();
  }
  return mknumber(prod);
}

lisp_t ftimes(lisp_t x)
{
  check(x->car(), object::type::Float);
  auto prod = x->car()->as_double();
  x = x->cdr();
  while(type_of(x) == object::type::Cons)
  {
    check(x->car(), object::type::Float);
    prod = prod * x->car()->as_double();
    x = x->cdr();
  }
  return mkfloat(prod);
}

lisp_t divide(lisp_t x, lisp_t y)
{
  check(x, object::type::Integer, object::type::Float);
  check(y, object::type::Integer, object::type::Float);
  if(type_of(x) == object::type::Integer)
    if(type_of(y) == object::type::Integer)
    {
      if(y->as_integer() == 0)
        error(error_errc::divide_by_zero, nil);
      return mknumber(x->as_integer() / y->as_integer());
    }
    else
    {
      if(y->as_double() == 0.0)
        error(error_errc::divide_by_zero, nil);
      return mkfloat((double)x->as_integer() / y->as_double());
    }
  else if(type_of(y) == object::type::Integer)
  {
    if(y->as_integer() == 0)
      error(error_errc::divide_by_zero, nil);
    return mkfloat(x->as_double() / static_cast<double>(y->as_integer()));
  }
  if(y->as_double() == 0.0)
    error(error_errc::divide_by_zero, nil);
  return mkfloat(x->as_double() / y->as_double());
}

lisp_t iquotient(lisp_t x, lisp_t y)
{
  check(x, object::type::Integer);
  check(y, object::type::Integer);
  if(y->as_integer() == 0)
    error(error_errc::divide_by_zero, nil);
  return mknumber(x->as_integer() / y->as_integer());
}

lisp_t iremainder(lisp_t x, lisp_t y)
{
  check(x, object::type::Integer);
  check(y, object::type::Integer);
  if(y->as_integer() == 0)
    error(error_errc::divide_by_zero, nil);
  return mknumber(x->as_integer() % y->as_integer());
}

lisp_t fdivide(lisp_t x, lisp_t y)
{
  check(x, object::type::Float);
  check(y, object::type::Float);
  if(y->as_double() == 0.0)
    error(error_errc::divide_by_zero, nil);
  return mkfloat(x->as_double() / y->as_double());
}

lisp_t minus(lisp_t x)
{
  check(x, object::type::Float, object::type::Integer);
  if(type_of(x) == object::type::Integer)
    return mknumber(-x->as_integer());
  return mkfloat(-x->as_double());
}

lisp_t iminus(lisp_t x)
{
  check(x, object::type::Integer);
  return mknumber(-x->as_integer());
}

lisp_t abs(lisp_t x)
{
  check(x, object::type::Integer);
  if(x->as_integer() < 0)
    return mknumber(-x->as_integer());
  return mknumber(x->as_integer());
}

lisp_t itof(lisp_t x)
{
  check(x, object::type::Integer);
  return mkfloat(static_cast<double>(x->as_integer()));
}

lisp_t add1(lisp_t x)
{
  check(x, object::type::Integer);
  return mknumber(x->as_integer() + 1);
}

lisp_t sub1(lisp_t x)
{
  check(x, object::type::Integer);
  return mknumber(x->as_integer() - 1);
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
  if(type_of(x) == object::type::Float)
  {
    if(type_of(y) == object::type::Float)
      return num_type::FLOATFLOAT;
    if(type_of(y) == object::type::Integer)
      return num_type::FLOATINT;
    return num_type::ILLEGAL2;
  }
  if(type_of(x) == object::type::Integer)
  {
    if(type_of(y) == object::type::Float)
      return num_type::INTFLOAT;
    if(type_of(y) == object::type::Integer)
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

inline lisp_t illegalreturn(lisp_t x) { return error(error_errc::illegal_arg, x); }

template<template<typename> typename Comparer>
inline lisp_t numcheck(lisp_t x, lisp_t y)
{
  switch(numtype(x, y))
  {
    case num_type::FLOATFLOAT:
      return docheck(x->as_double(), y->as_double(), Comparer<double>());
    case num_type::FLOATINT:
      return docheck(x->as_double().value(), static_cast<double>(y->as_integer()), Comparer<double>());
    case num_type::INTFLOAT:
      return docheck(static_cast<double>(x->as_integer()), y->as_double().value(), Comparer<double>());
    case num_type::INTINT:
      return docheck(x->as_integer(), y->as_integer(), Comparer<integer_t::value_type>());
    case num_type::ILLEGAL1:
      return illegalreturn(x);
    default:
      return illegalreturn(y);
  }
}

lisp_t greaterp(lisp_t x, lisp_t y) { return numcheck<std::greater>(x, y); }

lisp_t lessp(lisp_t x, lisp_t y) { return numcheck<std::less>(x, y); }

lisp_t eqp(lisp_t x, lisp_t y) { return numcheck<std::equal_to>(x, y); }

lisp_t geq(lisp_t x, lisp_t y) { return numcheck<std::greater_equal>(x, y); }

lisp_t leq(lisp_t x, lisp_t y) { return numcheck<std::less_equal>(x, y); }

lisp_t neqp(lisp_t x, lisp_t y) { return numcheck<std::not_equal_to>(x, y); }

lisp_t zerop(lisp_t x)
{
  if(type_of(x) == object::type::Integer && x->as_integer() == 0)
    return T;
  return nil;
}

lisp_t minusp(lisp_t x)
{
  if(type_of(x) == object::type::Float)
  {
    if(x->as_double() < 0.0)
      return T;
    return nil;
  }
  if(type_of(x) == object::type::Integer)
  {
    if(x->as_integer() < 0)
      return T;
    return nil;
  }
  return error(error_errc::illegal_arg, x);
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
  mkprim(pn::TIMES,       times,       subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
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
