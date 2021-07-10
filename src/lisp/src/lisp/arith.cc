/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include <doctest/doctest.h>
#include "libisp.hh"

namespace lisp
{
arith::arith(): base() {}
arith::arith(lisp& lisp): base(lisp) {}

/*
 * Functions with an i as a prefix are for integer arithmetic and those whith
 * an f are for floats.  Without a prefix, the functions are generic and
 * converts arguments to the apropriate type.  Any float as an argument to a
 * generic function results in a float.
 */

PRIMITIVE arith::plus(LISPT x)
{
  double fsum = 0.0;
  int sum = 0;
  int f = 0;

  while(type_of(x) == type::CONS)
  {
    if(f)
    {
      if(type_of(x->car()) == type::INTEGER)
        fsum += (double)x->car()->intval();
      else if(type_of(x->car()) == type::FLOAT)
        fsum += x->car()->floatval();
      else
        return l.error(ILLEGAL_ARG, x->car());
    }
    else if(type_of(x->car()) == type::INTEGER)
      sum += x->car()->intval();
    else if(type_of(x->car()) == type::FLOAT)
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
    l.check(*i, type::INTEGER);
    sum += (**i).intval();
  }
  return mknumber(l, sum);
}

PRIMITIVE arith::fplus(LISPT x)
{
  l.check(x->car(), type::FLOAT);
  auto sum = x->car()->floatval();
  x = x->cdr();
  while(type_of(x) == type::CONS)
  {
    l.check(x->car(), type::FLOAT);
    sum = sum + x->car()->floatval();
    x = x->cdr();
  }
  return mkfloat(l, sum);
}

PRIMITIVE arith::difference(LISPT x, LISPT y)
{
  l.check(x, type::INTEGER, type::FLOAT);
  l.check(y, type::INTEGER, type::FLOAT);
  if(type_of(x) == type::INTEGER)
    if(type_of(y) == type::INTEGER)
      return mknumber(l, x->intval() - y->intval());
    else
      return mkfloat(l, (double)x->intval() - y->floatval());
  else if(type_of(y) == type::INTEGER)
    return mkfloat(l, x->floatval() - (double)y->intval());
  return mkfloat(l, x->floatval() - y->floatval());
}

PRIMITIVE arith::idifference(LISPT x, LISPT y)
{
  l.check(x, type::INTEGER);
  l.check(y, type::INTEGER);
  return mknumber(l, x->intval() - y->intval());
}

PRIMITIVE arith::fdifference(LISPT x, LISPT y)
{
  l.check(x, type::FLOAT);
  l.check(y, type::FLOAT);
  return mkfloat(l, x->floatval() - y->floatval());
}

PRIMITIVE arith::ltimes(LISPT x)
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

PRIMITIVE arith::itimes(LISPT x)
{
  l.check(x->car(), type::INTEGER);
  auto prod = x->car()->intval();
  x = x->cdr();
  while(type_of(x) == type::CONS)
  {
    l.check(x->car(), type::INTEGER);
    prod = prod * x->car()->intval();
    x = x->cdr();
  }
  return mknumber(l, prod);
}

PRIMITIVE arith::ftimes(LISPT x)
{
  l.check(x->car(), type::FLOAT);
  auto prod = x->car()->floatval();
  x = x->cdr();
  while(type_of(x) == type::CONS)
  {
    l.check(x->car(), type::FLOAT);
    prod = prod * x->car()->floatval();
    x = x->cdr();
  }
  return mkfloat(l, prod);
}

PRIMITIVE arith::divide(LISPT x, LISPT y)
{
  l.check(x, type::INTEGER, type::FLOAT);
  l.check(y, type::INTEGER, type::FLOAT);
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

PRIMITIVE arith::iquotient(LISPT x, LISPT y)
{
  l.check(x, type::INTEGER);
  l.check(y, type::INTEGER);
  if(y->intval() == 0)
    return l.error(DIVIDE_ZERO, NIL);
  return mknumber(l, x->intval() / y->intval());
}

PRIMITIVE arith::iremainder(LISPT x, LISPT y)
{
  l.check(x, type::INTEGER);
  l.check(y, type::INTEGER);
  if(y->intval() == 0)
    return l.error(DIVIDE_ZERO, NIL);
  return mknumber(l, x->intval() % y->intval());
}

PRIMITIVE arith::fdivide(LISPT x, LISPT y)
{
  l.check(x, type::FLOAT);
  l.check(y, type::FLOAT);
  if(y->floatval() == 0.0)
    return l.error(DIVIDE_ZERO, NIL);
  return mkfloat(l, x->floatval() / y->floatval());
}

PRIMITIVE arith::minus(LISPT x)
{
  l.check(x, type::FLOAT, type::INTEGER);
  if(type_of(x) == type::INTEGER)
    return mknumber(l, -x->intval());
  return mkfloat(l, -x->floatval());
}

PRIMITIVE arith::iminus(LISPT x)
{
  l.check(x, type::INTEGER);
  return mknumber(l, -x->intval());
}

PRIMITIVE arith::absval(LISPT x)
{
  int sign;

  l.check(x, type::INTEGER);
  if(x->intval() < 0)
    sign = -1;
  else
    sign = 1;
  return mknumber(l, x->intval() * sign);
}

PRIMITIVE arith::itof(LISPT x)
{
  l.check(x, type::INTEGER);
  return mkfloat(l, (double)x->intval());
}

PRIMITIVE arith::add1(LISPT x)
{
  l.check(x, type::INTEGER);
  return mknumber(l, x->intval() + 1);
}

PRIMITIVE arith::sub1(LISPT x)
{
  l.check(x, type::INTEGER);
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
    else if(type_of(y) == type::INTEGER)
      return num_type::FLOATINT;
    return num_type::ILLEGAL2;
  }
  else if(type_of(x) == type::INTEGER)
  {
    if(type_of(y) == type::FLOAT)
      return num_type::INTFLOAT;
    else if(type_of(y) == type::INTEGER)
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
  return NIL;
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
  return l.error(BUG, NIL);
}

PRIMITIVE arith::greaterp(LISPT x, LISPT y) { return numcheck<std::greater>(l, x, y); }

PRIMITIVE arith::lessp(LISPT x, LISPT y) { return numcheck<std::less>(l, x, y); }

PRIMITIVE arith::eqp(LISPT x, LISPT y) { return numcheck<std::equal_to>(l, x, y); }

PRIMITIVE arith::geq(LISPT x, LISPT y) { return numcheck<std::greater_equal>(l, x, y); }

PRIMITIVE arith::leq(LISPT x, LISPT y) { return numcheck<std::less_equal>(l, x, y); }

PRIMITIVE arith::neqp(LISPT x, LISPT y) { return numcheck<std::not_equal_to>(l, x, y); }

PRIMITIVE arith::zerop(LISPT x)
{
  if(type_of(x) == type::INTEGER && x->intval() == 0)
    return C_T;
  return NIL;
}

PRIMITIVE arith::minusp(LISPT x)
{
  if(type_of(x) == type::FLOAT)
  {
    if(x->floatval() < 0.0)
      return C_T;
    else
      return NIL;
  }
  else if(type_of(x) == type::INTEGER)
  {
    if(x->intval() < 0)
      return C_T;
    else
      return NIL;
  }
  return l.error(ILLEGAL_ARG, x);
}

namespace pn
{
inline constexpr auto PLUS = "+";            // add
inline constexpr auto DIFFERENCE = "-";      // subtract
inline constexpr auto TIMES = "*";           // multiply
inline constexpr auto DIVIDE = "/";          // divide
inline constexpr auto IPLUS = "i+";          // integer add
inline constexpr auto IDIFFERENCE = "i-";    // integer subtract
inline constexpr auto ITIMES = "i*";         // integer multiply
inline constexpr auto IQUOTIENT = "i/";      // integer divide
inline constexpr auto IREMAINDER = "i%";     // integer mod
inline constexpr auto IMINUS = "iminus";     // integer change sign
inline constexpr auto MINUS = "minus";       // change sign generic
inline constexpr auto ADD1 = "add1";         // add one
inline constexpr auto SUB1 = "sub1";         // subtract one
inline constexpr auto ABS = "abs";           // absolute value
inline constexpr auto FPLUS = "f+";          // float add
inline constexpr auto FDIFFERENCE = "f-";    // float subtract
inline constexpr auto FTIMES = "f*";         // float multiply
inline constexpr auto FDIVIDE = "f/";        // float divide
inline constexpr auto ITOF = "itof";         // integer to float
inline constexpr auto GREATERP = "greaterp"; // t if greater than
inline constexpr auto GEQ = "geq";           // t if greater or eq
inline constexpr auto LESSP = "lessp";       // less than
inline constexpr auto LEQ = "leq";           // less or eq
inline constexpr auto ZEROP = "zerop";       // t if eq to 0
inline constexpr auto EQP = "eqp";           // number eq
inline constexpr auto NEQP = "neqp";         // not eqp
inline constexpr auto MINUSP = "minusp";     // t if negative
} // namespace pn

void arith::init()
{
  // clang-format off
  mkprim(pn::PLUS,        ::lisp::plus,        subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::DIFFERENCE,  ::lisp::difference,  subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::TIMES,       ::lisp::ltimes,      subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::DIVIDE,      ::lisp::divide,      subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::IPLUS,       ::lisp::iplus,       subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::IDIFFERENCE, ::lisp::idifference, subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::ITIMES,      ::lisp::itimes,      subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::IQUOTIENT,   ::lisp::iquotient,   subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::IREMAINDER,  ::lisp::iremainder,  subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::IMINUS,      ::lisp::iminus,      subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::MINUS,       ::lisp::minus,       subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::ADD1,        ::lisp::add1,        subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::SUB1,        ::lisp::sub1,        subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::FPLUS,       ::lisp::fplus,       subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::FDIFFERENCE, ::lisp::fdifference, subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::FTIMES,      ::lisp::ftimes,      subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::FDIVIDE,     ::lisp::fdivide,     subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::ITOF,        ::lisp::itof,        subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::GREATERP,    ::lisp::greaterp,    subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::GEQ,         ::lisp::geq,         subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::LESSP,       ::lisp::lessp,       subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::LEQ,         ::lisp::leq,         subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::ZEROP,       ::lisp::zerop,       subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::EQP,         ::lisp::eqp,         subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::NEQP,        ::lisp::neqp,        subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::MINUSP,      ::lisp::minusp,      subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::ABS,         ::lisp::absval,      subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  // clang-format on
}

TEST_CASE("Arithmetic functions")
{
  lisp l;
  SUBCASE("+")
  {
    auto r = eval(l, "(+ 1 2 3 4 5)");
    CHECK(r->intval() == 15);
  }
  SUBCASE("-")
  {
    auto r = eval(l, "(- 1 2)");
    CHECK(r->intval() == -1);
  }
  SUBCASE("*")
  {
    auto r = eval(l, "(* 5 7)");
    CHECK(r->intval() == 35);
  }
  SUBCASE("/ 1")
  {
    auto r = eval(l, "(/ 4 2)");
    CHECK(r->intval() == 2);
  }
  SUBCASE("/ 2")
  {
    auto r = eval(l, "(/ 4 (itof 2))");
    CHECK(r->floatval() == 2.0);
  }
  SUBCASE("i+")
  {
    auto r = eval(l, "(i+ 1 2 7)");
    CHECK(r->intval() == 10);
  }
  SUBCASE("i-")
  {
    auto r = eval(l, "(i- 13 2)");
    CHECK(r->intval() == 11);
  }
  SUBCASE("i*")
  {
    auto r = eval(l, "(i* 6 8)");
    CHECK(r->intval() == 48);
  }
  SUBCASE("i/")
  {
    auto r = eval(l, "(i/ 5 2)");
    CHECK(r->intval() == 2);
  }
  SUBCASE("i%")
  {
    auto r = eval(l, "(i% 5 2)");
    CHECK(r->intval() == 1);
  }

  SUBCASE("iminus")
  {
    auto r = eval(l, "(iminus 1)");
    CHECK(r->intval() == -1);
  }
  SUBCASE("minus")
  {
    auto r = eval(l, "(minus (itof 1))");
    CHECK(r->floatval() == -1.0);
  }
  SUBCASE("add1")
  {
    auto r = eval(l, "(add1 2)");
    CHECK(r->intval() == 3);
  }
  SUBCASE("sub1")
  {
    auto r = eval(l, "(sub1 3)");
    CHECK(r->intval() == 2);
  }
  SUBCASE("abs")
  {
    auto r = eval(l, "(abs -1)");
    CHECK(r->intval() == 1);
  }
  SUBCASE("f+")
  {
    auto r = eval(l, "(f+ (itof 5) (itof 2))");
    CHECK(type_of(r) == type::FLOAT);
    CHECK(r->floatval() == 7.0);
  }
  SUBCASE("f-")
  {
    auto r = eval(l, "(f- (itof 5) (itof 2))");
    CHECK(type_of(r) == type::FLOAT);
    CHECK(r->floatval() == 3.0);
  }
  SUBCASE("f*")
  {
    auto r = eval(l, "(f* (itof 5) (itof 2))");
    CHECK(type_of(r) == type::FLOAT);
    CHECK(r->floatval() == 10.0);
  }
  SUBCASE("f/")
  {
    auto r = eval(l, "(f/ (itof 5) (itof 2))");
    CHECK(type_of(r) == type::FLOAT);
    CHECK(r->floatval() == 2.5);
  }
  SUBCASE("itof")
  {
    auto r = eval(l, "(itof 8)");
    CHECK(type_of(r) == type::FLOAT);
    CHECK(r->floatval() == 8.0);
  }

  SUBCASE("greaterp 1")
  {
    auto r = eval(l, "(greaterp 5 2)");
    CHECK(is_T(r));
  }
  SUBCASE("greaterp 2")
  {
    auto r = eval(l, "(greaterp 5 5)");
    CHECK(is_NIL(r));
  }
  SUBCASE("geq 1")
  {
    auto r = eval(l, "(geq 5 2)");
    CHECK(is_T(r));
  }
  SUBCASE("geq 2")
  {
    auto r = eval(l, "(geq 5 5)");
    CHECK(is_T(r));
  }
  SUBCASE("lessp 1")
  {
    auto r = eval(l, "(lessp 5 2)");
    CHECK(is_NIL(r));
  }
  SUBCASE("lessp 2")
  {
    auto r = eval(l, "(lessp 2 5)");
    CHECK(is_T(r));
  }
  SUBCASE("leq 1")
  {
    auto r = eval(l, "(leq 5 2)");
    CHECK(is_NIL(r));
  }
  SUBCASE("leq 2")
  {
    auto r = eval(l, "(leq 2 5)");
    CHECK(is_T(r));
  }
  SUBCASE("leq 3")
  {
    auto r = eval(l, "(leq 2 2)");
    CHECK(is_T(r));
  }
  SUBCASE("zerop 1")
  {
    auto r = eval(l, "(zerop 0)");
    CHECK(is_T(r));
  }
  SUBCASE("zerop 2")
  {
    auto r = eval(l, "(zerop 1)");
    CHECK(is_NIL(r));
  }
  SUBCASE("eqp")
  {
    auto r = eval(l, "(eqp 5 2)");
    CHECK(is_NIL(r));
  }
  SUBCASE("neqp")
  {
    auto r = eval(l, "(neqp 5 2)");
    CHECK(is_T(r));
  }
  SUBCASE("minusp")
  {
    auto r = eval(l, "(minusp -5)");
    CHECK(is_T(r));
  }
}

} // namespace lisp
