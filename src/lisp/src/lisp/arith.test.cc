//
// Lips, lisp shell.
// Copyright 2021 Krister Joas
//
#include <doctest/doctest.h>
#include "libisp.hh"

namespace lisp
{

TEST_CASE("Arithmetic functions")
{
  lisp l;
  current c(l);

  SUBCASE("+")
  {
    auto r0 = eval(l, "(+ 1 2 3 4 5)");
    REQUIRE(type_of(r0) == type::INTEGER);
    CHECK(r0->intval() == 15);
    auto r1 = eval(l, "(+ 1 1.0 2.0)");
    REQUIRE(type_of(r1) == type::FLOAT);
    CHECK(r1->floatval() == 4.0);
    auto i2 = mknumber(1);
    auto r2 = plus(cons(i2, cons(i2, NIL)));
    REQUIRE(type_of(r2) == type::INTEGER);
    CHECK(r2->intval() == 2);
    auto i3 = mknumber(1);
    auto f3 = mkfloat(1.0);
    auto r3 = plus(cons(f3, cons(i3, NIL)));
    REQUIRE(type_of(r3) == type::FLOAT);
    CHECK(r3->floatval() == 2);
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
    REQUIRE(type_of(r) == type::FLOAT);
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

}
