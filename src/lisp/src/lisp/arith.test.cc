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
  std::ostringstream os;
  auto of = std::make_unique<file_t>(os);
  l.primerr(std::move(of));

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

  SUBCASE("greaterp")
  {
    CHECK(is_T(greaterp(l, mknumber(2), mknumber(1))));
    CHECK(is_NIL(greaterp(l, mknumber(1), mknumber(2))));
    CHECK(is_T(greaterp(l, mkfloat(2.0), mknumber(1))));
    CHECK(is_NIL(greaterp(l, mkfloat(1.0), mknumber(2))));
    CHECK(is_T(greaterp(l, mknumber(2), mkfloat(1.0))));
    CHECK(is_NIL(greaterp(l, mknumber(1), mkfloat(2.0))));
    CHECK(is_T(greaterp(l, mkfloat(2.0), mkfloat(1.0))));
    CHECK(is_NIL(greaterp(l, mkfloat(1.0), mkfloat(2.0))));
    CHECK(is_NIL(greaterp(mkfloat(1.0), mkfloat(2.0))));
    CHECK_THROWS(eval(l, "(greaterp \"a\" 1)"));
    CHECK_THROWS(eval(l, "(greaterp 1 \"b\")"));
  }

  SUBCASE("lessp")
  {
    CHECK(is_NIL(lessp(l, mknumber(2), mknumber(1))));
    CHECK(is_T(lessp(l, mknumber(1), mknumber(2))));
    CHECK(is_NIL(lessp(l, mkfloat(2.0), mknumber(1))));
    CHECK(is_T(lessp(l, mkfloat(1.0), mknumber(2))));
    CHECK(is_NIL(lessp(l, mknumber(2), mkfloat(1.0))));
    CHECK(is_T(lessp(l, mknumber(1), mkfloat(2.0))));
    CHECK(is_NIL(lessp(l, mkfloat(2.0), mkfloat(1.0))));
    CHECK(is_T(lessp(l, mkfloat(1.0), mkfloat(2.0))));
    CHECK(is_T(lessp(mkfloat(1.0), mkfloat(2.0))));
    CHECK_THROWS(eval(l, "(lessp \"a\" 1)"));
    CHECK_THROWS(eval(l, "(lessp 1 \"b\")"));
  }

  SUBCASE("eqp")
  {
    CHECK(is_NIL(eqp(l, mknumber(2), mknumber(1))));
    CHECK(is_NIL(eqp(l, mknumber(1), mknumber(2))));
    CHECK(is_NIL(eqp(l, mkfloat(2.0), mknumber(1))));
    CHECK(is_NIL(eqp(l, mkfloat(1.0), mknumber(2))));
    CHECK(is_NIL(eqp(l, mknumber(2), mkfloat(1.0))));
    CHECK(is_NIL(eqp(l, mknumber(1), mkfloat(2.0))));
    CHECK(is_NIL(eqp(l, mkfloat(2.0), mkfloat(1.0))));
    CHECK(is_NIL(eqp(l, mkfloat(1.0), mkfloat(2.0))));
    CHECK(is_T(eqp(l, mknumber(1), mknumber(1))));
    CHECK(is_T(eqp(mknumber(1), mknumber(1))));
    CHECK_THROWS(eval(l, "(eqp \"a\" 1)"));
    CHECK_THROWS(eval(l, "(eqp 1 \"b\")"));
  }

  SUBCASE("geq")
  {
    CHECK(is_T(geq(l, mknumber(2), mknumber(1))));
    CHECK(is_NIL(geq(l, mknumber(1), mknumber(2))));
    CHECK(is_T(geq(l, mkfloat(2.0), mknumber(1))));
    CHECK(is_NIL(geq(l, mkfloat(1.0), mknumber(2))));
    CHECK(is_T(geq(l, mknumber(2), mkfloat(1.0))));
    CHECK(is_NIL(geq(l, mknumber(1), mkfloat(2.0))));
    CHECK(is_T(geq(l, mkfloat(2.0), mkfloat(1.0))));
    CHECK(is_NIL(geq(l, mkfloat(1.0), mkfloat(2.0))));
    CHECK(is_T(geq(l, mknumber(1), mknumber(1))));
    CHECK(is_T(geq(mknumber(1), mknumber(1))));
    CHECK_THROWS(eval(l, "(geq \"a\" 1)"));
    CHECK_THROWS(eval(l, "(geq 1 \"b\")"));
  }

  SUBCASE("leq")
  {
    CHECK(is_NIL(leq(l, mknumber(2), mknumber(1))));
    CHECK(is_T(leq(l, mknumber(1), mknumber(2))));
    CHECK(is_NIL(leq(l, mkfloat(2.0), mknumber(1))));
    CHECK(is_T(leq(l, mkfloat(1.0), mknumber(2))));
    CHECK(is_NIL(leq(l, mknumber(2), mkfloat(1.0))));
    CHECK(is_T(leq(l, mknumber(1), mkfloat(2.0))));
    CHECK(is_NIL(leq(l, mkfloat(2.0), mkfloat(1.0))));
    CHECK(is_T(leq(l, mkfloat(1.0), mkfloat(2.0))));
    CHECK(is_T(leq(l, mknumber(1), mknumber(1))));
    CHECK(is_T(leq(mknumber(1), mknumber(1))));
    CHECK_THROWS(eval(l, "(leq \"a\" 1)"));
    CHECK_THROWS(eval(l, "(leq 1 \"b\")"));
  }

  SUBCASE("neqp")
  {
  }

  SUBCASE("zerop")
  {
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

  SUBCASE("minusp")
  {
    auto r = eval(l, "(minusp -5)");
    CHECK(is_T(r));
  }

}

}
