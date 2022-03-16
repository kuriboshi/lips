//
// Lips, lisp shell.
// Copyright 2021 Krister Joas
//
#include <catch2/catch.hpp>
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

  SECTION("+")
  {
    {
      auto r0 = plus(l, mklist(1_l, 2_l, 3_l, 4_l, 5_l));
      REQUIRE(type_of(r0) == type::INTEGER);
      CHECK(r0->intval() == 15);
    }
    {
      auto r0 = plus(l, mklist(1_l, 1.0_l, 2.0_l));
      REQUIRE(type_of(r0) == type::FLOAT);
      CHECK(r0->floatval() == 4.0);
    }
    {
      auto r0 = plus(mklist(1_l, 1_l));
      REQUIRE(type_of(r0) == type::INTEGER);
      CHECK(r0->intval() == 2);
    }
    {
      auto r0 = plus(mklist(1.0_l, 1_l));
      REQUIRE(type_of(r0) == type::FLOAT);
      CHECK(r0->floatval() == 2);
    }
    {
      auto r0 = plus(mklist(1.0_l, 1_l));
      REQUIRE(type_of(r0) == type::FLOAT);
      CHECK(r0->floatval() == 2);
    }
  }

  SECTION("-")
  {
    {
      auto r0 = difference(l, 1_l, 2_l);
      REQUIRE(type_of(r0) == type::INTEGER);
      CHECK(r0->intval() == -1);
    }
    {
      auto r0 = difference(1_l, 2_l);
      REQUIRE(type_of(r0) == type::INTEGER);
      CHECK(r0->intval() == -1);
    }
  }

  SECTION("*")
  {
    auto r0 = ltimes(l, mklist(5_l, 7_l));
    CHECK(r0->intval() == 35);
  }

  SECTION("/")
  {
    {
      auto r0 = divide(l, 4_l, 2_l);
      CHECK(r0->intval() == 2);
    }
    {
      auto r0 = divide(4_l, 2_l);
      CHECK(r0->intval() == 2);
    }
    {
      auto r0 = divide(l, 4_l, 2.0_l);
      CHECK(r0->floatval() == 2.0);
    }
    {
      auto r0 = divide(4_l, 2.0_l);
      CHECK(r0->floatval() == 2.0);
    }
  }

  SECTION("i+")
  {
    {
      auto r0 = iplus(l, mklist(1_l, 2_l, 7_l));
      CHECK(r0->intval() == 10);
    }
    {
      auto r0 = iplus(mklist(1_l, 2_l, 7_l));
      CHECK(r0->intval() == 10);
    }
  }

  SECTION("i-")
  {
    {
      auto r0 = idifference(l, 1_l, 2_l);
      REQUIRE(type_of(r0) == type::INTEGER);
      CHECK(r0->intval() == -1);
    }
    {
      auto r0 = idifference(1_l, 2_l);
      REQUIRE(type_of(r0) == type::INTEGER);
      CHECK(r0->intval() == -1);
    }
  }

  SECTION("i*")
  {
    {
      auto r0 = itimes(l, mklist(6_l, 8_l));
      CHECK(r0->intval() == 48);
    }
    {
      auto r0 = itimes(mklist(6_l, 8_l));
      CHECK(r0->intval() == 48);
    }
  }

  SECTION("i/")
  {
    {
      auto r0 = iquotient(l, 5_l, 2_l);
      CHECK(r0->intval() == 2);
    }
    {
      auto r0 = iquotient(5_l, 2_l);
      CHECK(r0->intval() == 2);
    }
  }

  SECTION("i%")
  {
    {
      auto r0 = iremainder(l, 5_l, 2_l);
      CHECK(r0->intval() == 1);
    }
    {
      auto r0 = iremainder(5_l, 2_l);
      CHECK(r0->intval() == 1);
    }
  }

  SECTION("minus")
  {
    {
      auto r0 = minus(l, 1.0_l);
      CHECK(r0->floatval() == -1.0);
    }
    {
      auto r0 = minus(1.0_l);
      CHECK(r0->floatval() == -1.0);
    }
  }

  SECTION("iminus")
  {
    {
      auto r0 = iminus(l, 1_l);
      CHECK(r0->intval() == -1);
    }
    {
      auto r0 = eval(l, "(iminus 1)");
      CHECK(r0->intval() == -1);
    }
  }

  SECTION("add1")
  {
    {
      auto r = add1(l, 2_l);
      CHECK(r->intval() == 3);
    }
    {
      auto r = add1(2_l);
      CHECK(r->intval() == 3);
    }
  }

  SECTION("sub1")
  {
    {
      auto r = sub1(l, 3_l);
      CHECK(r->intval() == 2);
    }
    {
      auto r = sub1(3_l);
      CHECK(r->intval() == 2);
    }
  }

  SECTION("abs")
  {
    {
      auto r = absval(l, mknumber(-1));
      CHECK(r->intval() == 1);
    }
    {
      auto r = absval(mknumber(-1));
      CHECK(r->intval() == 1);
    }
  }

  SECTION("f+")
  {
    auto r = eval(l, "(f+ (itof 5) (itof 2))");
    REQUIRE(type_of(r) == type::FLOAT);
    CHECK(r->floatval() == 7.0);
  }

  SECTION("f-")
  {
    {
      auto r0 = fdifference(l, 1.0_l, 2.0_l);
      REQUIRE(type_of(r0) == type::FLOAT);
      CHECK(r0->floatval() == -1);
    }
    {
      auto r0 = fdifference(1.0_l, 2.0_l);
      REQUIRE(type_of(r0) == type::FLOAT);
      CHECK(r0->floatval() == -1);
    }
  }

  SECTION("f*")
  {
    auto r = eval(l, "(f* (itof 5) (itof 2))");
    CHECK(type_of(r) == type::FLOAT);
    CHECK(r->floatval() == 10.0);
  }

  SECTION("f/")
  {
    auto r = eval(l, "(f/ (itof 5) (itof 2))");
    CHECK(type_of(r) == type::FLOAT);
    CHECK(r->floatval() == 2.5);
  }

  SECTION("itof")
  {
    auto r = eval(l, "(itof 8)");
    CHECK(type_of(r) == type::FLOAT);
    CHECK(r->floatval() == 8.0);
  }

  SECTION("greaterp")
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

  SECTION("lessp")
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

  SECTION("eqp")
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

  SECTION("geq")
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

  SECTION("leq")
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

  SECTION("neqp")
  {
  }

  SECTION("zerop")
  {
    {
      auto r = zerop(l, 0_l);
      CHECK(is_T(r));
    }
    {
      auto r = zerop(0_l);
      CHECK(is_T(r));
    }
    {
      auto r = zerop(l, 1_l);
      CHECK(is_NIL(r));
    }
    {
      auto r = zerop(1_l);
      CHECK(is_NIL(r));
    }
  }

  SECTION("minusp")
  {
    {
      auto r = minusp(l, mknumber(-5));
      CHECK(is_T(r));
    }
    {
      auto r = minusp(mknumber(-5));
      CHECK(is_T(r));
    }
    {
      auto r = minusp(l, 5_l);
      CHECK(is_NIL(r));
    }
    {
      auto r = minusp(5_l);
      CHECK(is_NIL(r));
    }
  }

}

}
