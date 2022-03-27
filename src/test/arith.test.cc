//
// Lips, lisp shell.
// Copyright 2021-2022 Krister Joas
//
#include <catch2/catch.hpp>
#include <lisp/libisp.hh>

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
    {
      CHECK_THROWS_WITH(plus(mklist(1_l, "s"_s)), "Illegal argument");
      CHECK_THROWS_WITH(plus(mklist(1.0_l, "s"_s)), "Illegal argument");
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
    {
      auto r0 = difference(1_l, 2.0_l);
      REQUIRE(type_of(r0) == type::FLOAT);
      CHECK(r0->floatval() == -1.0);
    }
    {
      auto r0 = difference(1.0_l, 2_l);
      REQUIRE(type_of(r0) == type::FLOAT);
      CHECK(r0->floatval() == -1.0);
    }
    {
      auto r0 = difference(1.0_l, 2.0_l);
      REQUIRE(type_of(r0) == type::FLOAT);
      CHECK(r0->floatval() == -1.0);
    }
  }

  SECTION("*")
  {
    {
      auto r0 = ltimes(l, mklist(5_l, 7_l));
      CHECK(r0->intval() == 35);
      r0 = ltimes(mklist(5_l, 7_l));
      CHECK(r0->intval() == 35);
    }
    {
      auto r0 = ltimes(l, mklist(5.0_l, 7_l));
      REQUIRE(type_of(r0) == type::FLOAT);
      CHECK(r0->floatval() == 35.0);
    }
    {
      auto r0 = ltimes(l, mklist(5_l, 7.0_l));
      REQUIRE(type_of(r0) == type::FLOAT);
      CHECK(r0->floatval() == 35.0);
    }
    {
      auto r0 = ltimes(l, mklist(5.0_l, 7.0_l));
      REQUIRE(type_of(r0) == type::FLOAT);
      CHECK(r0->floatval() == 35.0);
    }
    {
      CHECK_THROWS_WITH(ltimes(l, mklist(5_l, "s"_s)), "Illegal argument");
      CHECK_THROWS_WITH(ltimes(l, mklist(5.0_l, "s"_s)), "Illegal argument");
    }
  }

  SECTION("/")
  {
    {
      auto r0 = divide(l, 4_l, 2_l);
      REQUIRE(type_of(r0) == type::INTEGER);
      CHECK(r0->intval() == 2);
    }
    {
      auto r0 = divide(4_l, 2_l);
      REQUIRE(type_of(r0) == type::INTEGER);
      CHECK(r0->intval() == 2);
    }
    {
      auto r0 = divide(l, 4_l, 2.0_l);
      REQUIRE(type_of(r0) == type::FLOAT);
      CHECK(r0->floatval() == 2.0);
    }
    {
      auto r0 = divide(4_l, 2.0_l);
      REQUIRE(type_of(r0) == type::FLOAT);
      CHECK(r0->floatval() == 2.0);
    }
    {
      auto r0 = divide(4.0_l, 2_l);
      REQUIRE(type_of(r0) == type::FLOAT);
      CHECK(r0->floatval() == 2.0);
    }
    {
      auto r0 = divide(4.0_l, 2.0_l);
      REQUIRE(type_of(r0) == type::FLOAT);
      CHECK(r0->floatval() == 2.0);
    }
    {
      CHECK_THROWS_WITH(divide(1_l, 0_l), "Divide by zero");
      CHECK_THROWS_WITH(divide(1_l, 0.0_l), "Divide by zero");
      CHECK_THROWS_WITH(divide(1.0_l, 0_l), "Divide by zero");
      CHECK_THROWS_WITH(divide(1.0_l, 0.0_l), "Divide by zero");
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
      REQUIRE(type_of(r0) == type::INTEGER);
      CHECK(r0->intval() == 2);
    }
    {
      auto r0 = iquotient(5_l, 2_l);
      REQUIRE(type_of(r0) == type::INTEGER);
      CHECK(r0->intval() == 2);
    }
    {
      CHECK_THROWS_WITH(iquotient(1_l, 0_l), "Divide by zero");
    }
  }

  SECTION("i%")
  {
    {
      auto r0 = iremainder(l, 5_l, 2_l);
      REQUIRE(type_of(r0) == type::INTEGER);
      CHECK(r0->intval() == 1);
    }
    {
      auto r0 = iremainder(5_l, 2_l);
      REQUIRE(type_of(r0) == type::INTEGER);
      CHECK(r0->intval() == 1);
    }
    {
      CHECK_THROWS_WITH(iremainder(5_l, 0_l), "Divide by zero");
    }
  }

  SECTION("minus")
  {
    {
      auto r0 = minus(l, 1_l);
      REQUIRE(type_of(r0) == type::INTEGER);
      CHECK(r0->intval() == -1);
    }
    {
      auto r0 = minus(l, 1.0_l);
      REQUIRE(type_of(r0) == type::FLOAT);
      CHECK(r0->floatval() == -1.0);
    }
    {
      auto r0 = minus(1.0_l);
      REQUIRE(type_of(r0) == type::FLOAT);
      CHECK(r0->floatval() == -1.0);
    }
  }

  SECTION("iminus")
  {
    {
      auto r0 = iminus(l, 1_l);
      REQUIRE(type_of(r0) == type::INTEGER);
      CHECK(r0->intval() == -1);
    }
    {
      auto r0 = iminus(1_l);
      REQUIRE(type_of(r0) == type::INTEGER);
      CHECK(r0->intval() == -1);
    }
  }

  SECTION("add1")
  {
    {
      auto r = add1(l, 2_l);
      REQUIRE(type_of(r) == type::INTEGER);
      CHECK(r->intval() == 3);
    }
    {
      auto r = add1(2_l);
      REQUIRE(type_of(r) == type::INTEGER);
      CHECK(r->intval() == 3);
    }
  }

  SECTION("sub1")
  {
    {
      auto r = sub1(l, 3_l);
      REQUIRE(type_of(r) == type::INTEGER);
      CHECK(r->intval() == 2);
    }
    {
      auto r = sub1(3_l);
      REQUIRE(type_of(r) == type::INTEGER);
      CHECK(r->intval() == 2);
    }
  }

  SECTION("abs")
  {
    {
      auto r = absval(l, mknumber(-1));
      REQUIRE(type_of(r) == type::INTEGER);
      CHECK(r->intval() == 1);
    }
    {
      auto r = absval(mknumber(-1));
      REQUIRE(type_of(r) == type::INTEGER);
      CHECK(r->intval() == 1);
    }
    {
      auto r = absval(mknumber(1));
      REQUIRE(type_of(r) == type::INTEGER);
      CHECK(r->intval() == 1);
    }
  }

  SECTION("f+")
  {
    auto r = fplus(l, mklist(2.0_l, 5.0_l));
    REQUIRE(type_of(r) == type::FLOAT);
    CHECK(r->floatval() == 7.0);
    r = fplus(mklist(2.0_l, 5.0_l));
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
    auto r = ftimes(l, mklist(5.0_l, 2.0_l));
    CHECK(type_of(r) == type::FLOAT);
    CHECK(r->floatval() == 10.0);
    r = ftimes(mklist(5.0_l, 2.0_l));
    CHECK(type_of(r) == type::FLOAT);
    CHECK(r->floatval() == 10.0);
  }

  SECTION("f/")
  {
    auto r = fdivide(l, 5.0_l, 2.0_l);
    CHECK(type_of(r) == type::FLOAT);
    CHECK(r->floatval() == 2.5);
    r = fdivide(5.0_l, 2.0_l);
    CHECK(type_of(r) == type::FLOAT);
    CHECK(r->floatval() == 2.5);
    CHECK_THROWS_WITH(fdivide(1.0_l, 0.0_l), "Divide by zero");
  }

  SECTION("itof")
  {
    auto r = itof(l, 8_l);
    CHECK(type_of(r) == type::FLOAT);
    CHECK(r->floatval() == 8.0);
    r = itof(9_l);
    CHECK(type_of(r) == type::FLOAT);
    CHECK(r->floatval() == 9.0);
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
    CHECK_THROWS_WITH(greaterp(l, "a"_s, 1.0_l), "Illegal argument");
    CHECK_THROWS_WITH(greaterp(l, 1.0_l, "b"_s), "Illegal argument");
    CHECK_THROWS_WITH(greaterp(l, "a"_s, 1.0_l), "Illegal argument");
    CHECK_THROWS_WITH(greaterp(l, 1.0_l, "b"_s), "Illegal argument");
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
    CHECK_THROWS_WITH(lessp(l, "a"_s, 1_l), "Illegal argument");
    CHECK_THROWS_WITH(lessp(l, 1_l, "b"_s), "Illegal argument");
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
    CHECK_THROWS_WITH(eqp(l, "a"_s, 1_l), "Illegal argument");
    CHECK_THROWS_WITH(eqp(l, 1_l, "b"_s), "Illegal argument");
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
    CHECK_THROWS_WITH(geq(l, "a"_s, 1_l), "Illegal argument");
    CHECK_THROWS_WITH(geq(l, 1_l, "b"_s), "Illegal argument");
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
    CHECK_THROWS_WITH(leq(l, "a"_s, 1_l), "Illegal argument");
    CHECK_THROWS_WITH(leq(l, 1_l, "b"_s), "Illegal argument");
  }

  SECTION("neqp")
  {
    CHECK(is_T(neqp(l, mknumber(2), mknumber(1))));
    CHECK(is_T(neqp(l, mknumber(1), mknumber(2))));
    CHECK(is_T(neqp(l, mkfloat(2.0), mknumber(1))));
    CHECK(is_T(neqp(l, mkfloat(1.0), mknumber(2))));
    CHECK(is_T(neqp(l, mknumber(2), mkfloat(1.0))));
    CHECK(is_T(neqp(l, mknumber(1), mkfloat(2.0))));
    CHECK(is_T(neqp(l, mkfloat(2.0), mkfloat(1.0))));
    CHECK(is_T(neqp(l, mkfloat(1.0), mkfloat(2.0))));
    CHECK(is_NIL(neqp(l, mknumber(1), mknumber(1))));
    CHECK(is_NIL(neqp(mknumber(1), mknumber(1))));
    CHECK_THROWS(neqp(l, "a"_s, 1_l), "Illegal argument");
    CHECK_THROWS(neqp(l, 1_l, "b"_s), "Illegal argument");
  }

  SECTION("zerop")
  {
    CHECK(is_T(zerop(l, 0_l)));
    CHECK(is_T(zerop(0_l)));
    CHECK(is_NIL(zerop(l, 1_l)));
    CHECK(is_NIL(zerop(1_l)));
  }

  SECTION("minusp")
  {
    CHECK(is_T(minusp(l, mknumber(-5))));
    CHECK(is_T(minusp(mknumber(-5))));
    CHECK(is_NIL(minusp(l, 5_l)));
    CHECK(is_NIL(minusp(5_l)));
    CHECK(is_T(minusp(mkfloat(-1.0))));
    CHECK(is_NIL(minusp(1.0_l)));
    CHECK_THROWS_WITH(minusp("string"_s), "Illegal argument");
  }

}

}
