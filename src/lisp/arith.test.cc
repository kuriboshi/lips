//
// Lips, lisp shell.
// Copyright 2021-2022 Krister Joas
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

#include <sstream>

#include <catch2/catch.hpp>

#include "arith.hh"

namespace lisp
{

TEST_CASE("Arithmetic functions")
{
  std::ostringstream os;
  auto& ctx = context::current();
  auto old = ctx.primerr(ref_file_t::create(os));

  SECTION("+")
  {
    {
      auto r0 = plus(mklist(1_l, 2_l, 3_l, 4_l, 5_l));
      REQUIRE(type_of(r0) == type::Integer);
      CHECK(r0->intval() == 15);
    }
    {
      auto r0 = plus(mklist(1_l, 1.0_l, 2.0_l));
      REQUIRE(type_of(r0) == type::Float);
      CHECK(r0->floatval() == 4.0);
    }
    {
      auto r0 = plus(mklist(1_l, 1_l));
      REQUIRE(type_of(r0) == type::Integer);
      CHECK(r0->intval() == 2);
    }
    {
      auto r0 = plus(mklist(1.0_l, 1_l));
      REQUIRE(type_of(r0) == type::Float);
      CHECK(r0->floatval() == 2);
    }
    {
      auto r0 = plus(mklist(1.0_l, 1_l));
      REQUIRE(type_of(r0) == type::Float);
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
      auto r0 = difference(1_l, 2_l);
      REQUIRE(type_of(r0) == type::Integer);
      CHECK(r0->intval() == -1);
    }
    {
      auto r0 = difference(1_l, 2_l);
      REQUIRE(type_of(r0) == type::Integer);
      CHECK(r0->intval() == -1);
    }
    {
      auto r0 = difference(1_l, 2.0_l);
      REQUIRE(type_of(r0) == type::Float);
      CHECK(r0->floatval() == -1.0);
    }
    {
      auto r0 = difference(1.0_l, 2_l);
      REQUIRE(type_of(r0) == type::Float);
      CHECK(r0->floatval() == -1.0);
    }
    {
      auto r0 = difference(1.0_l, 2.0_l);
      REQUIRE(type_of(r0) == type::Float);
      CHECK(r0->floatval() == -1.0);
    }
  }

  SECTION("*")
  {
    {
      auto r0 = ltimes(mklist(5_l, 7_l));
      CHECK(r0->intval() == 35);
      r0 = ltimes(mklist(5_l, 7_l));
      CHECK(r0->intval() == 35);
    }
    {
      auto r0 = ltimes(mklist(5.0_l, 7_l));
      REQUIRE(type_of(r0) == type::Float);
      CHECK(r0->floatval() == 35.0);
    }
    {
      auto r0 = ltimes(mklist(5_l, 7.0_l));
      REQUIRE(type_of(r0) == type::Float);
      CHECK(r0->floatval() == 35.0);
    }
    {
      auto r0 = ltimes(mklist(5.0_l, 7.0_l));
      REQUIRE(type_of(r0) == type::Float);
      CHECK(r0->floatval() == 35.0);
    }
    {
      CHECK_THROWS_WITH(ltimes(mklist(5_l, "s"_s)), "Illegal argument");
      CHECK_THROWS_WITH(ltimes(mklist(5.0_l, "s"_s)), "Illegal argument");
    }
  }

  SECTION("/")
  {
    {
      auto r0 = divide(4_l, 2_l);
      REQUIRE(type_of(r0) == type::Integer);
      CHECK(r0->intval() == 2);
    }
    {
      auto r0 = divide(4_l, 2_l);
      REQUIRE(type_of(r0) == type::Integer);
      CHECK(r0->intval() == 2);
    }
    {
      auto r0 = divide(4_l, 2.0_l);
      REQUIRE(type_of(r0) == type::Float);
      CHECK(r0->floatval() == 2.0);
    }
    {
      auto r0 = divide(4_l, 2.0_l);
      REQUIRE(type_of(r0) == type::Float);
      CHECK(r0->floatval() == 2.0);
    }
    {
      auto r0 = divide(4.0_l, 2_l);
      REQUIRE(type_of(r0) == type::Float);
      CHECK(r0->floatval() == 2.0);
    }
    {
      auto r0 = divide(4.0_l, 2.0_l);
      REQUIRE(type_of(r0) == type::Float);
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
      auto r0 = iplus(mklist(1_l, 2_l, 7_l));
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
      auto r0 = idifference(1_l, 2_l);
      REQUIRE(type_of(r0) == type::Integer);
      CHECK(r0->intval() == -1);
    }
    {
      auto r0 = idifference(1_l, 2_l);
      REQUIRE(type_of(r0) == type::Integer);
      CHECK(r0->intval() == -1);
    }
  }

  SECTION("i*")
  {
    {
      auto r0 = itimes(mklist(6_l, 8_l));
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
      auto r0 = iquotient(5_l, 2_l);
      REQUIRE(type_of(r0) == type::Integer);
      CHECK(r0->intval() == 2);
    }
    {
      auto r0 = iquotient(5_l, 2_l);
      REQUIRE(type_of(r0) == type::Integer);
      CHECK(r0->intval() == 2);
    }
    {
      CHECK_THROWS_WITH(iquotient(1_l, 0_l), "Divide by zero");
    }
  }

  SECTION("i%")
  {
    {
      auto r0 = iremainder(5_l, 2_l);
      REQUIRE(type_of(r0) == type::Integer);
      CHECK(r0->intval() == 1);
    }
    {
      auto r0 = iremainder(5_l, 2_l);
      REQUIRE(type_of(r0) == type::Integer);
      CHECK(r0->intval() == 1);
    }
    {
      CHECK_THROWS_WITH(iremainder(5_l, 0_l), "Divide by zero");
    }
  }

  SECTION("minus")
  {
    {
      auto r0 = minus(1_l);
      REQUIRE(type_of(r0) == type::Integer);
      CHECK(r0->intval() == -1);
    }
    {
      auto r0 = minus(1.0_l);
      REQUIRE(type_of(r0) == type::Float);
      CHECK(r0->floatval() == -1.0);
    }
    {
      auto r0 = minus(1.0_l);
      REQUIRE(type_of(r0) == type::Float);
      CHECK(r0->floatval() == -1.0);
    }
  }

  SECTION("iminus")
  {
    {
      auto r0 = iminus(1_l);
      REQUIRE(type_of(r0) == type::Integer);
      CHECK(r0->intval() == -1);
    }
    {
      auto r0 = iminus(1_l);
      REQUIRE(type_of(r0) == type::Integer);
      CHECK(r0->intval() == -1);
    }
  }

  SECTION("add1")
  {
    {
      auto r = add1(2_l);
      REQUIRE(type_of(r) == type::Integer);
      CHECK(r->intval() == 3);
    }
    {
      auto r = add1(2_l);
      REQUIRE(type_of(r) == type::Integer);
      CHECK(r->intval() == 3);
    }
  }

  SECTION("sub1")
  {
    {
      auto r = sub1(3_l);
      REQUIRE(type_of(r) == type::Integer);
      CHECK(r->intval() == 2);
    }
    {
      auto r = sub1(3_l);
      REQUIRE(type_of(r) == type::Integer);
      CHECK(r->intval() == 2);
    }
  }

  SECTION("abs")
  {
    {
      auto r = abs(mknumber(-1));
      REQUIRE(type_of(r) == type::Integer);
      CHECK(r->intval() == 1);
    }
    {
      auto r = abs(mknumber(-1));
      REQUIRE(type_of(r) == type::Integer);
      CHECK(r->intval() == 1);
    }
    {
      auto r = abs(mknumber(1));
      REQUIRE(type_of(r) == type::Integer);
      CHECK(r->intval() == 1);
    }
  }

  SECTION("f+")
  {
    auto r = fplus(mklist(2.0_l, 5.0_l));
    REQUIRE(type_of(r) == type::Float);
    CHECK(r->floatval() == 7.0);
    r = fplus(mklist(2.0_l, 5.0_l));
    REQUIRE(type_of(r) == type::Float);
    CHECK(r->floatval() == 7.0);
  }

  SECTION("f-")
  {
    {
      auto r0 = fdifference(1.0_l, 2.0_l);
      REQUIRE(type_of(r0) == type::Float);
      CHECK(r0->floatval() == -1);
    }
    {
      auto r0 = fdifference(1.0_l, 2.0_l);
      REQUIRE(type_of(r0) == type::Float);
      CHECK(r0->floatval() == -1);
    }
  }

  SECTION("f*")
  {
    auto r = ftimes(mklist(5.0_l, 2.0_l));
    CHECK(type_of(r) == type::Float);
    CHECK(r->floatval() == 10.0);
    r = ftimes(mklist(5.0_l, 2.0_l));
    CHECK(type_of(r) == type::Float);
    CHECK(r->floatval() == 10.0);
  }

  SECTION("f/")
  {
    auto r = fdivide(5.0_l, 2.0_l);
    CHECK(type_of(r) == type::Float);
    CHECK(r->floatval() == 2.5);
    r = fdivide(5.0_l, 2.0_l);
    CHECK(type_of(r) == type::Float);
    CHECK(r->floatval() == 2.5);
    CHECK_THROWS_WITH(fdivide(1.0_l, 0.0_l), "Divide by zero");
  }

  SECTION("itof")
  {
    auto r = itof(8_l);
    CHECK(type_of(r) == type::Float);
    CHECK(r->floatval() == 8.0);
    r = itof(9_l);
    CHECK(type_of(r) == type::Float);
    CHECK(r->floatval() == 9.0);
  }

  SECTION("greaterp")
  {
    CHECK(is_T(greaterp(mknumber(2), mknumber(1))));
    CHECK(is_NIL(greaterp(mknumber(1), mknumber(2))));
    CHECK(is_T(greaterp(mkfloat(2.0), mknumber(1))));
    CHECK(is_NIL(greaterp(mkfloat(1.0), mknumber(2))));
    CHECK(is_T(greaterp(mknumber(2), mkfloat(1.0))));
    CHECK(is_NIL(greaterp(mknumber(1), mkfloat(2.0))));
    CHECK(is_T(greaterp(mkfloat(2.0), mkfloat(1.0))));
    CHECK(is_NIL(greaterp(mkfloat(1.0), mkfloat(2.0))));
    CHECK(is_NIL(greaterp(mkfloat(1.0), mkfloat(2.0))));
    CHECK_THROWS_WITH(greaterp("a"_s, 1.0_l), "Illegal argument");
    CHECK_THROWS_WITH(greaterp(1.0_l, "b"_s), "Illegal argument");
    CHECK_THROWS_WITH(greaterp("a"_s, 1.0_l), "Illegal argument");
    CHECK_THROWS_WITH(greaterp(1.0_l, "b"_s), "Illegal argument");
  }

  SECTION("lessp")
  {
    CHECK(is_NIL(lessp(mknumber(2), mknumber(1))));
    CHECK(is_T(lessp(mknumber(1), mknumber(2))));
    CHECK(is_NIL(lessp(mkfloat(2.0), mknumber(1))));
    CHECK(is_T(lessp(mkfloat(1.0), mknumber(2))));
    CHECK(is_NIL(lessp(mknumber(2), mkfloat(1.0))));
    CHECK(is_T(lessp(mknumber(1), mkfloat(2.0))));
    CHECK(is_NIL(lessp(mkfloat(2.0), mkfloat(1.0))));
    CHECK(is_T(lessp(mkfloat(1.0), mkfloat(2.0))));
    CHECK(is_T(lessp(mkfloat(1.0), mkfloat(2.0))));
    CHECK_THROWS_WITH(lessp("a"_s, 1_l), "Illegal argument");
    CHECK_THROWS_WITH(lessp(1_l, "b"_s), "Illegal argument");
  }

  SECTION("eqp")
  {
    CHECK(is_NIL(eqp(mknumber(2), mknumber(1))));
    CHECK(is_NIL(eqp(mknumber(1), mknumber(2))));
    CHECK(is_NIL(eqp(mkfloat(2.0), mknumber(1))));
    CHECK(is_NIL(eqp(mkfloat(1.0), mknumber(2))));
    CHECK(is_NIL(eqp(mknumber(2), mkfloat(1.0))));
    CHECK(is_NIL(eqp(mknumber(1), mkfloat(2.0))));
    CHECK(is_NIL(eqp(mkfloat(2.0), mkfloat(1.0))));
    CHECK(is_NIL(eqp(mkfloat(1.0), mkfloat(2.0))));
    CHECK(is_T(eqp(mknumber(1), mknumber(1))));
    CHECK(is_T(eqp(mknumber(1), mknumber(1))));
    CHECK_THROWS_WITH(eqp("a"_s, 1_l), "Illegal argument");
    CHECK_THROWS_WITH(eqp(1_l, "b"_s), "Illegal argument");
  }

  SECTION("geq")
  {
    CHECK(is_T(geq(mknumber(2), mknumber(1))));
    CHECK(is_NIL(geq(mknumber(1), mknumber(2))));
    CHECK(is_T(geq(mkfloat(2.0), mknumber(1))));
    CHECK(is_NIL(geq(mkfloat(1.0), mknumber(2))));
    CHECK(is_T(geq(mknumber(2), mkfloat(1.0))));
    CHECK(is_NIL(geq(mknumber(1), mkfloat(2.0))));
    CHECK(is_T(geq(mkfloat(2.0), mkfloat(1.0))));
    CHECK(is_NIL(geq(mkfloat(1.0), mkfloat(2.0))));
    CHECK(is_T(geq(mknumber(1), mknumber(1))));
    CHECK(is_T(geq(mknumber(1), mknumber(1))));
    CHECK_THROWS_WITH(geq("a"_s, 1_l), "Illegal argument");
    CHECK_THROWS_WITH(geq(1_l, "b"_s), "Illegal argument");
  }

  SECTION("leq")
  {
    CHECK(is_NIL(leq(mknumber(2), mknumber(1))));
    CHECK(is_T(leq(mknumber(1), mknumber(2))));
    CHECK(is_NIL(leq(mkfloat(2.0), mknumber(1))));
    CHECK(is_T(leq(mkfloat(1.0), mknumber(2))));
    CHECK(is_NIL(leq(mknumber(2), mkfloat(1.0))));
    CHECK(is_T(leq(mknumber(1), mkfloat(2.0))));
    CHECK(is_NIL(leq(mkfloat(2.0), mkfloat(1.0))));
    CHECK(is_T(leq(mkfloat(1.0), mkfloat(2.0))));
    CHECK(is_T(leq(mknumber(1), mknumber(1))));
    CHECK(is_T(leq(mknumber(1), mknumber(1))));
    CHECK_THROWS_WITH(leq("a"_s, 1_l), "Illegal argument");
    CHECK_THROWS_WITH(leq(1_l, "b"_s), "Illegal argument");
  }

  SECTION("neqp")
  {
    CHECK(is_T(neqp(mknumber(2), mknumber(1))));
    CHECK(is_T(neqp(mknumber(1), mknumber(2))));
    CHECK(is_T(neqp(mkfloat(2.0), mknumber(1))));
    CHECK(is_T(neqp(mkfloat(1.0), mknumber(2))));
    CHECK(is_T(neqp(mknumber(2), mkfloat(1.0))));
    CHECK(is_T(neqp(mknumber(1), mkfloat(2.0))));
    CHECK(is_T(neqp(mkfloat(2.0), mkfloat(1.0))));
    CHECK(is_T(neqp(mkfloat(1.0), mkfloat(2.0))));
    CHECK(is_NIL(neqp(mknumber(1), mknumber(1))));
    CHECK(is_NIL(neqp(mknumber(1), mknumber(1))));
    CHECK_THROWS(neqp("a"_s, 1_l), "Illegal argument");
    CHECK_THROWS(neqp(1_l, "b"_s), "Illegal argument");
  }

  SECTION("zerop")
  {
    CHECK(is_T(zerop(0_l)));
    CHECK(is_NIL(zerop(1_l)));
  }

  SECTION("minusp")
  {
    CHECK(is_T(minusp(mknumber(-5))));
    CHECK(is_NIL(minusp(5_l)));
    CHECK(is_T(minusp(mkfloat(-1.0))));
    CHECK(is_NIL(minusp(1.0_l)));
    CHECK_THROWS_WITH(minusp("string"_s), "Illegal argument");
  }

  ctx.primerr(old);
}

} // namespace lisp
