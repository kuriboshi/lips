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
#include <string>

#include <catch2/catch_test_macros.hpp>

#include "alloc.hh"
#include "low.hh"

namespace lisp
{

TEST_CASE("low: low level functions")
{
  SECTION("low: set")
  {
    std::string set0 = R"(
(progn
 (set 'a 'b)
 (set a 123)
 b)
)";
    auto e0 = eval(set0);
    CHECK(e0->intval() == 123);
  }

  SECTION("low: prohibit set on constants")
  {
    auto& ctx = context::current();
    std::ostringstream os;
    auto old = ctx.primerr(ref_file_t::create(os));
    CHECK_THROWS(set(T, nil));
    CHECK_THROWS(set(mkatom("nil"), nil));
    CHECK_THROWS(set(T, nil));
    CHECK_THROWS(set(mkatom("nil"), nil));
    ctx.primerr(old);
  }

  SECTION("low: setq")
  {
    auto a = mkatom("a");
    setq(a, mknumber(100));
    CHECK(a->value()->intval() == 100);
    setq(a, mknumber(200));
    CHECK(a->value()->intval() == 200);
  }

  SECTION("low: setqq")
  {
    // In C++ there is no real difference between 'set' and 'setqq'.
    auto r = setqq("a"_a, "b"_a);
    REQUIRE(type_of(r) == type::Symbol);
    CHECK(r->symbol()->pname == "b");
    // In lisp, however, it matters so test it by evaluating a lisp expression.
    r = "(progn (setqq a b) a)"_e;
    REQUIRE(type_of(r) == type::Symbol);
    CHECK(r->symbol()->pname == "b");
  }

  SECTION("low: while")
  {
    set("a"_l, 0_l);
    set("i"_l, 3_l);
    xwhile(mklist("greaterp"_a, "i"_a, 0_l),
      mklist(mklist("setq"_a, "a"_a, mklist("plus"_a, "a"_a, "i"_a)),
        mklist("setq"_a, "i"_a, mklist("difference"_a, "i"_a, 1_l))));
    CHECK("a"_a->value()->intval() == 6);
    set("i"_a, 3_l);
    xwhile(mklist("greaterp"_a, "i"_a, 0_l),
      mklist(mklist("setq"_a, "a"_a, mklist("plus"_a, "a"_a, "i"_a)),
        mklist("setq"_a, "i"_a, mklist("difference"_a, "i"_a, 1_l))));
  }

  SECTION("low: cond")
  {
    std::string cond0 = R"(
(progn
 (setq a 1)
 (cond ((eq a 1)
        "a")))
)";
    auto a0 = eval(cond0);
    CHECK(a0->getstr() == "a");

    std::string cond1 = R"(
(cond (nil nil))
)";
    auto a1 = eval(cond1);
    CHECK(is_nil(a1));

    auto a2 = cond(nil);
    CHECK(is_nil(a2));

    auto a3 = cond(cons(mklist(T, mkstring("A")), nil));
    CHECK(a3->getstr() == "A");

    auto a4 = cond(cons(mklist(T, mkstring("A")), nil));
    CHECK(a4->getstr() == "A");

    std::string cond5 = R"(
(cond (t))
)";
    auto a5 = eval(cond5);
    CHECK(is_T(a5));
  }

  SECTION("low: prog1")
  {
    auto r0 = prog1(1_l, mklist(2_l, 3_l, 4_l));
    CHECK(r0->intval() == 1);
    auto r1 = prog1(1_l, mklist(2_l, 3_l, 4_l));
    CHECK(r1->intval() == 1);
  }

  SECTION("low: progn")
  {
    auto a0 = progn(mklist(1_l, 2_l, 3_l));
    CHECK(a0->intval() == 3);
    auto a1 = progn(mklist(1_l, 2_l, 3_l));
    CHECK(a1->intval() == 3);
    auto a2 = progn(nil);
    CHECK(a2 == nil);
  }

  SECTION("low: while")
  {
    std::string p0 = R"(
(progn
  (setq r 0)
  ((lambda (i)
    (while (leq i 9)
           (setq i (plus i 1)))
    (setq r i))
   0)
  r)
)";
    auto r0 = eval(p0);
    CHECK(r0->intval() == 10);
  }
}

} // namespace lisp
