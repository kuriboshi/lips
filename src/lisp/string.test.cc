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

#include <catch2/catch.hpp>

#include "string.hh"

namespace lisp
{

TEST_CASE("String functions")
{
  SECTION("string: stringp")
  {
    auto s = mkstring("hello");
    auto r0 = stringp(s);
    CHECK(r0 != NIL);
    CHECK(r0->string() == s->string());
    auto i = mknumber(100);
    auto r1 = stringp(i);
    CHECK(r1 == NIL);
  }

  SECTION("string: strequal")
  {
    auto s0 = mkstring("lorem");
    auto s1 = mkstring("lorem");
    auto s2 = mkstring("ipsem");
    auto r0 = strequal(s0, s1);
    CHECK(r0 == T);
    auto r1 = strequal(s0, s2);
    CHECK(r1 == NIL);
  }

  SECTION("string: concat")
  {
    auto s0 = mkstring("hello ");
    auto s1 = mkstring("world");
    auto s2 = concat(cons(s0, cons(s1, NIL)));
    CHECK(s2->string() == mkstring("hello world")->string());
    auto s3 = concat(cons(s0, cons(s1, NIL)));
    CHECK(s3->string() == mkstring("hello world")->string());
  }

  SECTION("string: strlen")
  {
    auto s0 = mkstring("lorem");
    auto l0 = strlen(s0);
    CHECK(l0->intval() == 5);
    auto l1 = strlen(s0);
    CHECK(l1->intval() == 5);
  }

  SECTION("string: substring")
  {
    auto s0 = mkstring("hello world");
    auto s1 = substring(s0, mknumber(1), mknumber(5));
    REQUIRE(s1 != NIL);
    CHECK(s1->string() == "hello");
    auto s2 = substring(s0, mknumber(7), mknumber(11));
    REQUIRE(s2 != NIL);
    CHECK(s2->string() == "world");
    auto s3 = substring(s0, mknumber(-1), mknumber(5));
    REQUIRE(s3 != NIL);
    CHECK(s3->string() == "d");
    auto s4 = substring(s0, mknumber(0), mknumber(15));
    CHECK(s4 == NIL);
    auto s5 = substring(s0, mknumber(0), mknumber(-1));
    CHECK(s5 == NIL);
  }

  SECTION("string: symstr")
  {
    auto p0 = intern("symbol");
    auto r0 = symstr(p0);
    CHECK(type_of(r0) == type::String);
    CHECK(r0->string() == p0->getstr());
    auto r1 = symstr(p0);
    CHECK(type_of(r1) == type::String);
    CHECK(r1->string() == p0->getstr());
  }

  SECTION("string: strcmp")
  {
    auto s0 = mkstring("alpha");
    auto s1 = mkstring("zeta");
    auto r0 = strcmp(s0, s1);
    REQUIRE(type_of(r0) == type::Integer);
    CHECK(r0->intval() < 0);
    auto r1 = strcmp(s1, s0);
    REQUIRE(type_of(r1) == type::Integer);
    CHECK(r1->intval() > 0);
    auto r2 = strcmp(s0, s0);
    REQUIRE(type_of(r2) == type::Integer);
    CHECK(r2->intval() == 0);
  }
}

} // namespace lisp
