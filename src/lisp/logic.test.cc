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

#include <iostream>

#include <catch2/catch.hpp>

#include "alloc.hh"
#include "logic.hh"

template<typename Enumeration>
typename std::underlying_type<Enumeration>::type as_integer(Enumeration const value)
{
  return static_cast<typename std::underlying_type<Enumeration>::type>(value);
}

namespace lisp
{

TEST_CASE("Logic functions")
{
  SECTION("and")
  {
    CHECK(p_and(cons(T, cons(T, NIL))) == T);
    CHECK(p_and(cons(NIL, cons(T, NIL))) == NIL);
    CHECK(p_and(cons(T, cons(NIL, NIL))) == NIL);
    CHECK(p_and(cons(NIL, cons(NIL, NIL))) == NIL);
    CHECK(p_and(cons(T, cons(T, NIL))) == T);
    CHECK(p_and(cons(NIL, cons(T, NIL))) == NIL);
    CHECK(p_and(cons(T, cons(NIL, NIL))) == NIL);
    CHECK(p_and(cons(NIL, cons(NIL, NIL))) == NIL);
  }

  SECTION("or")
  {
    CHECK(p_or(cons(T, cons(T, NIL))) == T);
    CHECK(p_or(cons(NIL, cons(T, NIL))) == T);
    CHECK(p_or(cons(T, cons(NIL, NIL))) == T);
    CHECK(p_or(cons(NIL, cons(NIL, NIL))) == NIL);
    CHECK(p_or(cons(T, cons(T, NIL))) == T);
    CHECK(p_or(cons(NIL, cons(T, NIL))) == T);
    CHECK(p_or(cons(T, cons(NIL, NIL))) == T);
    CHECK(p_or(cons(NIL, cons(NIL, NIL))) == NIL);
  }

  SECTION("not")
  {
    CHECK(p_not(T) == NIL);
    CHECK(p_not(NIL) == T);
    CHECK(p_not(T) == NIL);
    CHECK(p_not(NIL) == T);
  }

  SECTION("logic: if")
  {
    auto r0 = "(if t 0 1)"_e;
    REQUIRE(type_of(r0) == type::Integer);
    CHECK(r0->intval() == 0);
    r0 = "(if nil 0 1)"_e;
    REQUIRE(type_of(r0) == type::Integer);
    CHECK(r0->intval() == 1);
  }
}

} // namespace lisp
