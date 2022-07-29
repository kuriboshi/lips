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

#include "map.hh"

namespace lisp
{

TEST_CASE("Map functions")
{
  SECTION("map")
  {
    auto& cvar = initcvar("cvar", NIL);
    auto r0 = eval(R"(
(map '(1 2 3)
      (lambda (a)
       (setq cvar (cons (car a) cvar)))))");
    CHECK(type_of(cvar) == type::Cons);
    CHECK(car(cvar)->intval() == 3);
    CHECK(cadr(cvar)->intval() == 2);
    CHECK(caddr(cvar)->intval() == 1);

    cvar = 0_l;
    auto f = eval("(lambda (a) (setq cvar (plus (apply plus a) cvar)))");
    map(mklist(1_l, 1_l, 1_l), f, NIL);
    CHECK(cvar->intval() == 6);

    cvar = NIL;
    map("(1 2 3)"_l, lambda("(a)"_l, "((setq cvar (cons (car a) cvar)))"_l), lambda("(a)"_l, "((cdr a))"_l));
    CHECK(type_of(cvar) == type::Cons);
    CHECK(car(cvar)->intval() == 3);
    CHECK(cadr(cvar)->intval() == 2);
    CHECK(caddr(cvar)->intval() == 1);
  }

  SECTION("mapc")
  {
    auto& cvar = initcvar("cvar", NIL);
    auto r0 = eval(R"(
(mapc '(1 2 3)
       (lambda (a)
        (setq cvar (cons a cvar)))))");
    REQUIRE(type_of(cvar) == type::Cons);
    CHECK(car(cvar)->intval() == 3);
    CHECK(cadr(cvar)->intval() == 2);
    CHECK(caddr(cvar)->intval() == 1);

    cvar = 0_l;
    auto f = lambda("(a)"_l, "((setq cvar (plus a cvar)))"_l);
    mapc(mklist(1_l, 1_l, 1_l), f, NIL);
    REQUIRE(type_of(cvar) == type::Integer);
    CHECK(cvar->intval() == 3);

    cvar = 0_l;
    mapc(mklist(1_l, 2_l, 3_l), f, lambda("(a)"_l, "((cdr a))"_l));
    REQUIRE(type_of(cvar) == type::Integer);
    CHECK(cvar->intval() == 6);
  }

  SECTION("maplist")
  {
    auto ls = mklist(mknumber(1), mknumber(2), mknumber(3));
    auto f = lambda("(a)"_l, "((car a))"_l);

    auto r0 = maplist(ls, f, NIL);
    REQUIRE(type_of(r0) == type::Cons);
    CHECK(car(r0)->intval() == 1);
    CHECK(cadr(r0)->intval() == 2);
    CHECK(caddr(r0)->intval() == 3);

    auto r1 = maplist(ls, f, NIL);
    REQUIRE(type_of(r1) == type::Cons);
    CHECK(car(r1)->intval() == 1);
    CHECK(cadr(r1)->intval() == 2);
    CHECK(caddr(r1)->intval() == 3);

    auto r2 = maplist(ls, f, lambda("(a)"_l, "((cdr a))"_l));
    REQUIRE(type_of(r2) == type::Cons);
    CHECK(car(r1)->intval() == 1);
    CHECK(cadr(r1)->intval() == 2);
    CHECK(caddr(r1)->intval() == 3);
  }

  SECTION("mapcar")
  {
    auto ls = mklist(mknumber(1), mknumber(2), mknumber(3));
    auto f = lambda("(a)"_l, "((plus a 1))"_l);

    auto r0 = mapcar(ls, f, NIL);
    REQUIRE(type_of(r0) == type::Cons);
    CHECK(car(r0)->intval() == 2);
    CHECK(cadr(r0)->intval() == 3);
    CHECK(caddr(r0)->intval() == 4);

    auto r1 = mapcar(ls, f, NIL);
    REQUIRE(type_of(r1) == type::Cons);
    CHECK(car(r1)->intval() == 2);
    CHECK(cadr(r1)->intval() == 3);
    CHECK(caddr(r1)->intval() == 4);

    auto r2 = mapcar(ls, f, lambda("(a)"_l, "((cdr a))"_l));
    REQUIRE(type_of(r0) == type::Cons);
    CHECK(car(r2)->intval() == 2);
    CHECK(cadr(r2)->intval() == 3);
    CHECK(caddr(r2)->intval() == 4);
  }
}

} // namespace lisp
