//
// Lips, lisp shell.
// Copyright 2021-2023 Krister Joas
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

#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers.hpp>

#include "alloc.hh"
#include "arith.hh"
#include "file.hh"
#include "list.hh"
#include "predicate.hh"
#include "types.hh"
#include "util.hh"

namespace lisp
{

TEST_CASE("list: list functions")
{
  SECTION("CAR and CDR")
  {
    auto sample = eval("(cons 1 2)");
    CHECK(car(sample)->as_integer() == 1);
    CHECK(cdr(sample)->as_integer() == 2);

    CHECK(car(sample)->as_integer() == 1);
    CHECK(cdr(sample)->as_integer() == 2);
  }

  SECTION("C..R and C...R")
  {
    {
      auto sample = eval("(quote ((1 . 2) 3 . 4))");
      CHECK(caar(sample)->as_integer() == 1);
      CHECK(cdar(sample)->as_integer() == 2);
      CHECK(cadr(sample)->as_integer() == 3);
      CHECK(cddr(sample)->as_integer() == 4);

      CHECK(caar(sample)->as_integer() == 1);
      CHECK(cdar(sample)->as_integer() == 2);
      CHECK(cadr(sample)->as_integer() == 3);
      CHECK(cddr(sample)->as_integer() == 4);
    }

    {
      auto sample = eval("(quote (((1 . 2) 3 . 4) (5 . 6) 7 . 8))");
      CHECK(caaar(sample)->as_integer() == 1);
      CHECK(cdaar(sample)->as_integer() == 2);
      CHECK(cadar(sample)->as_integer() == 3);
      CHECK(cddar(sample)->as_integer() == 4);
      CHECK(caadr(sample)->as_integer() == 5);
      CHECK(cdadr(sample)->as_integer() == 6);
      CHECK(caddr(sample)->as_integer() == 7);
      CHECK(cdddr(sample)->as_integer() == 8);

      CHECK(caaar(sample)->as_integer() == 1);
      CHECK(cdaar(sample)->as_integer() == 2);
      CHECK(cadar(sample)->as_integer() == 3);
      CHECK(cddar(sample)->as_integer() == 4);
      CHECK(caadr(sample)->as_integer() == 5);
      CHECK(cdadr(sample)->as_integer() == 6);
      CHECK(caddr(sample)->as_integer() == 7);
      CHECK(cdddr(sample)->as_integer() == 8);
    }
  }

  SECTION("C.R, C..R, C...R should return nil for non-CONS types")
  {
    auto sym = mkatom("a");
    CHECK(car(sym) == nil);
    CHECK(cdr(sym) == nil);

    CHECK(caar(sym) == nil);
    CHECK(cadr(sym) == nil);
    CHECK(cdar(sym) == nil);
    CHECK(cddr(sym) == nil);

    CHECK(caaar(sym) == nil);
    CHECK(caadr(sym) == nil);
    CHECK(cadar(sym) == nil);
    CHECK(caddr(sym) == nil);
    CHECK(cdaar(sym) == nil);
    CHECK(cdadr(sym) == nil);
    CHECK(cddar(sym) == nil);
    CHECK(cdddr(sym) == nil);
  }

  SECTION("append")
  {
    auto list0 = mklist(1_l, 2_l);
    auto list1 = mklist(3_l);
    auto list = append(cons(list0, cons(list1, nil)));
    // Original list is unchanged
    auto r0 = iplus(list0);
    CHECK(r0->as_integer() == 3);
    auto r1 = iplus(list);
    CHECK(r1->as_integer() == 6);
    auto list2 = mklist(4_l);
    list = append(cons(list, cons(list2, nil)));
    auto r2 = iplus(list);
    CHECK(r2->as_integer() == 10);
  }

  SECTION("nconc")
  {
    auto list0 = mklist(1_l, 2_l);
    auto list1 = mklist(3_l);
    auto list = nconc(cons(list0, cons(list1, nil)));
    // Original list changes
    auto r0 = iplus(list0);
    CHECK(r0->as_integer() == 6);
    auto r1 = iplus(list);
    CHECK(r1->as_integer() == 6);
    auto list2 = mklist(4_l);
    list = nconc(cons(list, cons(list2, nil)));
    // Original list changes
    auto r2 = iplus(list0);
    CHECK(r2->as_integer() == 10);
    auto r3 = iplus(list);
    CHECK(r3->as_integer() == 10);
  }

  SECTION("tconc")
  {
    // This example is from the Interlisp manual page 6.2.
    auto foo = tconc(nil, 1_l);
    tconc(foo, 4_l);
    tconc(foo, 3_l);
    tconc(foo, 2_l);
    tconc(foo, 1_l);
    CHECK(equal(car(foo), mklist(1_l, 4_l, 3_l, 2_l, 1_l)));
    // Another example from the Interlisp manual
    foo = cons(nil, nil);
    tconc(foo, 5_l);
    tconc(foo, 4_l);
    tconc(foo, 3_l);
    tconc(foo, 2_l);
    tconc(foo, 1_l);
    CHECK(equal(car(foo), mklist(5_l, 4_l, 3_l, 2_l, 1_l)));
  }

  SECTION("attach")
  {
    auto foo = mklist(2_l);
    attach(1_l, foo);
    CHECK(equal(foo, mklist(1_l, 2_l)));
    attach(0_l, foo);
    CHECK(equal(foo, mklist(0_l, 1_l, 2_l)));
    auto r = attach(0_l, nil);
    CHECK(equal(r, mklist(0_l)));
  }

  SECTION("length")
  {
    auto list = mklist(0_l, 1_l, 2_l, 3_l);
    CHECK(length(list)->as_integer() == 4);
    CHECK(length(list)->as_integer() == 4);
  }

  SECTION("nth")
  {
    auto foo = mklist(1_l, 2_l, 3_l, 4_l, 5_l);
    CHECK(equal(nth(foo, 2_l), mklist(2_l, 3_l, 4_l, 5_l)));
    CHECK(equal(nth(foo, 3_l), mklist(3_l, 4_l, 5_l)));
    CHECK(equal(nth(foo, 4_l), mklist(4_l, 5_l)));
    CHECK(equal(nth(foo, 5_l), mklist(5_l)));
    CHECK(is_nil(nth(nil, 1_l)));
  }

  SECTION("list")
  {
    auto l = list(mklist(1_l, 2_l));
    CHECK(type_of(l) == object::type::Cons);
    CHECK(length(l)->as_integer() == 2);
  }

  SECTION("null")
  {
    CHECK(!is_nil(null(nil)));
    CHECK(!is_nil(null(nil)));
    CHECK(is_nil(null(0_l)));
    CHECK(is_nil(null(0_l)));
  }
}

} // namespace lisp
