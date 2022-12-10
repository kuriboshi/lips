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

#include "alloc.hh"
#include "low.hh"
#include "pred.hh"
#include "prim.hh"

namespace lisp
{

TEST_CASE("pred: predicate functions")
{
  SECTION("numberp")
  {
    CHECK(numberp(mknumber(1)) != NIL);
    CHECK(numberp(mknumber(1)) != NIL);
    CHECK(numberp(mkfloat(1.0)) != NIL);
    CHECK(numberp(mkfloat(1.0)) != NIL);
    CHECK(numberp(mkstring("hello")) == NIL);
    CHECK(numberp(mkstring("hello")) == NIL);
  }

  SECTION("listp")
  {
    CHECK(listp(cons(mknumber(1), NIL)) != NIL);
    CHECK(listp(cons(mknumber(1), NIL)) != NIL);
    CHECK(listp(mkstring("hello")) == NIL);
    CHECK(listp(mkstring("hello")) == NIL);
    CHECK(listp(NIL) == NIL);
    CHECK(listp(NIL) == NIL);
  }

  SECTION("nlistp")
  {
    CHECK(nlistp(cons(mknumber(1), NIL)) == NIL);
    CHECK(nlistp(cons(mknumber(1), NIL)) == NIL);
    CHECK(nlistp(mkstring("hello")) != NIL);
    CHECK(nlistp(mkstring("hello")) != NIL);
    CHECK(nlistp(NIL) == T);
    CHECK(nlistp(NIL) == T);
  }

  SECTION("neq")
  {
    CHECK(neq(NIL, T) == T);
    CHECK(neq(T, T) == NIL);
    CHECK(neq(NIL, NIL) == NIL);
    CHECK(neq("a"_s, "a"_s) == T);
  }

  SECTION("boundp")
  {
    CHECK(is_NIL(boundp("string"_s)));
    auto ub = mkatom("ub");
    CHECK(is_NIL(boundp(ub)));
    auto bd = mkatom("bd");
    set(bd, NIL);
    CHECK(is_T(boundp(bd)));
  }

  SECTION("memb")
  {
    CHECK(memb(mknumber(2), mklist(mknumber(1), mknumber(2), mknumber(3))) != NIL);
    CHECK(memb(mknumber(2), mklist(mknumber(1), mknumber(2), mknumber(3))) != NIL);
    CHECK(memb(mknumber(4), mklist(mknumber(1), mknumber(2), mknumber(3))) == NIL);
    CHECK(memb(mknumber(4), mklist(mknumber(1), mknumber(2), mknumber(3))) == NIL);
  }

  SECTION("litatom")
  {
    CHECK(litatom(mkatom("a")) != NIL);
    CHECK(litatom(mkatom("a")) != NIL);
    CHECK(litatom(mkatom("t")) != NIL);
    CHECK(litatom(mkatom("t")) != NIL);
    CHECK(litatom(mkstring("a")) == NIL);
    CHECK(litatom(mkstring("a")) == NIL);
  }

  SECTION("equal")
  {
    auto num0 = mknumber(0);
    auto num1 = mknumber(1);
    auto str0 = mkstring("0");
    auto str1 = mkstring("1");
    auto lam0 = lambda(NIL, str0);
    auto lam1 = lambda(NIL, str1);
    CHECK(equal(num0, mknumber(0)) != NIL);
    CHECK(equal(num0, mknumber(0)) != NIL);
    CHECK(equal(num0, num1) == NIL);
    CHECK(equal(num0, num1) == NIL);
    CHECK(equal(str0, mkstring("0")) != NIL);
    CHECK(equal(str0, mkstring("0")) != NIL);
    CHECK(equal(str0, str1) == NIL);
    CHECK(equal(str0, str1) == NIL);
    CHECK(equal(lam0, lam1) == NIL);
    CHECK(equal(lam0, lam1) == NIL);
    CHECK(equal(lam0, lambda(NIL, mkstring("0"))) != NIL);
    CHECK(equal(lam0, lambda(NIL, mkstring("0"))) != NIL);
    CHECK(equal(num0, str0) == NIL);
    CHECK(equal(num0, str0) == NIL);
    CHECK(equal(mklist(num0, num1, num0), mklist(num1, num0, num1)) == NIL);
    CHECK(equal(mklist(num0, num1, num0), mklist(num1, num0, num1)) == NIL);
  }

  SECTION("typeof")
  {
    CHECK(xtypeof(NIL) == NIL);
    CHECK(xtypeof(mkatom("symbol")) == C_SYMBOL);
    CHECK(xtypeof(mknumber(0)) == C_INTEGER);
    CHECK(xtypeof(mkfloat(0.0)) == C_FLOAT);
    CHECK(xtypeof(cons(NIL, NIL)) == C_CONS);
    CHECK(xtypeof(mkstring("foo")) == C_STRING);
    CHECK(xtypeof(eval("plus")) == C_SUBR);
    CHECK(xtypeof(eval("quote")) == C_FSUBR);
    CHECK(xtypeof(lambda(NIL, NIL)) == C_LAMBDA);
    CHECK(xtypeof(nlambda(NIL, NIL)) == C_NLAMBDA);
    CHECK(xtypeof(closure(NIL, NIL)) == C_CLOSURE);
    CHECK(xtypeof(eval("unbound")) == C_UNBOUND);
    CHECK(xtypeof(""_l) == C_ENDOFFILE);
    auto f = LISPT::create(ref_file_t::create(""));
    CHECK(xtypeof(f) == C_FILE);
    CHECK(xtypeof(T) == T);
  }
}

} // namespace lisp
