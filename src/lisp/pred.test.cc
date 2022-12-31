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

#include <catch2/catch_test_macros.hpp>

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
    CHECK(numberp(mknumber(1)) != nil);
    CHECK(numberp(mknumber(1)) != nil);
    CHECK(numberp(mkfloat(1.0)) != nil);
    CHECK(numberp(mkfloat(1.0)) != nil);
    CHECK(numberp(mkstring("hello")) == nil);
    CHECK(numberp(mkstring("hello")) == nil);
  }

  SECTION("listp")
  {
    CHECK(listp(cons(mknumber(1), nil)) != nil);
    CHECK(listp(cons(mknumber(1), nil)) != nil);
    CHECK(listp(mkstring("hello")) == nil);
    CHECK(listp(mkstring("hello")) == nil);
    CHECK(listp(nil) == nil);
    CHECK(listp(nil) == nil);
  }

  SECTION("nlistp")
  {
    CHECK(nlistp(cons(mknumber(1), nil)) == nil);
    CHECK(nlistp(cons(mknumber(1), nil)) == nil);
    CHECK(nlistp(mkstring("hello")) != nil);
    CHECK(nlistp(mkstring("hello")) != nil);
    CHECK(nlistp(nil) == T);
    CHECK(nlistp(nil) == T);
  }

  SECTION("neq")
  {
    CHECK(neq(nil, T) == T);
    CHECK(neq(T, T) == nil);
    CHECK(neq(nil, nil) == nil);
    CHECK(neq("a"_s, "a"_s) == T);
  }

  SECTION("boundp")
  {
    CHECK(is_nil(boundp("string"_s)));
    auto ub = mkatom("ub");
    CHECK(is_nil(boundp(ub)));
    auto bd = mkatom("bd");
    set(bd, nil);
    CHECK(is_T(boundp(bd)));
  }

  SECTION("memb")
  {
    CHECK(memb(mknumber(2), mklist(mknumber(1), mknumber(2), mknumber(3))) != nil);
    CHECK(memb(mknumber(2), mklist(mknumber(1), mknumber(2), mknumber(3))) != nil);
    CHECK(memb(mknumber(4), mklist(mknumber(1), mknumber(2), mknumber(3))) == nil);
    CHECK(memb(mknumber(4), mklist(mknumber(1), mknumber(2), mknumber(3))) == nil);
  }

  SECTION("litatom")
  {
    CHECK(litatom(mkatom("a")) != nil);
    CHECK(litatom(mkatom("a")) != nil);
    CHECK(litatom(mkatom("t")) != nil);
    CHECK(litatom(mkatom("t")) != nil);
    CHECK(litatom(mkstring("a")) == nil);
    CHECK(litatom(mkstring("a")) == nil);
  }

  SECTION("equal")
  {
    auto num0 = mknumber(0);
    auto num1 = mknumber(1);
    auto str0 = mkstring("0");
    auto str1 = mkstring("1");
    auto lam0 = lambda(nil, str0);
    auto lam1 = lambda(nil, str1);
    CHECK(equal(num0, mknumber(0)) != nil);
    CHECK(equal(num0, mknumber(0)) != nil);
    CHECK(equal(num0, num1) == nil);
    CHECK(equal(num0, num1) == nil);
    CHECK(equal(str0, mkstring("0")) != nil);
    CHECK(equal(str0, mkstring("0")) != nil);
    CHECK(equal(str0, str1) == nil);
    CHECK(equal(str0, str1) == nil);
    CHECK(equal(lam0, lam1) == nil);
    CHECK(equal(lam0, lam1) == nil);
    CHECK(equal(lam0, lambda(nil, mkstring("0"))) != nil);
    CHECK(equal(lam0, lambda(nil, mkstring("0"))) != nil);
    CHECK(equal(num0, str0) == nil);
    CHECK(equal(num0, str0) == nil);
    CHECK(equal(mklist(num0, num1, num0), mklist(num1, num0, num1)) == nil);
    CHECK(equal(mklist(num0, num1, num0), mklist(num1, num0, num1)) == nil);
  }

  SECTION("typeof")
  {
    CHECK(xtypeof(nil) == nil);
    CHECK(xtypeof(mkatom("symbol")) == C_SYMBOL);
    CHECK(xtypeof(mknumber(0)) == C_INTEGER);
    CHECK(xtypeof(mkfloat(0.0)) == C_FLOAT);
    CHECK(xtypeof(cons(nil, nil)) == C_CONS);
    CHECK(xtypeof(mkstring("foo")) == C_STRING);
    CHECK(xtypeof(eval("plus")) == C_SUBR);
    CHECK(xtypeof(eval("quote")) == C_FSUBR);
    CHECK(xtypeof(lambda(nil, nil)) == C_LAMBDA);
    CHECK(xtypeof(nlambda(nil, nil)) == C_NLAMBDA);
    CHECK(xtypeof(closure(nil, nil)) == C_CLOSURE);
    CHECK(xtypeof(eval("unbound")) == C_UNBOUND);
    CHECK(xtypeof(""_l) == C_ENDOFFILE);
    auto f = lisp_t::create(ref_file_t::create(""));
    CHECK(xtypeof(f) == C_FILE);
  }
}

} // namespace lisp
