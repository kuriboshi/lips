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

#include <memory>
#include <sstream>
#include <string>

#include <catch2/catch.hpp>

#include "alloc.hh"
#include "eval.hh"
#include "low.hh"
#include "pred.hh"
#include "prim.hh"

namespace lisp
{

TEST_CASE("eval: LAMBDA and NLAMBDA")
{
  SECTION("LAMBDA - basic case")
  {
    auto a = eval("(setq f (lambda () \"hello\"))");
    auto b = eval("(f)");
    CHECK(type_of(b) == type::String);
    CHECK(b->string() == "hello");
  }
  SECTION("LAMBDA - one argument")
  {
    auto a = eval("(setq f (lambda (x) (cons x nil)))");
    auto b = eval("(f 10)");
    CHECK(type_of(b) == type::Cons);
    CHECK(type_of(b->car()) == type::Integer);
    CHECK(b->car()->intval() == 10);
  }
  SECTION("LAMBDA - spread case")
  {
    auto a = eval("(setq f (lambda x (cadr x)))");
    auto b = eval("(f 1 2)");
    CHECK(type_of(b) == type::Integer);
    CHECK(b->intval() == 2);
  }
  SECTION("LAMBDA - half spread")
  {
    auto a = eval("(setq f (lambda (a . x) (list a (cadr x))))");
    auto b = eval("(f 0 1 2)");
    CHECK(type_of(b) == type::Cons);
    CHECK(type_of(b->car()) == type::Integer);
    CHECK(b->car()->intval() == 0);
    CHECK(type_of(b->cdr()->car()) == type::Integer);
    CHECK(b->cdr()->car()->intval() == 2);
  }
  SECTION("NLAMBDA - basic case")
  {
    auto a = eval("(setq f (nlambda (a) a))");
    auto b = eval("(f x)");
    CHECK(type_of(b) == type::Symbol);
    CHECK(b->symbol().pname == "x");
  }
}

TEST_CASE("eval: Eval functions")
{
  SECTION("Evaluate variable")
  {
    auto var = mkatom("i");
    auto val = mknumber(123);
    set(var, val);
    auto r0 = eval(var);
    CHECK(r0->intval() == 123);
  }

  SECTION("Evaluate simple expression: (plus 123 1)")
  {
    auto e1 = cons(mkatom("plus"), cons(mknumber(123), cons(mknumber(1), nullptr)));
    auto out0 = std::make_unique<file_t>(std::make_unique<io::string_sink>());
    prin0(e1, *out0.get());
    CHECK(to_string(out0->sink()) == std::string("(plus 123 1)"));
    auto r1 = eval(e1);
    CHECK(r1->intval() == 124);
  }
}

TEST_CASE("eval: Closure")
{
  auto a = setq(mkatom("a"), mknumber(1));
  auto clos = closure(lambda(NIL, cons(mkatom("a"), NIL)), cons(mkatom("a"), NIL));
  auto r0 = eval(cons(clos, NIL));
  a = setq(mkatom("a"), mknumber(2));
  auto r1 = eval(apply(clos, NIL));
  CHECK(equal(r0, r1) != NIL);
}

TEST_CASE("eval: topofstack")
{
  auto a = set(mkatom("a"), 88_l);
  eval(R"(
(defineq (f0 (lambda (a) (destblock (topofstack))))
         (f1 (lambda (a) (f0 a))))
)");
  auto r0 = eval("(f0 101)");
  CHECK(!is_NIL(equal(mklist(1_l, cons(mkatom("a"), 88_l)), r0)));
  auto r1 = eval("(f1 99)");
  CHECK(!is_NIL(equal(mklist(1_l, cons(mkatom("a"), 99_l)), r1)));
}

TEST_CASE("eval: control limits")
{
  auto& ctx = context::current();
  std::ostringstream err;
  auto old = ctx.primerr(ref_file_t::create(err));
  "(defineq (f (lambda () (f))))"_e;
  CHECK_THROWS_WITH("(f)"_e, "abort");
  CHECK(err.str() == "Stack overflow [in f]\n");
  ctx.primerr(old);
}

TEST_CASE("eval: undefhook")
{
  bool called = false;
  auto f = [&called](LISPT, LISPT*) -> int { called = true; return 1; };
  auto old = undefhook(f);
  std::ignore = "(undefined)"_e;
  CHECK(called);
  std::ignore = undefhook(old);
}

} // namespace lisp
