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

#include <filesystem>
#include <memory>
#include <sstream>
#include <string>
#include <vector>

#include <catch2/catch.hpp>
using Catch::Matchers::Matches;

#include "alloc.hh"
#include "eval.hh"
#include "file.hh"
#include "low.hh"
#include "pred.hh"
#include "prim.hh"
#include "prop.hh"

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

TEST_CASE("eval: eval functions")
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

TEST_CASE("eval: closure")
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
  auto f = [&called](LISPT, LISPT*) -> int {
    called = true;
    return 1;
  };
  auto old = undefhook(f);
  std::ignore = "(undefined)"_e;
  CHECK(called);
  std::ignore = undefhook(old);
}

TEST_CASE("eval: breakhook")
{
  bool called = false;
  auto f = [&called]() { called = true; };
  auto hook = breakhook(f);
  CHECK_NOTHROW("(undefined)"_e);
  CHECK(called);
  std::ignore = breakhook(hook);
}

TEST_CASE("eval: apply throws")
{
  try
  {
    apply("exit"_a, "(100)"_l);
  }
  catch(const lisp_finish& ex)
  {
    CHECK(ex.exit_code == 100);
  }
}

TEST_CASE("eval: autoload")
{
  std::string autoload{R"((setq auto (lambda () 123))
)"};
  {
    std::ofstream of{"autoload.lisp"};
    of << autoload;
  }
  putprop("auto"_a, "autoload"_a, "autoload.lisp"_a);
  auto result = "(auto)"_e;
  CHECK(type_of(result) == type::Integer);
  CHECK(result->intval() == 123);
  putprop("noauto"_a, "autoload"_a, "autoload.lisp"_a);
  CHECK_NOTHROW("(noauto)"_e);
  std::filesystem::remove("autoload.lisp");
}

TEST_CASE("eval: string function")
{
  auto fun = R"(("fun"))";
  CHECK_NOTHROW(eval(fun));
}

TEST_CASE("eval: illegal function")
{
  auto fun = R"((1))";
  CHECK_NOTHROW(eval(fun));
}

TEST_CASE("eval: indirect and cvariable")
{
  SECTION("indirect function")
  {
    auto& cvar = initcvar("f0", "(lambda () 99)"_l);
    auto result = "(f0)"_e;
    REQUIRE(type_of(result) == type::Integer);
    CHECK(result->intval() == 99);
  }

  SECTION("indirect variable")
  {
    auto& cvar = initcvar("c0", 123_l);
    auto result = "(plus c0)"_e;
    REQUIRE(type_of(result) == type::Integer);
    CHECK(result->intval() == 123);
  }
}

TEST_CASE("eval: illegal apply")
{
  SECTION("apply string")
  {
    CHECK_NOTHROW(R"((apply "string" nil))"_e);
  }

  SECTION("apply unbound")
  {
    CHECK_THROWS(R"((apply unbound nil))"_e);
  }

  SECTION("apply int")
  {
    CHECK_NOTHROW(R"((apply 100 nil))"_e);
  }
}

TEST_CASE("eval: backtrace")
{
  auto& ctx = context::current();
  std::ostringstream err;
  auto old = ctx.primerr(ref_file_t::create(err));
  auto result = R"(
((lambda (a b)
  (a)
  b)
 backtrace
 99)
)"_e;
  const std::vector<std::string> expected{
    R"(18: ev2)",
    R"(17: destblock_t:\(1 \(nil . #<lambda [0-9a-f]+>\)\))",
    R"(16: ev1)",
    R"(15: \(\(a\) b\))",
    R"(14: #<lambda [0-9a-f]+>)",
    R"(13: ev0)",
    R"(12: \(a\))",
    R"(11: evseq3)",
    R"(10: evlam0)",
    R"(9: destblock_t:)",
    R"(8: \(lambda \(a b\) \(a\) b\))",
    R"(7: ev4)",
    R"(6: ev1)",
    R"(5: nil)",
    R"(4: nil)",
    R"(3: ev0)",
    R"(2: \(\(lambda \(a b\) \(a\) b\) backtrace 99\))",
    R"(1: eval0)",
    R"(0: destblock_t:)"
  };
  const auto str = err.str();
  std::istringstream is{str};
  for(auto e: expected)
  {
    std::string line;
    CHECK(std::getline(is, line));
    CHECK_THAT(line, Matches(e));
  }
  ctx.primerr(old);
}

TEST_CASE("eval: backtrace, topofstack, destblock")
{
  auto b = backtrace();
  CHECK(b == NIL);
  auto t = topofstack();
  CHECK(type_of(t) == type::Environ);
  auto d = destblock(t);
  CHECK(d == NIL);
}

} // namespace lisp
