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

#include <filesystem>
#include <memory>
#include <sstream>
#include <string>
#include <vector>

#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>
using Catch::Matchers::Matches;

#include "alloc.hh"
#include "file.hh"
#include "low.hh"
#include "predicate.hh"
#include "prim.hh"
#include "property.hh"
#include "vm.hh"

namespace lisp
{

TEST_CASE("eval: LAMBDA and NLAMBDA")
{
  SECTION("LAMBDA - basic case")
  {
    auto a = eval("(setq f (lambda () \"hello\"))");
    auto b = eval("(f)");
    CHECK(type_of(b) == object::type::String);
    CHECK(b->as_string() == "hello");
  }
  SECTION("LAMBDA - one argument")
  {
    auto a = eval("(setq f (lambda (x) (cons x nil)))");
    auto b = eval("(f 10)");
    CHECK(type_of(b) == object::type::Cons);
    CHECK(type_of(b->car()) == object::type::Integer);
    CHECK(b->car()->as_integer() == 10);
  }
  SECTION("LAMBDA - spread case")
  {
    auto a = eval("(setq f (lambda x (cadr x)))");
    auto b = eval("(f 1 2)");
    CHECK(type_of(b) == object::type::Integer);
    CHECK(b->as_integer() == 2);
  }
  SECTION("LAMBDA - half spread")
  {
    auto a = eval("(setq f (lambda (a . x) (list a (cadr x))))");
    auto b = eval("(f 0 1 2)");
    CHECK(type_of(b) == object::type::Cons);
    CHECK(type_of(b->car()) == object::type::Integer);
    CHECK(b->car()->as_integer() == 0);
    CHECK(type_of(b->cdr()->car()) == object::type::Integer);
    CHECK(b->cdr()->car()->as_integer() == 2);
  }
  SECTION("NLAMBDA - basic case")
  {
    auto a = eval("(setq f (nlambda (a) a))");
    auto b = eval("(f x)");
    CHECK(type_of(b) == object::type::Symbol);
    CHECK(b->as_symbol()->pname == "x");
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
    CHECK(r0->as_integer() == 123);
  }

  SECTION("Evaluate simple expression: (plus 123 1)")
  {
    auto e1 = cons(mkatom("plus"), cons(mknumber(123), cons(mknumber(1), nullptr)));
    auto out0 = std::make_unique<file_t>(std::make_unique<io::string_sink>());
    prin0(e1, *out0.get());
    CHECK(to_string(out0->sink()) == std::string("(plus 123 1)"));
    auto r1 = eval(e1);
    CHECK(r1->as_integer() == 124);
  }
}

TEST_CASE("eval: closure")
{
  auto a = setq(mkatom("a"), mknumber(1));
  auto clos = closure(lambda(nil, cons(mkatom("a"), nil)), cons(mkatom("a"), nil));
  auto r0 = eval(cons(clos, nil));
  a = setq(mkatom("a"), mknumber(2));
  auto r1 = eval(apply(clos, nil));
  CHECK(equal(r0, r1) != nil);
}

TEST_CASE("eval: topofstack")
{
  auto a = set(mkatom("a"), 88_l);
  eval(R"(
(defineq (f0 (lambda (a) (destblock (topofstack))))
         (f1 (lambda (a) (f0 a))))
)");
  auto r0 = eval("(f0 101)");
  CHECK(!is_nil(equal(mklist(1_l, cons(mkatom("a"), 88_l)), r0)));
  auto r1 = eval("(f1 99)");
  CHECK(!is_nil(equal(mklist(1_l, cons(mkatom("a"), 99_l)), r1)));
}

TEST_CASE("eval: control limits")
{
  std::ostringstream err;
  auto old = vm::primerr(ref_file_t::create(err));
  "(defineq (f (lambda () (f))))"_e;
  CHECK_THROWS_WITH("(f)"_e, "Abort");
  CHECK(err.str() == "Stack overflow [in f]\n");
  vm::primerr(old);
}

TEST_CASE("eval: undefhook")
{
  bool called = false;
  auto f = [&called](lisp_t, lisp_t*) -> int {
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
  CHECK_THROWS("(undefined)"_e);
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
  CHECK(type_of(result) == object::type::Integer);
  CHECK(result->as_integer() == 123);
  putprop("noauto"_a, "autoload"_a, "autoload.lisp"_a);
  CHECK_THROWS("(noauto)"_e);
  std::filesystem::remove("autoload.lisp");
}

TEST_CASE("eval: string function")
{
  auto fun = R"(("fun"))";
  CHECK_THROWS(eval(fun));
}

TEST_CASE("eval: illegal function")
{
  auto fun = R"((1))";
  CHECK_THROWS(eval(fun));
}

TEST_CASE("eval: indirect and cvariable")
{
  SECTION("indirect function")
  {
    auto& cvar = initcvar("f0", "(lambda () 99)"_l);
    auto result = "(f0)"_e;
    REQUIRE(type_of(result) == object::type::Integer);
    CHECK(result->as_integer() == 99);
  }

  SECTION("indirect variable")
  {
    auto& cvar = initcvar("c0", 123_l);
    auto result = "(plus c0)"_e;
    REQUIRE(type_of(result) == object::type::Integer);
    CHECK(result->as_integer() == 123);
  }
}

TEST_CASE("eval: illegal apply")
{
  SECTION("apply string") { CHECK_THROWS(R"((apply "string"))"_e); }

  SECTION("apply unbound") { CHECK_THROWS(R"((apply unbound))"_e); }

  SECTION("apply int") { CHECK_THROWS(R"((apply 100))"_e); }
}

TEST_CASE("eval: backtrace")
{
  std::ostringstream err;
  auto old = vm::primerr(ref_file_t::create(err));
  auto result = R"(
((lambda (a b)
  (a)
  b)
 backtrace
 99)
)"_e;
  // clang-format off
  const std::vector<std::string> expected{
    R"(17: destblock_t: 1 \(nil . #<lambda [0-9a-f]+>\))",
    R"(16: cont: ev1)",
    R"(15: args: \(\(a\) b\))",
    R"(14: fun: #<lambda [0-9a-f]+>)",
    R"(13: cont: eval_end)",
    R"(12: expr: \(a\))",
    R"(11: cont: eval_seq2)",
    R"(10: cont: evlam0)",
    R"(9: destblock_t: nil)",
    R"(8: expr: \(lambda \(a b\) \(a\) b\))",
    R"(7: cont: ev4)",
    R"(6: cont: ev1)",
    R"(5: args: nil)",
    R"(4: fun: nil)",
    R"(3: cont: eval_end)",
    R"(2: expr: \(\(lambda \(a b\) \(a\) b\) backtrace 99\))",
    R"(1: cont: eval0)",
    R"(0: destblock_t: nil)"
  };
  // clang-format on
  const auto str = err.str();
  std::istringstream is{str};
  for(auto e: expected)
  {
    std::string line;
    CHECK(std::getline(is, line));
    CHECK_THAT(line, Matches(e));
  }
  vm::primerr(old);
}

TEST_CASE("eval: backtrace, topofstack, destblock")
{
  auto b = backtrace();
  CHECK(b == nil);
  auto t = topofstack();
  CHECK(type_of(t) == object::type::Environ);
  auto d = destblock(t);
  CHECK(d == nil);
}

TEST_CASE("vm: trace")
{
  std::ostringstream os;
  class redirect
  {
  public:
    redirect(std::ostream& stream, std::ostream& output)
      : _stream(stream),
        _buf(_stream.rdbuf(output.rdbuf()))
    {}
    ~redirect() { _stream.rdbuf(_buf); }

  private:
    std::ostream& _stream;
    decltype(_stream.rdbuf()) _buf;
  } rdbuf{std::cerr, os};
  class trace
  {
  public:
    trace()
      : _old(vm::get().trace(true))
    {}
    ~trace() { vm::get().trace(_old); }

  private:
    bool _old;
  } t;
  auto r = eval(R"((plus 1 2 3))");
  REQUIRE(type_of(r) == object::type::Integer);
  CHECK(r->as_integer() == 6);
}

template<class T>
void pool_test()
{
  new T(pool_test_t());
}

TEST_CASE("pool: object and closure_t")
{
  auto c0 = lisp::object::freecount();
  CHECK_THROWS(pool_test<lisp::object>());
  CHECK(c0 == lisp::object::freecount());
  c0 = lisp::closure_t::freecount();
  CHECK_THROWS(pool_test<lisp::closure_t>());
  CHECK(c0 == lisp::closure_t::freecount());
  c0 = lisp::lambda_t::freecount();
  CHECK_THROWS(pool_test<lisp::lambda_t>());
  CHECK(c0 == lisp::lambda_t::freecount());
  c0 = lisp::cons_t::freecount();
  CHECK_THROWS(pool_test<lisp::cons_t>());
  CHECK(c0 == lisp::cons_t::freecount());
  c0 = lisp::string_t::freecount();
  CHECK_THROWS(pool_test<lisp::string_t>());
  CHECK(c0 == lisp::string_t::freecount());
}

} // namespace lisp
