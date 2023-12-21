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

#include <memory>
#include <vector>

#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers.hpp>

#include "alloc.hh"
#include "file.hh"
#include "iter.hh"
#include "pred.hh"
#include "prim.hh"
#include "types.hh"
#include "util.hh"
#include "version.hh"

namespace lisp
{

TEST_CASE("lisp: to_underlying")
{
  CHECK(to_underlying(object::type::Nil) == 0);
  CHECK(to_underlying(object::type::Symbol) == 1);
  CHECK(to_underlying(object::type::Integer) == 2);
  CHECK(to_underlying(object::type::Float) == 3);
  CHECK(to_underlying(object::type::Indirect) == 4);
  CHECK(to_underlying(object::type::Cons) == 5);
  CHECK(to_underlying(object::type::String) == 6);
  CHECK(to_underlying(object::type::Subr) == 7);
  CHECK(to_underlying(object::type::Lambda) == 8);
  CHECK(to_underlying(object::type::Closure) == 9);
  CHECK(to_underlying(object::type::Environ) == 10);
  CHECK(to_underlying(object::type::File) == 11);
  CHECK(to_underlying(object::type::Cvariable) == 12);
}

TEST_CASE("lisp: mkprim")
{
  SECTION("Define a new function")
  {
    std::vector<integer_t::value_type> result;
    mkprim(
      "printall",
      [&result](lisp_t a) -> lisp_t {
        for(auto p: a)
        {
          result.push_back(p->as_integer());
        }
        return nil;
      },
      subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
    eval("(printall 0 1 2)");
    CHECK(result[0] == 0);
    CHECK(result[1] == 1);
    CHECK(result[2] == 2);
  }

  SECTION("Redefine subr")
  {
    CHECK_THROWS_WITH(mkprim(
                        "plus", []() -> lisp_t { return nil; }, subr_t::subr::NOEVAL, subr_t::spread::SPREAD),
      "Redefinition of subr not allowed: plus");
  }

  SECTION("four arguments")
  {
    mkprim(
      "four_arguments", [](lisp_t a, lisp_t b, lisp_t c, lisp_t d) { return mklist(a, b, c, d); }, subr_t::subr::NOEVAL,
      subr_t::spread::SPREAD);
    auto result = eval("(four_arguments 1 2 3 4)");
    CHECK(is_T(equal(result, "(1 2 3 4)"_l)));
  }

  SECTION("four arguments, nospread")
  {
    mkprim(
      "four_arguments_nospread", [](lisp_t a, lisp_t b, lisp_t c, lisp_t d) { return mklist(a, b, c, d); },
      subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
    auto result = eval("(four_arguments_nospread 1 2 3 4)");
    CHECK(is_T(equal(result, "(1 2 3 (4))"_l)));
  }

  SECTION("four const arguments")
  {
    mkprim(
      "four_arguments_const",
      [](const lisp_t& a, const lisp_t& b, const lisp_t& c, const lisp_t& d) { return mklist(a, b, c, d); },
      subr_t::subr::NOEVAL, subr_t::spread::SPREAD);
    auto result = eval("(four_arguments_const 1 2 3 4)");
    CHECK(is_T(equal(result, "(1 2 3 4)"_l)));
  }
}

TEST_CASE("lisp: cons")
{
  auto v = cons("a"_a, "b"_a);
  REQUIRE(type_of(v) == object::type::Cons);
  CHECK(v->cons().car == "a"_a);
  CHECK(v->cons().cdr == "b"_a);
}

TEST_CASE("lisp: type_of")
{
  auto v = "string"_s;
  CHECK(type_of(*v) == object::type::String);
}

TEST_CASE("lisp: version")
{
  auto version = vm::version();
  CHECK(version == lisp::version());
}

TEST_CASE("lisp: literals")
{
  SECTION("string")
  {
    auto s = "string"_s;
    CHECK(type_of(s) == object::type::String);
  }

  SECTION("integer")
  {
    auto i = 1001_l;
    CHECK(type_of(i) == object::type::Integer);
  }

  SECTION("float")
  {
    auto d = 1.2_l;
    CHECK(type_of(d) == object::type::Float);
  }

  SECTION("sexpr")
  {
    auto sexpr = "((a b) c)"_l;
    REQUIRE(type_of(sexpr) == object::type::Cons);
    auto a = caar(sexpr);
    CHECK(type_of(a) == object::type::Symbol);
    CHECK(a->as_symbol()->pname == "a");
    file_t out(std::make_unique<io::string_sink>());
    prin0(sexpr, out);
    CHECK(to_string(out.sink()) == "((a b) c)");
  }

  SECTION("atom")
  {
    auto a = "atom"_a;
    CHECK(type_of(a) == object::type::Symbol);
  }

  SECTION("eval")
  {
    auto e = "(plus 1 2)"_e;
    REQUIRE(type_of(e) == object::type::Integer);
    CHECK(e->as_integer() == 3);
  }
}

TEST_CASE("lisp: iter")
{
  SECTION("regular list")
  {
    auto list = "(a b c)"_l;
    auto i = begin(list);
    CHECK(type_of(*i) == object::type::Symbol);
    CHECK(*i == "a"_a);
    CHECK(*++i == "b"_a);
    CHECK(*i++ == "b"_a);
    ++i;
    CHECK(i == end(list));
  }

  SECTION("dotted pair")
  {
    // The current behaviour for a dotted pair is strange and should be
    // considered undefined. In the case below it will iterate over three
    // elements but the last one will be nil and 'c' will never be accessed.
    auto list = "(a b . c)"_l;
    auto i = begin(list);
    CHECK(type_of(*i) == object::type::Symbol);
    CHECK(*i == "a"_a);
    CHECK(*++i == "b"_a);
    CHECK(*i++ == "b"_a);
    CHECK(i != end(list));
    ++i;
    CHECK(i == end(list));
    // It's possible to dereference the end marker because it's a lisp_t with a
    // nil value.
    CHECK(is_nil(*end(list)));
  }
}

TEST_CASE("lisp: new object")
{
  lisp_t obj{new object{100}};
  CHECK(obj->as_integer() == 100);
}

TEST_CASE("lisp: object move constructor")
{
  object o{123};
  REQUIRE(type_of(o) == object::type::Integer);
  CHECK(o.as_integer() == 123);
  auto o1 = std::move(o);
  CHECK(type_of(o) == object::type::Nil);
  CHECK(is_nil(o));
  REQUIRE(type_of(o1) == object::type::Integer);
  CHECK(o1.as_integer() == 123);
}

TEST_CASE("lisp: integer_t")
{
  SECTION("default constructor")
  {
    integer_t value{};
    CHECK(value.value() == 0);
  }
}

TEST_CASE("lisp: double_t")
{
  SECTION("default constructor")
  {
    double_t value{};
    CHECK(value.value() == 0.0);
  }
}

} // namespace lisp
