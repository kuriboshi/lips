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
#include <vector>

#include <catch2/catch.hpp>

#include "alloc.hh"
#include "iter.hh"
#include "prim.hh"
#include "types.hh"
#include "util.hh"
#include "version.hh"

namespace lisp
{

TEST_CASE("lisp: to_underlying")
{
  CHECK(to_underlying(type::Nil) == 0);
  CHECK(to_underlying(type::T) == 1);
  CHECK(to_underlying(type::Empty) == 2);
  CHECK(to_underlying(type::Symbol) == 3);
  CHECK(to_underlying(type::Integer) == 4);
  CHECK(to_underlying(type::Float) == 5);
  CHECK(to_underlying(type::Indirect) == 6);
  CHECK(to_underlying(type::Cons) == 7);
  CHECK(to_underlying(type::String) == 8);
  CHECK(to_underlying(type::Subr) == 9);
  CHECK(to_underlying(type::Lambda) == 10);
  CHECK(to_underlying(type::Closure) == 11);
  CHECK(to_underlying(type::Unbound) == 12);
  CHECK(to_underlying(type::Environ) == 13);
  CHECK(to_underlying(type::File) == 14);
  CHECK(to_underlying(type::Eof) == 15);
  CHECK(to_underlying(type::Error) == 16);
  CHECK(to_underlying(type::Cvariable) == 17);
}

TEST_CASE("lisp: mkprim")
{
  SECTION("Define a new function")
  {
    std::vector<int> result;
    mkprim(
      "printall",
      [&result](context&, LISPT a) -> LISPT {
        for(auto p: a)
        {
          result.push_back(p->intval());
        }
        return NIL;
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
                        "plus", [](context&) -> LISPT { return NIL; }, subr_t::subr::NOEVAL, subr_t::spread::SPREAD),
      "redefinition of subr not allowed");
  }
}

TEST_CASE("lisp: cons")
{
  auto v = cons("a"_a, "b"_a);
  REQUIRE(type_of(v) == type::Cons);
  CHECK(v->cons().car == "a"_a);
  CHECK(v->cons().cdr == "b"_a);
}

TEST_CASE("lisp: type_of")
{
  auto v = "string"_s;
  CHECK(type_of(*v) == type::String);
}

TEST_CASE("lisp: version")
{
  auto version = context::current().version();
  CHECK(version == VERSION);
}

TEST_CASE("lisp: literals")
{
  SECTION("string")
  {
    auto s = "string"_s;
    CHECK(type_of(s) == type::String);
  }

  SECTION("integer")
  {
    auto i = 1001_l;
    CHECK(type_of(i) == type::Integer);
  }

  SECTION("float")
  {
    auto d = 1.2_l;
    CHECK(type_of(d) == type::Float);
  }

  SECTION("sexpr")
  {
    auto sexpr = "((a b) c)"_l;
    REQUIRE(type_of(sexpr) == type::Cons);
    auto a = caar(sexpr);
    CHECK(type_of(a) == type::Symbol);
    CHECK(a->symbol().pname == "a");
    file_t out(std::make_unique<io::string_sink>());
    prin0(sexpr, out);
    CHECK(to_string(out.sink()) == "((a b) c)");
  }

  SECTION("atom")
  {
    auto a = "atom"_a;
    CHECK(type_of(a) == type::Symbol);
  }

  SECTION("eval")
  {
    auto e = "(plus 1 2)"_e;
    REQUIRE(type_of(e) == type::Integer);
    CHECK(e->intval() == 3);
  }
}

TEST_CASE("lisp: iter")
{
  SECTION("regular list")
  {
    auto list = "(a b c)"_l;
    auto i = begin(list);
    CHECK(type_of(*i) == type::Symbol);
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
    CHECK(type_of(*i) == type::Symbol);
    CHECK(*i == "a"_a);
    CHECK(*++i == "b"_a);
    CHECK(*i++ == "b"_a);
    CHECK(i != end(list));
    ++i;
    CHECK(i == end(list));
    // It's possible to dereference the end marker because it's a LISPT with a
    // nil value.
    CHECK(is_NIL(*end(list)));
  }
}

TEST_CASE("lisp: create lisp twice")
{
  auto f = []() { context c; };
  CHECK_THROWS_WITH(f(), "context::context called twice");
}

TEST_CASE("lisp: new lisp_t")
{
  LISPT obj{new lisp_t(100)};
  CHECK(obj->intval() == 100);
}

} // namespace lisp
