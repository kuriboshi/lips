//
// Lips, lisp shell.
// Copyright 2021-2024 Krister Joas
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

#include <lisp/lisp.hh>
#include "exec.hh"

using namespace lisp;
using namespace std::literals;

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
TEST_CASE("exec.cc: check_meta")
{
  exec::process proc;
  SECTION("test 1")
  {
    auto b = proc.check_meta("hello");
    CHECK(!b.first);
  }
  SECTION("test 2")
  {
    auto b = proc.check_meta("hello*");
    CHECK(b.first);
  }
  SECTION("test 3")
  {
    auto b = proc.check_meta("hello\\*");
    CHECK(!b.first);
    CHECK(b.second == "hello*"s);
  }
  SECTION("test 4")
  {
    auto b = proc.check_meta(R"(hello\*\[\])");
    CHECK(!b.first);
    CHECK(b.second == "hello*[]"s);
  }
  SECTION("test 5")
  {
    auto b = proc.check_meta("hello\\*[a]\\*");
    CHECK(b.first);
    CHECK(b.second == "hello*[a]*"s);
  }
}

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
TEST_CASE("exec.cc: make_exec")
{
  exec::process proc;

  SECTION("(make_exec (a b c)) -> a b c")
  {
    auto result = proc.make_exec(cons(mkstring("a"), cons(mkstring("b"), cons(mkstring("c"), nil))));
    CHECK(result.size() == 3);
    auto i = result.begin();
    CHECK(*i++ == "a");
    CHECK(*i++ == "b");
    CHECK(*i++ == "c");
  }

  SECTION("(make_exec (100)) -> 100")
  {
    auto result = proc.make_exec(cons(mknumber(100), nil)); // NOLINT: Test code
    CHECK(result.at(0) == "100"s);
  }

  SECTION("(make_exec (plus 1 2)) -> 3")
  {
    auto expr = lispread("((plus 1 2))");
    auto result = proc.make_exec(expr);
    CHECK(result.at(0) == "3"s);
  }

  SECTION("(make_exec (/b*)) -> /bin")
  {
    auto expr = lispread("(/b*)");
    auto result = proc.make_exec(expr);
    REQUIRE(!result.empty());
    CHECK(result.at(0) == "/bin"s);
  }

  SECTION("(make_exec (/_*)) -> <empty>")
  {
    auto expr = lispread("(/_*)");
    auto result = proc.make_exec(expr);
    REQUIRE(result.empty());
  }
}

TEST_CASE("execute")
{
  exec::process proc;
  auto result = proc.execute("/bin/ls", cons(mkstring("ls"), nil));
  CHECK(result->as_integer() == 0);
}
