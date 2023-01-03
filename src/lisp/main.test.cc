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

#include <sstream>

#include <catch2/catch_test_macros.hpp>

#include "alloc.hh"
#include "check.hh"
#include "io.hh"
#include "run.hh"

namespace lisp
{

TEST_CASE("main: incomplete input")
{
  // An incomplete input expression is treated as ending with a super
  // parenthesis so there should be no error message in this case.
  auto& ctx = context::current();
  std::ostringstream cout;
  auto oldout = ctx.primout(ref_file_t::create(cout));
  auto oldin = ctx.primin(ref_file_t::create(R"((print "hello")"));
  std::ostringstream os;
  run(ctx, os);
  CHECK(os.str() == "");
  CHECK(cout.str() == R"(> "hello"
"hello"
> )");
  ctx.primin(oldin);
  ctx.primout(oldout);
}

TEST_CASE("main: exit")
{
  auto& ctx = context::current();
  std::ostringstream cout;
  auto old = ctx.primout(ref_file_t::create(cout));
  {
    auto old = ctx.primin(ref_file_t::create(R"((exit))"));
    std::ostringstream os;
    CHECK(run(ctx, os) == 0);
    ctx.primin(old);
  }
  {
    auto old = ctx.primin(ref_file_t::create(R"((exit 99))"));
    std::ostringstream os;
    CHECK(run(ctx, os) == 99);
    ctx.primin(old);
  }
  ctx.primout(old);
}

TEST_CASE("main: reset")
{
  auto& ctx = context::current();
  std::ostringstream cout;
  auto old = ctx.primout(ref_file_t::create(cout));

  SECTION("throw lisp_reset")
  {
    // The symbol 'foo' is undefined so this will throw a lisp_reset exception
    // which is caught by 'run' which returns 0.
    auto old = ctx.primin(ref_file_t::create(R"((foo))"));
    std::ostringstream os;
    CHECK(run(ctx, os) == 0);
    ctx.primin(old);
  }

  SECTION("context interrupt")
  {
    auto old = ctx.primin(ref_file_t::create(R"((plus 1 2))"));
    // This will cause abort to be called and a lisp_error exception to be
    // called.
    ctx.interrupt = true;
    std::ostringstream os;
    CHECK(run(ctx, os) == 0);
    ctx.interrupt = false;
    ctx.primin(old);
  }

  SECTION("std::exception")
  {
    // Throw a standard exception which will reset the vm.
    mkprim(
      "throw",
      [](lisp_t a) -> lisp_t {
        check(a, object::type::String);
        throw std::runtime_error(a->string());
      },
      subr_t::subr::NOEVAL, subr_t::spread::SPREAD);
    auto old = ctx.primin(ref_file_t::create(R"((throw "exception"))"));
    std::ostringstream os;
    CHECK(run(ctx, os) == 0);
    CHECK(os.str() == "exception: exception\n");
    ctx.primin(old);
  }

  SECTION("unwind")
  {
    // Unwind the stack before throwing a standard exception
    mkprim(
      "throw_unwind",
      [](lisp_t a) -> lisp_t {
        check(a, object::type::String);
        // The lamda ensure we have an environment at this point.
        CHECK(topofstack()->environ() != nil);
        // Unwind the stack and check if the stack has been unwound.
        unwind();
        CHECK(topofstack()->environ() == nil);
        // Returning at this point doesn't work so we bail out with an
        // exception.
        throw std::runtime_error(a->string());
      },
      subr_t::subr::NOEVAL, subr_t::spread::SPREAD);
    // Throw inside a lambda so that we have one environment.
    auto old = ctx.primin(ref_file_t::create(R"(((lambda () (throw_unwind "throw_unwind"))))"));
    std::ostringstream os;
    CHECK(run(ctx, os) == 0);
    CHECK(os.str() == "exception: throw_unwind\n");
    ctx.primin(old);
  }

  ctx.primout(old);
}

} // namespace lisp
