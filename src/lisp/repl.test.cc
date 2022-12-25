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

#include <iostream>
#include <sstream>
#include <string>

#include <catch2/catch.hpp>

#include "io.hh"
#include "repl.hh"

namespace lisp
{

TEST_CASE("repl: interactive tests")
{
  auto& ctx = context::current();
  std::ostringstream cout;
  auto oldout = ctx.primout(ref_file_t::create(cout));

  std::ostringstream cerr;
  auto olderr = ctx.primerr(ref_file_t::create(cerr));

  SECTION("Simple repl")
  {
    auto old = ctx.primin(ref_file_t::create(R"((print "hello"))"));
    repl repl(ctx);
    repl(NIL);
    std::string expected = R"(> "hello"
"hello"
> )";
    CHECK(cout.str() == expected);
    ctx.primin(old);
  }

  SECTION("Example interaction")
  {
    std::string is = R"(
()
(setq a 100)
a
)";
    auto old = ctx.primin(ref_file_t::create(is));
    repl repl(ctx);
    repl(NIL);
    std::string expected = R"(> nil
> 100
> 100
> )";
    CHECK(cout.str() == expected);
    ctx.primin(old);
  }

  SECTION("Break repl (reset)")
  {
    std::string is = R"(((lambda () (xyzzy)))
(reset)
)";
    auto old = ctx.primin(ref_file_t::create(is));
    repl repl(ctx);
    ctx.repl = [&repl](lisp_t) -> lisp_t { return repl(NIL); };
    CHECK_NOTHROW(repl(NIL));
    std::string expected_err = R"(Undefined function xyzzy
(xyzzy broken)
)";
    std::string expected_out = R"(> : > )";
    CHECK(cout.str() == expected_out);
    CHECK(cerr.str() == expected_err);
    ctx.primin(old);
  }

  SECTION("Break repl (bt)")
  {
    std::string is = R"(((lambda () (xyzzy)))
(bt)
(reset)
)";
    auto old = ctx.primin(ref_file_t::create(is));
    repl repl(ctx);
    ctx.repl = [&repl](lisp_t) -> lisp_t { return repl(NIL); };
    CHECK_NOTHROW(repl(NIL));
    // Backtrace sets the print level to 2 which is why the '&' is printed
    // representing a deeper level.
    std::string expected_err = R"(Undefined function xyzzy
(xyzzy broken)
(xyzzy)
((lambda nil &))
)";
    std::string expected_out = R"(> : : > )";
    CHECK(cout.str() == expected_out);
    CHECK(cerr.str() == expected_err);
    ctx.primin(old);
  }

  SECTION("Break repl (return)")
  {
    std::string is = R"(((lambda () (xyzzy)))
(return "hello")
)";
    auto old = ctx.primin(ref_file_t::create(is));
    repl repl(ctx);
    ctx.repl = [&repl](lisp_t) -> lisp_t { return repl(NIL); };
    repl(NIL);
    std::string expected_err = R"(Undefined function xyzzy
(xyzzy broken)
)";
    std::string expected_out = R"(> : "hello"
> )";
    CHECK(cout.str() == expected_out);
    CHECK(cerr.str() == expected_err);
    ctx.primin(old);
  }

  SECTION("help")
  {
    std::string is = R"(((lambda () (xyzzy)))
help
(return nil)
)";
    auto old = ctx.primin(ref_file_t::create(is));
    repl repl(ctx);
    ctx.repl = [&repl](lisp_t) -> lisp_t { return repl(NIL); };
    repl(NIL);
    std::string expected_err = R"(Undefined function xyzzy
(xyzzy broken)
)";
    std::string expected_out = R"(> : (go) continue
(reset) back to top loop
(bt) print backtrace
(return exp) return expression
: nil
> )";
    CHECK(cout.str() == expected_out);
    CHECK(cerr.str() == expected_err);
    ctx.primin(old);
  }

  ctx.primerr(olderr);
  ctx.primout(oldout);
}

} // namespace lisp
