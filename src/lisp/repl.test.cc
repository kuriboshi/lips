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
#include <iostream>
#include <lisp/lisp.hh>
#include <lisp/repl.hh>

namespace lisp
{

TEST_CASE("Interactive tests")
{
  auto& l = lisp::current();
  std::ostringstream cout;
  auto oldout = l.primout(ref_file_t::create(cout));

  std::ostringstream cerr;
  auto olderr = l.primerr(ref_file_t::create(cerr));

  SECTION("Simple repl")
  {
    auto old = l.primin(ref_file_t::create(R"((print "hello"))"));
    repl repl(l);
    repl(NIL);
    std::string expected = R"(> "hello"
"hello"
> )";
    CHECK(cout.str() == expected);
    l.primin(old);
  }

  SECTION("Example interaction")
  {
    std::string is = R"(
()
(setq a 100)
a
)";
    auto old = l.primin(ref_file_t::create(is));
    repl repl(l);
    repl(NIL);
    std::string expected = R"(> nil
> 100
> 100
> )";
    CHECK(cout.str() == expected);
    l.primin(old);
  }

  SECTION("Break repl (reset)")
  {
    std::string is = R"(((lambda () (xyzzy)))
(reset)
)";
    auto old = l.primin(ref_file_t::create(is));
    repl repl(l);
    l.repl = [&repl](LISPT) -> LISPT { return repl(NIL); };
    CHECK_THROWS(repl(NIL));
    std::string expected_err = R"(Undefined function xyzzy
(xyzzy broken)
)";
    std::string expected_out = R"(> : )";
    CHECK(cout.str() == expected_out);
    CHECK(cerr.str() == expected_err);
    l.primin(old);
  }

  SECTION("Break repl (bt)")
  {
    std::string is = R"(((lambda () (xyzzy)))
(bt)
(reset)
)";
    auto old = l.primin(ref_file_t::create(is));
    repl repl(l);
    l.repl = [&repl](LISPT) -> LISPT { return repl(NIL); };
    CHECK_THROWS(repl(NIL));
    // Backtrace sets the print level to 2 which is why the '&' is printed
    // representing a deeper level.
    std::string expected_err = R"(Undefined function xyzzy
(xyzzy broken)
(xyzzy)
((lambda nil &))
)";
    std::string expected_out = R"(> : : )";
    CHECK(cout.str() == expected_out);
    CHECK(cerr.str() == expected_err);
    l.primin(old);
  }

  SECTION("Break repl (return)")
  {
    std::string is = R"(((lambda () (xyzzy)))
(return "hello")
)";
    auto old = l.primin(ref_file_t::create(is));
    repl repl(l);
    l.repl = [&repl](LISPT) -> LISPT { return repl(NIL); };
    repl(NIL);
    std::string expected_err = R"(Undefined function xyzzy
(xyzzy broken)
)";
    std::string expected_out = R"(> : "hello"
> )";
    CHECK(cout.str() == expected_out);
    CHECK(cerr.str() == expected_err);
    l.primin(old);
  }
  l.primerr(olderr);
  l.primout(oldout);
}

} // namespace lisp
