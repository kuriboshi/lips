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
  auto out = ref_file_t::create(cout);
  l.primout(std::move(out));

  std::ostringstream cerr;
  auto err = ref_file_t::create(cerr);
  l.primerr(std::move(err));

  SECTION("Simple repl")
  {
    l.primin(ref_file_t::create(R"((print "hello"))"));
    repl repl(l);
    repl(NIL);
    std::string expected = R"(> "hello"
"hello"
> )";
    CHECK(cout.str() == expected);
  }

  SECTION("Example interaction")
  {
    std::string is = R"(
()
(setq a 100)
a
)";
    l.primin(ref_file_t::create(is));
    repl repl(l);
    repl(NIL);
    std::string expected = R"(> nil
> 100
> 100
> )";
    CHECK(cout.str() == expected);
  }

  SECTION("Break repl (reset)")
  {
    unwind();
    std::string is = R"(((lambda () (xyzzy)))
(reset)
)";
    l.primin(ref_file_t::create(is));
    repl repl(l);
    l.repl = [&repl](LISPT) -> LISPT { return repl(NIL); };
    CHECK_THROWS(repl(NIL));
    std::string expected_err = R"(Undefined function xyzzy
(xyzzy broken)
)";
    std::string expected_out = R"(> : )";
    CHECK(cout.str() == expected_out);
    CHECK(cerr.str() == expected_err);
  }

  SECTION("Break repl (bt)")
  {
    std::string is = R"(((lambda () (xyzzy)))
(bt)
(reset)
)";
    l.primin(ref_file_t::create(is));
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
  }

  SECTION("Break repl (return)")
  {
    std::string is = R"(((lambda () (xyzzy)))
(return "hello")
)";
    l.primin(ref_file_t::create(is));
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
  }
}

} // namespace lisp
