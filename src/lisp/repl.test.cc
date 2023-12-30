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

#include <iostream>
#include <sstream>
#include <string>

#include <catch2/catch_test_macros.hpp>

#include "io.hh"
#include "repl.hh"
#include "vm.hh"

namespace lisp
{

TEST_CASE("repl: interactive tests")
{
  std::ostringstream cout;
  auto oldout = vm::primout(ref_file_t::create(cout));

  std::ostringstream cerr;
  auto olderr = vm::primerr(ref_file_t::create(cerr));

  SECTION("Simple repl")
  {
    auto old = vm::primin(ref_file_t::create(R"((print "hello"))"));
    repl repl(vm::get());
    repl(nil);
    std::string expected = R"(> "hello"
"hello"
> )";
    CHECK(cout.str() == expected);
    vm::primin(old);
  }

  SECTION("Example interaction")
  {
    std::string is = R"(
()
(setq a 100)
a
)";
    auto old = vm::primin(ref_file_t::create(is));
    repl repl(vm::get());
    repl(nil);
    std::string expected = R"(> nil
> 100
> 100
> )";
    CHECK(cout.str() == expected);
    vm::primin(old);
  }

  SECTION("Break repl (go)")
  {
    std::string is = R"(((lambda () (repl_go)))
(defineq (repl_go (lambda ())))
(go)
)";
    auto old = vm::primin(ref_file_t::create(is));
    repl repl(vm::get());
    vm::get().repl = [&repl](lisp_t) -> lisp_t { return repl(nil); };
    CHECK(is_nil(repl(nil)));
    std::string expected_err = R"(Undefined function repl_go
(repl_go broken)
)";
    std::string expected_out = R"(> 1: (repl_go)
1: nil
> )";
    CHECK(cout.str() == expected_out);
    CHECK(cerr.str() == expected_err);
    vm::primin(old);
  }

  SECTION("Break repl (reset)")
  {
    std::string is = R"(((lambda () (xyzzy)))
(reset)
)";
    auto old = vm::primin(ref_file_t::create(is));
    repl repl(vm::get());
    vm::get().repl = [&repl](lisp_t) -> lisp_t { return repl(nil); };
    CHECK_THROWS(repl(nil));
    std::string expected_err = R"(Undefined function xyzzy
(xyzzy broken)
)";
    std::string expected_out = R"(> 1: )";
    CHECK(cout.str() == expected_out);
    CHECK(cerr.str() == expected_err);
    vm::primin(old);
  }

  SECTION("Break repl (bt)")
  {
    std::string is = R"(((lambda () (xyzzy)))
(bt)
(reset)
)";
    auto old = vm::primin(ref_file_t::create(is));
    repl repl(vm::get());
    vm::get().repl = [&repl](lisp_t) -> lisp_t { return repl(nil); };
    CHECK_THROWS(repl(nil));
    // Backtrace sets the print level to 2 which is why the '&' is printed
    // representing a deeper level.
    std::string expected_err = R"(Undefined function xyzzy
(xyzzy broken)
(xyzzy)
((lambda nil &))
)";
    std::string expected_out = R"(> 1: 1: )";
    CHECK(cout.str() == expected_out);
    CHECK(cerr.str() == expected_err);
    vm::primin(old);
  }

  SECTION("Break repl (return)")
  {
    std::string is = R"(((lambda () (xyzzy)))
(return "hello")
)";
    auto old = vm::primin(ref_file_t::create(is));
    repl repl(vm::get());
    vm::get().repl = [&repl](lisp_t) -> lisp_t { return repl(nil); };
    repl(nil);
    std::string expected_err = R"(Undefined function xyzzy
(xyzzy broken)
)";
    std::string expected_out = R"(> 1: "hello"
> )";
    CHECK(cout.str() == expected_out);
    CHECK(cerr.str() == expected_err);
    vm::primin(old);
  }

  SECTION("help")
  {
    std::string is = R"(((lambda () (xyzzy)))
help
(return nil)
)";
    auto old = vm::primin(ref_file_t::create(is));
    repl repl(vm::get());
    vm::get().repl = [&repl](lisp_t) -> lisp_t { return repl(nil); };
    repl(nil);
    std::string expected_err = R"(Undefined function xyzzy
(xyzzy broken)
)";
    std::string expected_out = R"(> 1: (go) continue
(reset) back to top loop
(bt) print backtrace
(return exp) return expression
1: nil
> )";
    CHECK(cout.str() == expected_out);
    CHECK(cerr.str() == expected_err);
    vm::primin(old);
  }

  vm::primerr(olderr);
  vm::primout(oldout);
}

} // namespace lisp
