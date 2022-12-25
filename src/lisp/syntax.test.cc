//
// Lips, lisp shell.
// Copyright 2022 Krister Joas
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

#include "alloc.hh"
#include "syntax.hh"

#include <catch2/catch.hpp>

namespace lisp
{

TEST_CASE("syntax: macro")
{
  syntax stx;
  auto f0 = ref_file_t::create(R"(hello)");

  SECTION("Read macro")
  {
    auto f = "(lambda (f) (read f))"_l;
    stx.set('^', f);
    auto r = stx.macro(context::current(), f0, '^');
    REQUIRE(r != nil);
    REQUIRE(type_of(r) == type::Symbol);
    REQUIRE(r->symbol().pname == "hello");
  }

  SECTION("Read macro is nil")
  {
    stx.set('*', nil);
    auto r = stx.macro(context::current(), f0, '*');
    CHECK(r == nil);
  }
}

} // namespace lisp
