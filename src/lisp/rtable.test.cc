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

#include <catch2/catch.hpp>

#include "alloc.hh"
#include "prim.hh"
#include "rtable.hh"

namespace lisp
{

TEST_CASE("rtable: rmdquote")
{
  lisp_t in = details::alloc::getobject(ref_file_t::create(R"(he\"llo")"));
  auto hello = rtable::rmdquote(context::current(), in);
  CHECK(type_of(hello) == type::String);
  CHECK(hello->string() == "he\"llo");
}

TEST_CASE("rtable: rmsquote")
{
  SECTION("quote next atom")
  {
    auto in = details::alloc::getobject(ref_file_t::create(R"(1)"));
    auto q = rtable::rmsquote(context::current(), in);
    CHECK(type_of(q) == type::Cons);
    CHECK(car(q) == C_QUOTE);
    CHECK(eq(cadr(q), 1_l) == T);
  }

  SECTION("quote before right parenthesis")
  {
    auto in = details::alloc::getobject(ref_file_t::create(R"())"));
    auto q = rtable::rmsquote(context::current(), in);
    CHECK(type_of(q) == type::Symbol);
    CHECK(q == "'"_a);
  }
}

TEST_CASE("rtable: rmgetenv")
{
  SECTION("existing environment variable")
  {
    lisp_t in = details::alloc::getobject(ref_file_t::create(R"(HOME)"));
    auto home = rtable::rmgetenv(context::current(), in);
    CHECK(type_of(home) == type::String);
  }

  SECTION("non-existing environment variable")
  {
    lisp_t in = details::alloc::getobject(ref_file_t::create(R"(DOES_NOT_EXIST)"));
    auto none = rtable::rmgetenv(context::current(), in);
    CHECK(none == nil);
  }
}

} // namespace lisp
