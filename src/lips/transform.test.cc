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

#include <catch2/catch_test_macros.hpp>

#include <lisp/lisp.hh>

#include "transform.hh"

using namespace lisp;

TEST_CASE("transform")
{
  SECTION("pipe |")
  {
    auto result = transform("(ls | wc -l)"_l);
    CHECK(equal(result, "(pipe-cmd (ls) (wc -l))"_l));
  }

  SECTION("redirect >")
  {
    auto result = transform("(ls > foo)"_l);
    CHECK(equal(result, "(redir-to (ls) foo)"_l));
  }

  SECTION("redirect >")
  {
    auto result = transform("(ls > foo 1)"_l);
    CHECK(equal(result, "(redir-to (ls) foo 1)"_l));
  }

  SECTION("redirect >>")
  {
    auto result = transform("(ls >> foo)"_l);
    CHECK(equal(result, "(append-to (ls) foo)"_l));
  }

  SECTION("redirect <")
  {
    auto result = transform("(ls < foo)"_l);
    CHECK(equal(result, "(redir-from (ls) foo)"_l));
  }

  SECTION("pipe | redirect >")
  {
    auto result = transform("(ls | wc > foo)"_l);
    CHECK(equal(result, "(redir-to (pipe-cmd (ls) (wc)) foo)"_l));
  }

  SECTION("pipe | | redirect >")
  {
    auto result = transform(R"((a | b | c))"_l);
    CHECK(equal(result, R"((pipe-cmd (pipe-cmd (a) (b)) (c)))"_l));
  }

  SECTION("background &")
  {
    auto result = transform("(ls &)"_l);
    CHECK(equal(result, "(back (ls))"_l));
  }
}
