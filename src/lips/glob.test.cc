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

#include "glob.hh"
#include "env.hh"

using namespace lisp;
using namespace std::literals;

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
TEST_CASE("extilde")
{
  std::string home = env::get("HOME");
  SECTION("~ == HOME")
  {
    auto dir = glob::extilde("~");
    REQUIRE(dir);
    CHECK(home == *dir); // NOLINT(bugprone-unchecked-optional-access)
  }
  SECTION("~/ == HOME/")
  {
    auto dir = glob::extilde("~/");
    REQUIRE(dir);
    home.push_back('/');
    CHECK(home == *dir); // NOLINT(bugprone-unchecked-optional-access)
  }
  SECTION("~/hello/ == HOME/")
  {
    auto dir = glob::extilde("~/hello/");
    REQUIRE(dir);
    home += "/hello/";
    CHECK(home == *dir); // NOLINT(bugprone-unchecked-optional-access)
  }
  SECTION("~USER == HOME")
  {
    const std::string user = env::get("USER");
    auto tilde_user = "~" + user;
    auto dir = glob::extilde(tilde_user);
    REQUIRE(dir);
    CHECK(home == *dir); // NOLINT(bugprone-unchecked-optional-access)
  }
  SECTION("~UNKNOWN != ")
  {
    const std::string unknown = "~foobar";
    auto dir = glob::extilde(unknown);
    REQUIRE(!dir);
  }
}

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
TEST_CASE("expandfiles")
{
  std::error_code ec;
  const std::vector<std::string> dirs{"testdir/a"s, "testdir/bb"s, "testdir/ccc"s};
  for(auto d: dirs)
  {
    std::filesystem::create_directories(d, ec);
    REQUIRE(!ec);
  }

  SECTION("Expand all files")
  {
    auto result = glob::expandfiles("testdir/*", true);
    CHECK(length(result)->as_integer() == 3);

    int count = 3;
    for(auto d: dirs)
    {
      for(auto a: result)
      {
        if(a->getstr() == d)
        {
          CHECK(a->getstr() == d);
          --count;
          break;
        }
      }
    }
    CHECK(count == 0);
  }

  SECTION("Expand only one file")
  {
    auto result = glob::expandfiles("testdir/??", true);
    CHECK(length(result)->as_integer() == 1);

    int count = 1;
    for(auto d: dirs)
    {
      for(auto a: result)
      {
        if(a->getstr() == d)
        {
          CHECK(a->getstr() == d);
          --count;
          break;
        }
      }
    }
    CHECK(count == 0);
  }

  SECTION("testdir/*")
  {
    const lisp_t wild = mkstring("testdir/*");
    auto e = expand(wild);
    CHECK(length(e)->as_integer() == 3);
    for(auto i: e)
      for(auto d: dirs)
      {
        if(i->getstr() == d)
        {
          CHECK(i->getstr() == d);
          break;
        }
      }
  }

  SECTION("testd*/*")
  {
    const lisp_t wild = mkstring("testd*/*");
    auto e = expand(wild);
    CHECK(length(e)->as_integer() == 3);
    for(auto i: e)
      for(auto d: dirs)
      {
        if(i->getstr() == d)
        {
          CHECK(i->getstr() == d);
          break;
        }
      }
  }

  SECTION("./testd*")
  {
    const std::string s{"./testd*"};
    auto e = glob::expandfiles(s, true);
    REQUIRE(length(e)->as_integer() >= 1);
    CHECK(e->car()->getstr() == "./testdir");
  }

  for(auto d: dirs)
  {
    std::filesystem::remove(d, ec);
    CHECK(!ec);
  }
  std::filesystem::remove("testdir", ec);
  CHECK(!ec);
}
