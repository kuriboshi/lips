//
// Lips, lisp shell.
// Copyright 1988, 2020-2023 Krister Joas
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
#include <filesystem>
#include <string>
#include <optional>
#include <catch2/catch_test_macros.hpp>

#include <pwd.h>

#include <lisp/lisp.hh>
#include "main.hh"
#include "exec.hh"
#include "glob.hh"
#include "env.hh"

using namespace std::literals;
using namespace lisp;

namespace
{
///
/// @brief Checks if the original file should be a directory or not.
///
/// @details If the glob pattern ends with a slash or multiple stars then the
/// original file should be a directory. Regular files should not match such a
/// pattern.
///
/// @param wild The rest of the glob pattern if we've reached the end of the
/// string we are comparing or empty in which case we return true.
/// @param original The original filename we're matching. Used to check if it's
/// a directory or not.
///
/// @returns True if the glob ends with multiple stars or a slash and the
/// original file is a directory or if wild is empty.
///
bool dircheck(const std::string& wild, const std::string& original) // NOLINT: easily-swappable-parameters
{
  auto wbegin = wild.begin();
  if(*wbegin == '/')
    return std::filesystem::is_directory(original);
  while(wbegin != wild.end() && *wbegin == '*')
    ++wbegin;
  return wbegin == wild.end();
}

TEST_CASE("dircheck") { CHECK(dircheck("/", "/")); }

//
// Returns true if s matches wildcard pattern in w, false otherwise. STR is a
// simple string with no slashes.
//
// NOLINTNEXTLINE(readability-function-cognitive-complexity)
bool match(const std::string& str, const std::string& wild)
{
  auto sbegin = str.begin();
  auto wbegin = wild.begin();
  while(sbegin != str.end() && wbegin != wild.end())
  {
    switch(*wbegin)
    {
      case '*':
        ++wbegin;
        while(sbegin != str.end())
        {
          if(match(std::string(sbegin, str.end()), std::string(wbegin, wild.end())))
            return true;
          ++sbegin;
        }
        return sbegin == str.end() && dircheck(std::string(wbegin, wild.end()), str);
      case '?':
        break;
      case '[':
      {
        bool ok = false;
        while(wbegin != wild.end() && *wbegin != ']')
        {
          if(*wbegin == *sbegin)
            ok = true;
          ++wbegin;
        }
        if(!ok && wbegin != wild.end())
          return false;
        break;
      }
      case '\\':
        ++wbegin;
        /* fall through */
      default:
        if(*sbegin != *wbegin)
          return false;
        break;
    }
    ++sbegin;
    ++wbegin;
  }
  return sbegin == str.end() && dircheck(std::string(wbegin, wild.end()), str);
}

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
TEST_CASE("match")
{
  std::error_code ec;
  std::filesystem::create_directories("testdir", ec);
  REQUIRE(!ec);
  {
    const std::ofstream of("testfile");
  }

  SECTION("match: dircheck")
  {
    CHECK(match("testdir", "test*/"));
    CHECK(!match("testfile", "testf*/"));
    CHECK(match("testdir", "testd**"));
  }
  SECTION("pattern a*")
  {
    CHECK(match("alpha", "a*"));
    CHECK(!match("beta", "a*"));
    CHECK(match("aaa", "a*"));
  }
  SECTION("pattern *a*")
  {
    CHECK(match("xxxaxxx", "*a*"));
    CHECK(match("xxxa", "*a*"));
    CHECK(match("axxx", "*a*"));
  }
  SECTION("pattern *.cc")
  {
    CHECK(match("glob.cc", "*.cc"));
    CHECK(!match("glob.hh", "*.cc"));
  }
  SECTION("pattern *.??")
  {
    CHECK(match("foo.cc", "*.??"));
    CHECK(!match("foo.cpp", "*.??"));
  }
  SECTION("pattern [abc].??")
  {
    CHECK(match("a.cc", "[abc].??"));
    CHECK(match("b.cc", "[abc].??"));
    CHECK(match("c.hh", "[abc].??"));
    CHECK(!match("d.cc", "[abc].??"));
    CHECK(!match("b.cpp", "[abc].??"));
    CHECK(!match("b.c", "[abc].??"));
    CHECK(match("b...", "[abc].??"));
  }

  std::filesystem::remove("testfile", ec);
  std::filesystem::remove("testdir", ec);
  REQUIRE(!ec);
}

///
/// @brief Walks through files and returns an unsorted vector of files and
/// directories matching a glob pattern.
///
/// @param wild The glob pattern to match. Standard patterns are supported (*, ?, [...]).
///
// NOLINTNEXTLINE(readability-function-cognitive-complexity)
std::vector<std::string> walkfiles(const std::filesystem::path& wild)
{
  std::vector<std::filesystem::path> collect;
  if(wild.is_absolute())
    collect.emplace_back("/");
  else
    collect.emplace_back("");
  for(const auto& w: wild)
  {
    // The iterating over an absolute path starts with a forward slash. We skip this one
    if(w == "/")
      continue;
    auto process = std::move(collect);
    collect.clear();
    for(const auto& p: process)
    {
      auto dir_path = p;
      if(dir_path.empty())
        dir_path = ".";
      if(!w.empty() && *w.begin()->string().begin() == '.')
      {
        if(match(".", w))
          collect.push_back(p / ".");
        if(match("..", w))
          collect.push_back(p / "..");
      }

      std::error_code ec;
      auto is_dir = std::filesystem::is_directory(dir_path, ec);
      if(is_dir && !ec)
      {
        for(const auto& e: std::filesystem::directory_iterator(dir_path, ec))
        {
          if(match(e.path().filename().string(), w))
            collect.push_back(p / e.path().filename());
        }
      }
      else if(!ec)
      {
        collect.push_back(dir_path);
      }
    }
  }
  std::vector<std::string> result;
  for(const auto& d: collect)
    result.push_back(d.string());
  return result;
}

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
TEST_CASE("walkfiles")
{
  std::error_code ec;
  for(const auto* s: {"testdir/a", "testdir/bb", "testdir/ccc", "testdir/x/y"})
  {
    std::filesystem::create_directories(s, ec);
    REQUIRE(!ec);
  }

  SECTION("walkfiles: *")
  {
    auto result = walkfiles("*");
    CHECK(!result.empty());
  }
  SECTION("walkfiles: testdi*")
  {
    auto result = walkfiles("testdi*");
    REQUIRE(!result.empty());
    CHECK(result.size() == 1);
    CHECK(result[0] == "testdir"s);
  }
  SECTION("walkfiles: testdir/*")
  {
    auto result = walkfiles("testdir/*");
    REQUIRE(!result.empty());
    CHECK(result.size() == 4);
    for(const auto* r: {"testdir/a", "testdir/bb", "testdir/ccc", "testdir/x"})
      CHECK(std::find(result.begin(), result.end(), r) != result.end());
  }
  SECTION("walkfiles: testdir/*/*")
  {
    auto result = walkfiles("testdir/*/*");
    REQUIRE(!result.empty());
    CHECK(result.size() == 1);
    for(const auto* r: {"testdir/x/y"})
      CHECK(std::find(result.begin(), result.end(), r) != result.end());
  }
  SECTION("walkfiles: testdir/[b]*")
  {
    auto result = walkfiles("testdir/[b]*");
    REQUIRE(!result.empty());
    CHECK(result.size() == 1);
    for(const auto* r: {"testdir/bb"})
      CHECK(std::find(result.begin(), result.end(), r) != result.end());
  }
  SECTION("walkfiles: ./testd*")
  {
    auto result = walkfiles("./testd*");
    REQUIRE(!result.empty());
    CHECK(result.size() == 1);
    CHECK(*result.begin() == "./testdir");
  }

  for(const auto* s: {"testdir/a", "testdir/bb", "testdir/ccc", "testdir/x/y", "testdir/x", "testdir"})
  {
    std::filesystem::remove(s, ec);
    REQUIRE(!ec);
  }
}

lisp_t buildlist(const std::vector<std::string>& list)
{
  lisp_t l = nil;
  for(auto r: list)
    l = cons(mkstring(r), l);
  return l;
}
} // namespace

namespace glob
{
//
// Expands tilde character in first position to home directory or other users
// home directory.
//
std::optional<std::string> extilde(const std::string& w)
{
  if(w.empty() || w[0] != '~')
    return w;
  auto p = w.begin();
  ++p;
  std::string s;
  if(p == w.end() || *p == '/')
  {
    s = environment->home()->getstr();
    std::copy(p, w.end(), std::back_inserter(s));
  }
  else
  {
    auto first = std::find(p, w.end(), '/');
    if(first == w.end())
      std::copy(p, w.end(), std::back_inserter(s));
    else
      std::copy(p, first - 1, std::back_inserter(s));
    auto* pw = getpwnam(s.c_str());
    if(pw == nullptr)
    {
      return {};
    }
    s = pw->pw_dir;
    std::copy(first, w.end(), std::back_inserter(s));
  }
  return s;
}

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
TEST_CASE("extilde")
{
  std::string home = env::get("HOME");
  SECTION("~ == HOME")
  {
    auto dir = extilde("~");
    REQUIRE(dir);
    CHECK(home == *dir); // NOLINT(bugprone-unchecked-optional-access)
  }
  SECTION("~/ == HOME/")
  {
    auto dir = extilde("~/");
    REQUIRE(dir);
    home.push_back('/');
    CHECK(home == *dir); // NOLINT(bugprone-unchecked-optional-access)
  }
  SECTION("~/hello/ == HOME/")
  {
    auto dir = extilde("~/hello/");
    REQUIRE(dir);
    home += "/hello/";
    CHECK(home == *dir); // NOLINT(bugprone-unchecked-optional-access)
  }
  SECTION("~USER == HOME")
  {
    const std::string user = env::get("USER");
    auto tilde_user = "~" + user;
    auto dir = extilde(tilde_user);
    REQUIRE(dir);
    CHECK(home == *dir); // NOLINT(bugprone-unchecked-optional-access)
  }
  SECTION("~UNKNOWN != ")
  {
    const std::string unknown = "~foobar";
    auto dir = extilde(unknown);
    REQUIRE(!dir);
  }
}

//
// expandfiles - expand file in the current directory matching the glob pattern
//               in 'wild'.  If SORT is true the result list is sorted.
//
lisp_t expandfiles(const std::string& wild, bool sort)
{
  if(wild == "/"s)
    return cons(mkstring(wild), nil);
  auto files = walkfiles(wild);
  if(files.empty())
    return C_ERROR;
  const struct
  {
    bool operator()(const std::string& a, const std::string& b) { return b < a; }
  } reverse;
  if(!is_nil(environment->globsort()) || sort)
    std::sort(files.begin(), files.end(), reverse);
  return buildlist(files);
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
    auto result = expandfiles("testdir/*", true);
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
    auto result = expandfiles("testdir/??", true);
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
    auto e = expandfiles(s, true);
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

//
// Lisp function expand.  Expand all files matching wild in directory dir.
//
lisp_t expand(lisp_t wild)
{
  check(wild, object::type::String, object::type::Symbol);
  auto wstr = extilde(wild->getstr());
  if(!wstr)
    return nil;
  return expandfiles(*wstr, false);
}

} // namespace glob
