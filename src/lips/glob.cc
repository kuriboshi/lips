/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include <iostream>
#include <filesystem>
#include <string>
#include <optional>
#include <doctest/doctest.h>

#include <pwd.h>

#include <lisp/libisp.hh>
#include "main.hh"
#include "exec.hh"
#include "glob.hh"

using namespace std::literals;
using namespace lisp;

namespace glob
{
//
// If *wild is a slash then str must be a directory to match wild completely.
// Used by match.
//
// str is the rest of the file and has to be the empty string for the function
// to return true.  wild should end with a slash or one or more stars, in which
// case the original glob expression had at least two stars.  The final
// parameter, ss, is the complete original path and is used to verify that the
// path refers to a directory.
//
bool dircheck(const std::string& str, const std::string& wild, const std::string& ss)
{
  auto sbegin = str.begin();
  auto wbegin = wild.begin();
  if(*wbegin == '/')
  {
    if(sbegin != str.end())
      return false;
    return std::filesystem::is_directory(ss);
  }
  while(*wbegin == '*') ++wbegin;
  if(sbegin != str.end() || wbegin != wild.end())
    return false;
  return true;
}

TEST_CASE("glob::dircheck") { CHECK(glob::dircheck("", "/", "/")); }

//
// Returns true if s matches wildcard pattern in w, false otherwise. STR is a
// simple string with no slashes.
//
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
        return dircheck(std::string(sbegin, str.end()), std::string(wbegin, wild.end()), str);
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
  return dircheck(std::string(sbegin, str.end()), std::string(wbegin, wild.end()), str);
}

TEST_CASE("glob::match")
{
  SUBCASE("pattern a*")
  {
    CHECK(glob::match("alpha", "a*"));
    CHECK(!glob::match("beta", "a*"));
    CHECK(glob::match("aaa", "a*"));
  }
  SUBCASE("pattern *a*")
  {
    CHECK(glob::match("xxxaxxx", "*a*"));
    CHECK(glob::match("xxxa", "*a*"));
    CHECK(glob::match("axxx", "*a*"));
  }
  SUBCASE("pattern *.cc")
  {
    CHECK(glob::match("glob.cc", "*.cc"));
    CHECK(!glob::match("glob.hh", "*.cc"));
  }
  SUBCASE("pattern *.??")
  {
    CHECK(glob::match("foo.cc", "*.??"));
    CHECK(!glob::match("foo.cpp", "*.??"));
  }
  SUBCASE("pattern [abc].??")
  {
    CHECK(glob::match("a.cc", "[abc].??"));
    CHECK(glob::match("b.cc", "[abc].??"));
    CHECK(glob::match("c.hh", "[abc].??"));
    CHECK(!glob::match("d.cc", "[abc].??"));
    CHECK(!glob::match("b.cpp", "[abc].??"));
    CHECK(!glob::match("b.c", "[abc].??"));
    CHECK(glob::match("b...", "[abc].??"));
  }
}

//
// Inserts element WHAT in list WHERE keeping alphabetic order.
//
LISPT orderinsert(LISPT what, LISPT where)
{
  LISPT p1 = NIL;
  auto p2 = where;
  while(!is_NIL(p2))
  {
    if(what->getstr() < p2->car()->getstr())
    {
      if(!is_NIL(p1))
      {
        rplacd(p1, cons(what, NIL));
        rplacd(p1->cdr(), p2);
      }
      else
        where = cons(what, where);
      break;
    }
    p1 = p2;
    p2 = p2->cdr();
  }
  if(is_NIL(where))
    where = cons(what, NIL);
  else if(is_NIL(p2))
    rplacd(p1, cons(what, NIL));
  return where;
}

TEST_CASE("glob::orderinsert")
{
  auto list = cons(mkstring("a"), cons(mkstring("c"), NIL));
  SUBCASE("Insert in the middle")
  {
    auto b = mkstring("b");
    auto result = orderinsert(b, list);
    REQUIRE(type_of(result) == type::CONS);
    CHECK(result->cdr()->car()->getstr() == b->getstr());
    CHECK(equal(list, result));
  }
  SUBCASE("Insert at the end")
  {
    auto d = mkstring("d");
    auto result = orderinsert(d, list);
    REQUIRE(type_of(result) == type::CONS);
    CHECK(result->cdr()->cdr()->car()->getstr() == d->getstr());
    CHECK(equal(list, result));
  }
  SUBCASE("Insert at the beginning")
  {
    auto A = mkstring("A");
    auto result = orderinsert(A, list);
    REQUIRE(type_of(result) == type::CONS);
    CHECK(result->car()->getstr() == A->getstr());
    CHECK(equal(result->cdr(), list));
  }
}

//
// Expands tilde character in first position to home directory or other users
// home directory.
//
std::optional<std::string> extilde(const std::string& w, bool report)
{
  if(w.empty() || w[0] != '~')
    return w;
  auto p = w.begin();
  ++p;
  std::string s;
  if(p == w.end() || *p == '/')
  {
    s = env->home->getstr();
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
      if(report)
        error(NO_USER, mkstring(s));
      return {};
    }
    s = pw->pw_dir;
    std::copy(first, w.end(), std::back_inserter(s));
  }
  return s;
}

TEST_CASE("glob::extilde")
{
  std::string home = std::getenv("HOME");
  SUBCASE("~ == HOME")
  {
    auto dir = glob::extilde("~", false);
    REQUIRE(dir);
    CHECK(home == *dir);
  }
  SUBCASE("~/ == HOME/")
  {
    auto dir = glob::extilde("~/", false);
    REQUIRE(dir);
    home.push_back('/');
    CHECK(home == *dir);
  }
  SUBCASE("~/hello/ == HOME/")
  {
    auto dir = glob::extilde("~/hello/", false);
    REQUIRE(dir);
    home += "/hello/";
    CHECK(home == *dir);
  }
  SUBCASE("~USER == HOME")
  {
    std::string user = std::getenv("USER");
    auto tilde_user = "~" + user;
    auto dir = glob::extilde(tilde_user, false);
    REQUIRE(dir);
    CHECK(home == *dir);
  }
  SUBCASE("~UNKNOWN != ")
  {
    std::string unknown = "~foobar";
    auto dir = glob::extilde(unknown, false);
    REQUIRE(!dir);
  }
}

//
// walkfiles - walks through files as specified by WILD and builds an unsorted
//             array of character strings.  Returns true if any file matched
//             the pattern, false otherwise.
//
std::vector<std::string> walkfiles(const std::filesystem::path& root, const std::string& wild, bool all, bool report)
{
  std::vector<std::string> result;

  auto last = wild.find('/');
  auto w = [&]() {
    if(last == std::string::npos)
      return wild;
    return wild.substr(0, last);
  }();

  auto rest = [&]() {
    if(last == std::string::npos)
      return ""s;
    auto first = wild.find_first_not_of('/', last);
    return wild.substr(first);
  }();

  if(last == 0)
    return walkfiles("/", rest, all, report);

  auto walkentry = [&](const std::filesystem::path& path) -> void {
    if((all || path.filename().string()[0] != '.' || w[0] == '.') && match(path.filename().string(), w))
    {
      if(!rest.empty() && std::filesystem::is_directory(path))
      {
        auto subdir = root;
        if(subdir == "."s)
          subdir = path.filename();
        else
          subdir /= path.filename();
        for(auto& p: walkfiles(subdir, rest, all, report)) result.push_back(p);
      }
      else
      {
        if(root == "."s)
          result.push_back(path.filename());
        else
          result.push_back(root / path.filename());
      }
    }
  };

  // Work around the fact that directory_iterator skips "." and "..".
  walkentry(".");
  walkentry("..");
  for(auto& d: std::filesystem::directory_iterator(root)) walkentry(d.path());
  return result;
}

TEST_CASE("glob::walkfiles")
{
  std::error_code ec;
  for(auto s: {"testdir/a", "testdir/bb", "testdir/ccc", "testdir/x/y"})
  {
    std::filesystem::create_directories(s, ec);
    REQUIRE(!ec);
  }

  SUBCASE("test 1")
  {
    auto result = glob::walkfiles(".", "*", false, false);
    CHECK(!result.empty());
  }
  SUBCASE("test 2")
  {
    auto result = glob::walkfiles(".", "testdi*", false, false);
    REQUIRE(!result.empty());
    CHECK(result.size() == 1);
    CHECK(result[0] == "testdir"s);
  }
  SUBCASE("test 3")
  {
    auto result = glob::walkfiles(".", "testdir/*", false, false);
    REQUIRE(!result.empty());
    CHECK(result.size() == 4);
    for(auto r: {"testdir/a", "testdir/bb", "testdir/ccc", "testdir/x"})
      CHECK(std::find(result.begin(), result.end(), r) != result.end());
  }
  SUBCASE("test 4")
  {
    auto result = glob::walkfiles(".", "testdir/*/*", false, false);
    REQUIRE(!result.empty());
    CHECK(result.size() == 1);
    for(auto r: {"testdir/x/y"}) CHECK(std::find(result.begin(), result.end(), r) != result.end());
  }
  SUBCASE("test 5")
  {
    auto result = glob::walkfiles(".", "testdir/[b]*", false, false);
    REQUIRE(!result.empty());
    CHECK(result.size() == 1);
    for(auto r: {"testdir/bb"}) CHECK(std::find(result.begin(), result.end(), r) != result.end());
  }

  for(auto s: {"testdir/a", "testdir/bb", "testdir/ccc", "testdir/x/y", "testdir/x", "testdir"})
  {
    std::filesystem::remove(s, ec);
    REQUIRE(!ec);
  }
}

LISPT buildlist(const std::vector<std::string>& list)
{
  LISPT l = NIL;
  for(auto r: list) l = cons(mkstring(r), l);
  return l;
}

//
// expandfiles - expand file in the current directory matching the glob pattern
//               in 'wild'.  If ALL is true the list includes all files, even
//               hidden files.  If REPORT is true errors will throw an
//               exception.  If SORT is true the result list is sorted.
//
LISPT expandfiles(const std::string& wild, bool all, bool report, bool sort)
{
  if(wild == "/"s)
    return cons(mkstring(wild), NIL);
  auto files = glob::walkfiles(".", wild, all, report);
  if(files.empty())
    return C_ERROR;
  struct
  {
    bool operator()(const std::string& a, const std::string& b) { return b < a; }
  } reverse;
  if(!is_NIL(env->globsort) || sort)
    std::sort(files.begin(), files.end(), reverse);
  return glob::buildlist(files);
}

TEST_CASE("glob::expandfiles")
{
  std::error_code ec;
  std::vector<std::string> dirs{"testdir/a"s, "testdir/bb"s, "testdir/ccc"s};
  for(auto d: dirs)
  {
    std::filesystem::create_directories(d, ec);
    CHECK(!ec);
  }

  SUBCASE("Expand all files")
  {
    auto result = glob::expandfiles("testdir/*", false, false, true);
    CHECK(length(result)->intval() == 3);

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
  SUBCASE("Expand only one file")
  {
    auto result = glob::expandfiles("testdir/??", false, false, true);
    CHECK(length(result)->intval() == 1);

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

  SUBCASE("testdir/*")
  {
    LISPT wild = mkstring("testdir/*");
    auto e = ::expand(wild, NIL, NIL);
    CHECK(length(e)->intval() == 3);
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

  SUBCASE("test*/*")
  {
    LISPT wild = mkstring("testd*/*");
    auto e = ::expand(wild, NIL, NIL);
    CHECK(length(e)->intval() == 3);
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
LISPT expand(LISPT wild, LISPT rep, LISPT all)
{
  auto r = is_NIL(rep);
  check(wild, type::STRING, type::SYMBOL);
  auto wstr = glob::extilde(wild->getstr(), r);
  if(!wstr)
    return NIL;
  return glob::expandfiles(*wstr, !is_NIL(all), r, false);
}

} // namespace glob
