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

#include <sys/param.h>
#include <sys/stat.h>
#include <dirent.h>
#include <pwd.h>

#include <cstring>
#include <cstdlib>

#include <lisp/libisp.hh>
#include "main.hh"
#include "exec.hh"
#include "glob.hh"

extern lisp::lisp* L;

using namespace std::literals;
using namespace lisp;

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
static bool dircheck(const char* str, const char* wild, const std::string& ss)
{
  if(*wild == '/')
  {
    if(*str)
      return false;
    return std::filesystem::is_directory(ss);
  }
  while(*wild == '*') wild++;
  if(*str || *wild)
    return false;
  return true;
}

TEST_CASE("glob.cc: dircheck")
{
  REQUIRE(dircheck("", "/", "/"));
}

/*
 * Returns 1 if s matches wildcard pattern in w, 0 otherwise. Str
 * is a simple string with no slashes.
 */
static bool match(const char* str, const char* wild)
{
  const char* ss = str;

  while(*wild && *str)
  {
    switch(*wild)
    {
      case '*':
        wild++;
        while(*str)
          if(match(str, wild))
            return true;
          else
            str++;
        return dircheck(str, wild, ss);
      case '?':
        break;
      case '[':
      {
        bool ok = false;
        while(*wild && *wild != ']')
        {
          if(*wild == *str)
            ok = true;
          wild++;
        }
        if(!ok && *wild)
          return false;
        break;
      }
      case '\\':
        wild++;
        /* fall through */
      default:
        if(*str != *wild)
          return false;
        break;
    }
    str++;
    wild++;
  }
  return dircheck(str, wild, ss);
}

TEST_CASE("glob.cc: match")
{
  SUBCASE("pattern a*")
  {
    REQUIRE(match("alpha", "a*"));
    REQUIRE(!match("beta", "a*"));
    REQUIRE(match("aaa", "a*"));
  }
  SUBCASE("pattern *a*")
  {
    REQUIRE(match("xxxaxxx", "*a*"));
    REQUIRE(match("xxxa", "*a*"));
    REQUIRE(match("axxx", "*a*"));
  }
  SUBCASE("pattern *.cc")
  {
    REQUIRE(match("glob.cc", "*.cc"));
    REQUIRE(!match("glob.hh", "*.cc"));
  }
  SUBCASE("pattern *.??")
  {
    REQUIRE(match("foo.cc", "*.??"));
    REQUIRE(!match("foo.cpp", "*.??"));
  }
  SUBCASE("pattern [abc].??")
  {
    REQUIRE(match("a.cc", "[abc].??"));
    REQUIRE(match("b.cc", "[abc].??"));
    REQUIRE(match("c.hh", "[abc].??"));
    REQUIRE(!match("d.cc", "[abc].??"));
    REQUIRE(!match("b.cpp", "[abc].??"));
    REQUIRE(!match("b.c", "[abc].??"));
    REQUIRE(match("b...", "[abc].??"));
  }
}

/*
 * Inserts element what in list where keeping alphabetic order.
 */
static LISPT orderinsert(LISPT what, LISPT where)
{
  LISPT p1 = C_NIL;
  auto p2 = where;
  while(!is_NIL(p2))
  {
    if(strcmp(p2->car()->getstr().c_str(), what->getstr().c_str()) > 0)
    {
      if(!is_NIL(p1))
      {
        rplacd(p1, cons(what, C_NIL));
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
    where = cons(what, C_NIL);
  else if(is_NIL(p2))
    rplacd(p1, cons(what, C_NIL));
  return where;
}

TEST_CASE("glob.cc: orderinsert")
{
  auto list = cons(mkstring("a"), cons(mkstring("c"), C_NIL));
  SUBCASE("Insert in the middle")
  {
    auto b = mkstring("b");
    auto result = orderinsert(b, list);
    REQUIRE(type_of(result) == CONS);
    REQUIRE(result->cdr()->car()->getstr() == b->getstr());
    REQUIRE(equal(list, result));
  }
  SUBCASE("Insert at the end")
  {
    auto d = mkstring("d");
    auto result = orderinsert(d, list);
    REQUIRE(type_of(result) == CONS);
    REQUIRE(result->cdr()->cdr()->car()->getstr() == d->getstr());
    REQUIRE(equal(list, result));
  }
  SUBCASE("Insert at the beginning")
  {
    auto A = mkstring("A");
    auto result = orderinsert(A, list);
    REQUIRE(type_of(result) == CONS);
    REQUIRE(result->car()->getstr() == A->getstr());
    REQUIRE(equal(result->cdr(), list));
  }
}

/*
 * Expands tilde character in first position to home directory or
 * other users home directory.
 */
const std::optional<std::string> extilde(const std::string& w, bool report)
{
  if(w.empty() || w[0] != '~')
    return w;
  auto p = w.begin();
  ++p;
  std::string s;
  if(p == w.end() || *p == '/')
  {
    s = home->getstr();
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
        L->error(NO_USER, mkstring(s));
      return {};
    }
    s = pw->pw_dir;
    std::copy(first, w.end(), std::back_inserter(s));
  }
  return s;
}

TEST_CASE("glob.cc: extilde")
{
  std::string home = std::getenv("HOME");
  SUBCASE("~ == HOME")
  {
    auto dir = extilde("~", false);
    REQUIRE(dir);
    CHECK(home == *dir);
  }
  SUBCASE("~/ == HOME/")
  {
    auto dir = extilde("~/", false);
    REQUIRE(dir);
    home.push_back('/');
    CHECK(home == *dir);
  }
  SUBCASE("~/hello/ == HOME/")
  {
    auto dir = extilde("~/hello/", false);
    REQUIRE(dir);
    home += "/hello/";
    CHECK(home == *dir);
  }
  SUBCASE("~USER == HOME")
  {
    std::string user = std::getenv("USER");
    auto tilde_user = "~" + user;
    auto dir = extilde(tilde_user, false);
    REQUIRE(dir);
    CHECK(home == *dir);
  }
  SUBCASE("~UNKNOWN != ")
  {
    std::string unknown = "~foobar";
    auto dir = extilde(unknown, false);
    REQUIRE(!dir);
  }
}

/* 
 * walkfiles - walks through files as specified by WILD and builds an 
 *             unsorted array of character strings. Returns true if
 *             any file matched the pattern, false otherwise.
 */
static std::vector<std::string> walkfiles(
  const std::filesystem::path& root, const std::string& wild, bool all, bool report)
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
  for(auto& d: std::filesystem::directory_iterator(root))
  {
    if((all || d.path().filename().string()[0] != '.' || w[0] == '.')
      && match(d.path().filename().string().c_str(), w.c_str()))
    {
      if(!rest.empty() && std::filesystem::is_directory(d.path()))
      {
        auto subdir = root;
        if(subdir == "."s)
          subdir = d.path().filename();
        else
          subdir /= d.path().filename();
        for(auto& p: walkfiles(subdir, rest, all, report))
          result.push_back(p);
      }
      else
      {
        if(root == "."s)
          result.push_back(d.path().filename());
        else
          result.push_back(root / d.path().filename());
      }
    }
  }
  return result;
}

TEST_CASE("glob.cc: walkfiles")
{
  std::error_code ec;
  for(auto s: {"testdir/a", "testdir/bb", "testdir/ccc", "testdir/x/y"})
  {
    std::filesystem::create_directories(s, ec);
    CHECK(!ec);
  }

  SUBCASE("test 1")
  {
    auto result = walkfiles(".", "*", false, false);
    REQUIRE(!result.empty());
  }
  SUBCASE("test 2")
  {
    auto result = walkfiles(".", "testdi*", false, false);
    REQUIRE(!result.empty());
    CHECK(result.size() == 1);
    CHECK(result[0] == "testdir"s);
  }
  SUBCASE("test 3")
  {
    auto result = walkfiles(".", "testdir/*", false, false);
    REQUIRE(!result.empty());
    CHECK(result.size() == 4);
    for(auto r: {"testdir/a", "testdir/bb", "testdir/ccc", "testdir/x"})
      CHECK(std::find(result.begin(), result.end(), r) != result.end());
  }
  SUBCASE("test 4")
  {
    auto result = walkfiles(".", "testdir/*/*", false, false);
    REQUIRE(!result.empty());
    CHECK(result.size() == 1);
    for(auto r: {"testdir/x/y"})
      CHECK(std::find(result.begin(), result.end(), r) != result.end());
  }
  SUBCASE("test 5")
  {
    auto result = walkfiles(".", "testdir/[b]*", false, false);
    REQUIRE(!result.empty());
    CHECK(result.size() == 1);
    for(auto r: {"testdir/bb"})
      CHECK(std::find(result.begin(), result.end(), r) != result.end());
  }

  for(auto s: {"testdir/a", "testdir/bb", "testdir/ccc", "testdir/x/y", "testdir/x", "testdir"})
  {
    std::filesystem::remove(s, ec);
    CHECK(!ec);
  }
}

static LISPT buildlist(const std::vector<std::string>& list)
{
  LISPT l = C_NIL;
  for(auto r: list)
    l = cons(mkstring(r), l);
  return l;
}

LISPT expandfiles(const char* wild, bool all, bool report, bool sort)
{
  if(*wild == '/' && *(wild + 1) == '\0')
    return cons(mkstring(*L, wild), C_NIL);
  auto files = walkfiles(".", wild, all, report);
  if(files.empty())
    return C_ERROR;
  struct {
    bool operator()(const std::string& a, const std::string& b) { return b < a; }
  } reverse;
  if(!is_NIL(globsort) || sort)
    std::sort(files.begin(), files.end(), reverse);
  return buildlist(files);
}

/*
 * Lisp function expand. Expand all files matching wild
 * in directory dir.
 */
PRIMITIVE expand(::lisp::lisp& l, LISPT wild, LISPT rep, LISPT all)
{
  bool r = is_NIL(rep);
  l.check(wild, STRING, SYMBOL);
  auto wstr = extilde(wild->getstr(), r);
  if(!wstr)
    return C_NIL;
  return expandfiles(wstr->c_str(), !is_NIL(all), r, false);
}

TEST_CASE("glob.cc: expandfiles")
{
  std::error_code ec;
  std::filesystem::create_directories("testdir/a", ec);
  CHECK(!ec);
  std::filesystem::create_directories("testdir/bb", ec);
  CHECK(!ec);
  std::filesystem::create_directories("testdir/ccc", ec);
  CHECK(!ec);
  
  SUBCASE("Expand all files")
  {
    auto result = expandfiles("testdir/*", false, false, true);
    CHECK(length(result)->intval() == 3);

    int count = 3;
    for(auto i: {"testdir/a"s, "testdir/bb"s, "testdir/ccc"s})
    {
      for(auto a: result)
      {
        if(a->getstr() == i)
        {
          CHECK(a->getstr() == i);
          --count;
          break;
        }
      }
    }
    CHECK(count == 0);
  }
  SUBCASE("Expand only one file")
  {
    auto result = expandfiles("testdir/??", false, false, true);
    CHECK(length(result)->intval() == 1);

    int count = 1;
    for(auto i: {"testdir/a"s, "testdir/bb"s, "testdir/ccc"s})
    {
      for(auto a: result)
      {
        if(a->getstr() == i)
        {
          CHECK(a->getstr() == i);
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
    auto e = expand(*L, wild, 0, 0);
    CHECK(length(e)->intval() == 3);
    std::vector<std::string> x = {"testdir/a"s, "testdir/bb"s, "testdir/ccc"s};
    for(auto i: e)
      for(auto j: x)
      {
        if(i->getstr() == j)
        {
          CHECK(i->getstr() == j);
          break;
        }
      }
  }

  SUBCASE("test*/*")
  {
    LISPT wild = mkstring("testd*/*");
    auto e = expand(*L, wild, 0, 0);
    CHECK(length(e)->intval() == 3);
    std::vector<std::string> x = {"testdir/a"s, "testdir/bb"s, "testdir/ccc"s};
    for(auto i: e)
      for(auto j: x)
      {
        if(i->getstr() == j)
        {
          CHECK(i->getstr() == j);
          break;
        }
      }
  }

  std::filesystem::remove("testdir/a", ec);
  CHECK(!ec);
  std::filesystem::remove("testdir/bb", ec);
  CHECK(!ec);
  std::filesystem::remove("testdir/ccc", ec);
  CHECK(!ec);
  std::filesystem::remove("testdir", ec);
  CHECK(!ec);
}

LISPT glob(LISPT wild) { return expand(*L, wild, 0, 0); }
