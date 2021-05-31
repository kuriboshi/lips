/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include <iostream>
#include <filesystem>
#include <string>
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

inline constexpr int TICKS = 64;
inline constexpr int NAMELEN = 1024;

static char r[MAXPATHLEN];
static char** globarr;
static char** globp;
static char** globlimit;

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
static bool dircheck(const char* str, const char* wild, const char* ss)
{
  struct stat sbuf;
  int pos;

  if(*wild == '/')
  {
    if(*str)
      return false;
    pos = strlen(r);
    if(pos != 0)
      strcat(r, "/");
    strcat(r, ss);
    stat(r, &sbuf);
    r[pos] = '\0';
    if(sbuf.st_mode & S_IFDIR)
      return true;
    return false;
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
  int ok;
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
        ok = 0;
        while(*wild && *wild != ']')
        {
          if(*wild == *str)
            ok = 1;
          wild++;
        }
        if(!ok && *wild)
          return false;
        break;
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
const char* extilde(const char* w, int rep)
{
  struct passwd* pw;
  static char s[NAMELEN];

  if(*w != '~')
    return w;
  w++;
  if(*w == '/' || !*w)
    strcpy(s, home->getstr().c_str());
  else
  {
    if(index(w, '/') == nullptr)
    {
      pw = getpwnam(w);
      strcpy(s, w);
      w = "";
    }
    else
    {
      int i;

      for(i = 0; *w != '/'; i++) s[i] = *w++;
      s[i] = '\0';
      pw = getpwnam(s);
    }
    if(pw == nullptr)
    {
      if(rep)
        L->error(NO_USER, mkstring(*L, s));
      return nullptr;
    }
    strncpy(s, pw->pw_dir, MAXNAMLEN);
  }
  strcat(s, w);
  return s;
}

TEST_CASE("glob.cc: extilde")
{
  std::string home = std::getenv("HOME");
  SUBCASE("~ == HOME")
  {
    std::string dir = extilde("~", false);
    REQUIRE(home == dir);
  }
  SUBCASE("~USER == HOME")
  {
    std::string user = std::getenv("USER");
    auto tilde_user = "~" + user;
    std::string dir = extilde(tilde_user.c_str(), false);
    REQUIRE(home == dir);
  }
  SUBCASE("~UNKNOWN != ")
  {
    std::string unknown = "~foobar";
    REQUIRE(extilde(unknown.c_str(), false) == nullptr);
  }
}

/* 
 * walkfiles - walks through files as specified by WILD and builds an 
 *             unsorted array of character strings. Returns true if
 *             any file matched the pattern, false otherwise.
 */
static bool walkfiles(const char* wild, int all, int report)
{
  bool result;
  int pos;
  struct dirent* rdir;
  DIR* odir;
  const char* sw;
  const char* w;

  if(*wild == '/')
    w = wild + 1;
  else
    w = wild;
  if((odir = opendir(*r == '\0' ? "." : r)) == nullptr)
  {
    if(report)
      L->error(NO_DIRECTORY, mkstring(*L, r));
    return false;
  }
  while((rdir = readdir(odir)) != nullptr)
  {
    if((all || rdir->d_name[0] != '.' || *w == '.') && match(rdir->d_name, w))
    {
      result = true;
      pos = strlen(r);
      if(pos != 0 && r[pos - 1] != '/')
        strcat(r, "/");
      strcat(r, rdir->d_name);
      for(sw = w; *sw && *sw != '/'; sw++)
        ;
      if(*sw && *(++sw))
        result = walkfiles(sw, all, 0);
      else
      {
        *globp = strsave(r);
        if(globp == globlimit)
        {
          int foo;

          foo = globp - globarr;
          globarr = (char**)realloc(globarr, (foo + TICKS) * sizeof(char*));
          globlimit = globarr + foo + TICKS;
          globp = globarr + foo;
        }
        globp++;
      }
      r[pos] = '\0';
    }
  }
  closedir(odir);
  if(!result && report)
    L->error(NO_MATCH, mkstring(*L, wild));
  return result;
}

TEST_CASE("glob.cc: walkfiles")
{
  globarr = (char**)malloc(TICKS * sizeof(char*));
  globp = globarr;
  globlimit = globarr + TICKS;
  auto result = walkfiles("*", false, false);
  REQUIRE(result);
#if 0
  for(auto r = globarr; r < globp; r++)
    std::cout << *r << std::endl;
#endif
}

static LISPT buildlist()
{
  LISPT l = C_NIL;
  for(auto r = globarr; r < globp; r++)
  {
    l = cons(*L, mkstring(*L, *r), l);
    free(*r);
  }
  free(globarr);
  return l;
}

static int comp(const void* a, const void* b)
{
  /* Reverse sort. */
  return -strcmp(*(char**)a, *(char**)b);
}

LISPT expandfiles(const char* wild, int all, int report, int sort)
{
  if(*wild == '/' && *(wild + 1) == '\0')
    return cons(*L, mkstring(*L, wild), C_NIL);
  if(*wild == '/')
    strcpy(r, "/");
  else
    strcpy(r, "");
  globarr = (char**)malloc(TICKS * sizeof(char*));
  globp = globarr;
  globlimit = globarr + TICKS;
  if(!walkfiles(wild, all, report))
    return C_ERROR;
  if(!is_NIL(globsort) || sort)
    qsort((char*)globarr, globp - globarr, sizeof(char*), comp);
  return buildlist();
}

TEST_CASE("glob.cc: expandfiles")
{
  std::error_code ec;
  std::filesystem::create_directories("testdir/a", ec);
  REQUIRE(!ec);
  std::filesystem::create_directories("testdir/bb", ec);
  REQUIRE(!ec);
  std::filesystem::create_directories("testdir/ccc", ec);
  REQUIRE(!ec);
  
  SUBCASE("Expand all files")
  {
    auto result = expandfiles("testdir/*", false, false, true);
    REQUIRE(length(result)->intval() == 3);

    int count = 3;
    for(auto i: {"testdir/a"s, "testdir/bb"s, "testdir/ccc"s})
    {
      for(auto a = result; !is_NIL(a); a = a->cdr())
      {
        if(a->car()->getstr() == i)
          --count;
      }
    }
    REQUIRE(count == 0);
  }
  SUBCASE("Expand only one file")
  {
    auto result = expandfiles("testdir/??", false, false, true);
    REQUIRE(length(result)->intval() == 1);

    int count = 1;
    for(auto i: {"testdir/a"s, "testdir/bb"s, "testdir/ccc"s})
    {
      for(auto a = result; !is_NIL(a); a = a->cdr())
      {
        if(a->car()->getstr() == i)
          --count;
      }
    }
    REQUIRE(count == 0);
  }

  std::filesystem::remove("testdir/a", ec);
  REQUIRE(!ec);
  std::filesystem::remove("testdir/bb", ec);
  REQUIRE(!ec);
  std::filesystem::remove("testdir/ccc", ec);
  REQUIRE(!ec);
  std::filesystem::remove("testdir", ec);
  REQUIRE(!ec);
}

/*
 * Lisp function expand. Expand all files matching wild
 * in directory dir.
 */
PRIMITIVE expand(::lisp::lisp& l, LISPT wild, LISPT rep, LISPT all)
{
  const char* wstr;
  int r = 0;

  if(is_NIL(rep))
    r = 1;
  l.check(wild, STRING, SYMBOL);
  wstr = extilde(wild->getstr().c_str(), r);
  if(wstr == nullptr)
    return C_NIL;
  return expandfiles(wstr, is_NIL(all) ? 0 : 1, r, 0);
}

LISPT glob(LISPT wild) { return expand(*L, wild, 0, 0); }
