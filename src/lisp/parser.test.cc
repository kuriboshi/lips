//
// Lips, lisp shell.
//
// Copyright 2022 Krister Joas
//

#include <sstream>
#include <catch2/catch.hpp>
#include "parser.hh"

namespace
{
using StringReader = lisp::Reader<std::string>;

void lisp_compare(std::string input, const std::string& result)
{
  StringReader reader{input};
  auto res = lisp::Parser(reader).parse();
  std::ostringstream os;
  os << res;
  CHECK(os.str() == result);
}
}

namespace lisp
{

const std::string& pname(LISPT sym)
{
  return sym->symbol().pname.name;
}

TEST_CASE("Parser (a b . c)")
{
  std::string s{"(a b . c)"};
  StringReader reader{s};
  auto res = Parser(reader).parse();
  CHECK(pname(cdr(cdr(res))) == "c");
}

TEST_CASE("Parser (a . (b))")
{
  lisp_compare("(a . (b))", "(a b)");
}

LISPT nth(LISPT o, int n)
{
  auto o1 = o;
  while(n > 0)
  {
    REQUIRE(listp(o));
    o1 = cdr(o1);
    --n;
  }
  REQUIRE(listp(o));
  return car(o1);
}

//
// Dotted pair tests.
//
TEST_CASE("Parser (a b . c d)")
{
  std::string s{"(a b . c d)"};
  StringReader reader{s}; // -> (a b . c d)
  auto res = Parser(reader).parse();
  CHECK(pname(nth(res, 0)) == "a");
  CHECK(pname(nth(res, 1)) == "b");
  CHECK(pname(nth(res, 2)) == ".");
  CHECK(pname(nth(res, 3)) == "c");
  CHECK(pname(nth(res, 4)) == "d");
}

TEST_CASE("Parser (a b . (c d))")
{
  std::string s{"(a b . (c d))"};
  StringReader reader{s}; // -> (a b c d)
  auto res = Parser(reader).parse();
  CHECK(pname(nth(res, 2)) == "c");
  CHECK(pname(nth(res, 3)) == "d");
}

TEST_CASE("Parser (a b . (c d]")
{
  std::string s{"(a b . (c d]"};
  StringReader reader{s}; // -> (a b c d)
  auto res = Parser(reader).parse();
  CHECK(pname(nth(res, 2)) == "c");
  CHECK(pname(nth(res, 3)) == "d");
}

TEST_CASE("Parser (a . (b . c))")
{
  lisp_compare("(a . (b . c))", "(a b . c)");
}

//
// Regular objects and lists.
//
TEST_CASE("Parser string")
{
  lisp_compare("\"hello\"", "\"hello\"");
  lisp_compare("\"he\\\"llo\"", "\"he\\\"llo\"");
}

TEST_CASE("Parser normal")
{
  lisp_compare("a", "a");
  lisp_compare("(a b c)", "(a b c)");
  lisp_compare("((a b c))", "((a b c))");
  lisp_compare("(a b (c))", "(a b (c))");
  lisp_compare("(a b (c) d)", "(a b (c) d)");
}

//
// Super parenthesis tests.
///
TEST_CASE("Parser super")
{
  lisp_compare("(a b c]", "(a b c)");
  lisp_compare("[a b c]", "(a b c)");
  lisp_compare("((a b c]", "((a b c))");
  lisp_compare("(a b (c]", "(a b (c))");
  lisp_compare("(a (b (c]", "(a (b (c)))");
  lisp_compare("(a [b (c])", "(a (b (c)))");
  lisp_compare("(a [b (c] d)", "(a (b (c)) d)");
}

TEST_CASE("Parser [a b) c)]")
{
  lisp_compare("[a b) c)]", "(((a b) c))");
}

TEST_CASE("Parser [a b) c))]")
{
  lisp_compare("[a b) c))]", "((((a b) c)))");
}

TEST_CASE("Parser .")
{
  lisp_compare(".", ".");
}

//
// Missing top level parenthesis should parse as if they where there.
//
TEST_CASE("Parser top level single object")
{
  lisp_compare("", "");
  lisp_compare("a)", "a");
  lisp_compare("a]", "a");
}

TEST_CASE("Parser top level multiple objects")
{
  lisp_compare("a b)", "(a b)");
  lisp_compare("a b", "(a b)");
  lisp_compare("a b c]", "(a b c)");
  lisp_compare("a b c ()", "(a b c nil)");
}

//
// Numeric objects.
//
TEST_CASE("Parser numbers")
{
  lisp_compare("123", "123");
  lisp_compare("00123", "123");
  lisp_compare("(00000)", "(0)");
}

TEST_CASE("Parser parse")
{
  lisp_compare("()", "nil");
  lisp_compare("a", "a");
  lisp_compare("(a)", "(a)");
  lisp_compare("(a b)", "(a b)");
  lisp_compare("(a (b))", "(a (b))");
  lisp_compare("(a (b) (c) d)", "(a (b) (c) d)");
  lisp_compare("(a (b) c d)", "(a (b) c d)");
  lisp_compare("(a]", "(a)");
  lisp_compare("((a]", "((a))");
  lisp_compare("[(a]", "((a))");
  lisp_compare("[(a b]", "((a b))");
  lisp_compare("([(a b] c)", "(((a b)) c)");
}

TEST_CASE("Parser dot")
{
  lisp_compare("(list a (cadr x))", "(list a (cadr x))");
  lisp_compare("(lambda (a . x) (list a (cadr x)))", "(lambda (a . x) (list a (cadr x)))");
  lisp_compare("(setq f (lambda (a . x) (list a (cadr x))))", "(setq f (lambda (a . x) (list a (cadr x))))");
}

}
