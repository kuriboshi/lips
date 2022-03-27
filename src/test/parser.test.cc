//
// Lips, lisp shell.
//
// Copyright 2022 Krister Joas
//

#include <sstream>
#include <catch2/catch.hpp>
#include <lisp/parser.hh>

namespace
{
using string_reader = lisp::reader<std::string>;

void lisp_compare(std::string input, const std::string& result)
{
  string_reader reader{input};
  auto res = lisp::parser(reader).parse();
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

TEST_CASE("parser (a b . c)")
{
  std::string s{"(a b . c)"};
  string_reader reader{s};
  auto res = parser(reader).parse();
  CHECK(pname(cdr(cdr(res))) == "c");
}

TEST_CASE("parser (a . (b))")
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
TEST_CASE("parser (a b . c d)")
{
  std::string s{"(a b . c d)"};
  string_reader reader{s}; // -> (a b . c d)
  auto res = parser(reader).parse();
  CHECK(pname(nth(res, 0)) == "a");
  CHECK(pname(nth(res, 1)) == "b");
  CHECK(pname(nth(res, 2)) == ".");
  CHECK(pname(nth(res, 3)) == "c");
  CHECK(pname(nth(res, 4)) == "d");
}

TEST_CASE("parser (a b . (c d))")
{
  std::string s{"(a b . (c d))"};
  string_reader reader{s}; // -> (a b c d)
  auto res = parser(reader).parse();
  CHECK(pname(nth(res, 2)) == "c");
  CHECK(pname(nth(res, 3)) == "d");
}

TEST_CASE("parser (a b . (c d]")
{
  std::string s{"(a b . (c d]"};
  string_reader reader{s}; // -> (a b c d)
  auto res = parser(reader).parse();
  CHECK(pname(nth(res, 2)) == "c");
  CHECK(pname(nth(res, 3)) == "d");
}

TEST_CASE("parser (a . (b . c))")
{
  lisp_compare("(a . (b . c))", "(a b . c)");
}

//
// Regular objects and lists.
//
TEST_CASE("parser string")
{
  lisp_compare("\"hello\"", "\"hello\"");
  lisp_compare("\"he\\\"llo\"", "\"he\\\"llo\"");
}

TEST_CASE("parser normal")
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
TEST_CASE("parser super")
{
  lisp_compare("(a b c]", "(a b c)");
  lisp_compare("[a b c]", "(a b c)");
  lisp_compare("((a b c]", "((a b c))");
  lisp_compare("(a b (c]", "(a b (c))");
  lisp_compare("(a (b (c]", "(a (b (c)))");
  lisp_compare("(a [b (c])", "(a (b (c)))");
  lisp_compare("(a [b (c] d)", "(a (b (c)) d)");
}

TEST_CASE("parser [a b) c)]")
{
  lisp_compare("[a b) c)]", "(((a b) c))");
}

TEST_CASE("parser [a b) c))]")
{
  lisp_compare("[a b) c))]", "((((a b) c)))");
}

TEST_CASE("parser .")
{
  lisp_compare(".", ".");
}

TEST_CASE("parser end parenthesis")
{
  lisp_compare(")", "nil");
  lisp_compare("]", "nil");
  lisp_compare("a)", "a");
}

//
// Numeric objects.
//
TEST_CASE("parser numbers")
{
  lisp_compare("123", "123");
  lisp_compare("00123", "123");
  lisp_compare("(00000)", "(0)");
}

TEST_CASE("parser parse")
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

TEST_CASE("parser dot")
{
  lisp_compare("(list a (cadr x))", "(list a (cadr x))");
  lisp_compare("(lambda (a . x) (list a (cadr x)))", "(lambda (a . x) (list a (cadr x)))");
  lisp_compare("(setq f (lambda (a . x) (list a (cadr x))))", "(setq f (lambda (a . x) (list a (cadr x))))");
}

TEST_CASE("parser: (greaterp 1.0 \"b\")")
{
  std::string s{"(greaterp 1.0 \"b\")"};
  string_reader reader{s};
  auto res = parser(reader).parse();
  REQUIRE(type_of(res) == type::CONS);
  CHECK(car(res) == "greaterp"_a);
  CHECK(cadr(res)->floatval() == 1.0);
  CHECK(caddr(res)->stringval() == "b");
}

}
