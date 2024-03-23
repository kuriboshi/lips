//
// Lips, lisp shell.
// Copyright 2022-2024 Krister Joas
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

#include <sstream>
#include <string>

#include <catch2/catch_test_macros.hpp>

#include "atoms.hh"
#include "file.hh"
#include "list.hh"
#include "parser.hh"
#include "predicate.hh"
#include "rtable.hh"

namespace
{
void lisp_compare(std::string input, const std::string& result)
{
  lisp::lexer lexer{input};
  auto res = lisp::parser(lexer).parse();
  std::ostringstream os;
  os << res;
  CHECK(os.str() == result);
}
} // namespace

namespace lisp
{

const std::string& pname(lisp_t sym) { return sym->as_symbol()->pname; }

TEST_CASE("parser: (a b . c)")
{
  lexer lexer{"(a b . c)"};
  auto res = parser(lexer).parse();
  CHECK(pname(cdr(cdr(res))) == "c");
}

TEST_CASE("parser: (a . (b))") { lisp_compare("(a . (b))", "(a b)"); }

lisp_t nth(lisp_t o, int n)
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
TEST_CASE("parser: (a b . c d)")
{
  lexer lexer{"(a b . c d)"}; // -> (a b . c d)
  auto res = parser(lexer).parse();
  CHECK(pname(nth(res, 0)) == "a");
  CHECK(pname(nth(res, 1)) == "b");
  CHECK(pname(nth(res, 2)) == ".");
  CHECK(pname(nth(res, 3)) == "c");
  CHECK(pname(nth(res, 4)) == "d");
}

TEST_CASE("parser: (a b . (c d))")
{
  lexer lexer{"(a b . (c d))"}; // -> (a b c d)
  auto res = parser(lexer).parse();
  CHECK(pname(nth(res, 2)) == "c");
  CHECK(pname(nth(res, 3)) == "d");
}

TEST_CASE("parser: (a b . (c d]")
{
  lexer lexer{"(a b . (c d]"}; // -> (a b c d)
  auto res = parser(lexer).parse();
  CHECK(pname(nth(res, 2)) == "c");
  CHECK(pname(nth(res, 3)) == "d");
}

TEST_CASE("parser: (a . (b . c))") { lisp_compare("(a . (b . c))", "(a b . c)"); }

//
// Regular objects and lists.
//
TEST_CASE("parser: string")
{
  lisp_compare("\"hello\"", "\"hello\"");
  lisp_compare("\"he\\\"llo\"", "\"he\\\"llo\"");
}

TEST_CASE("parser: normal")
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
TEST_CASE("parser: super")
{
  lisp_compare("(a b c]", "(a b c)");
  lisp_compare("[a b c]", "(a b c)");
  lisp_compare("((a b c]", "((a b c))");
  lisp_compare("(a b (c]", "(a b (c))");
  lisp_compare("(a (b (c]", "(a (b (c)))");
  lisp_compare("(a [b (c])", "(a (b (c)))");
  lisp_compare("(a [b (c] d)", "(a (b (c)) d)");
}

TEST_CASE("parser: [a b) c)]") { lisp_compare("[a b) c)]", "(((a b) c))"); }

TEST_CASE("parser: [a b) c))]") { lisp_compare("[a b) c))]", "((((a b) c)))"); }

TEST_CASE("parser: .") { lisp_compare(".", "."); }

TEST_CASE("parser: end parenthesis")
{
  lisp_compare(")", "nil");
  lisp_compare("]", "nil");
  lisp_compare("a)", "a");
}

//
// Numeric objects.
//
TEST_CASE("parser: numbers")
{
  lisp_compare("123", "123");
  lisp_compare("00123", "123");
  lisp_compare("(00000)", "(0)");
}

TEST_CASE("parser: parse")
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

TEST_CASE("parser: dot")
{
  lisp_compare("(list a (cadr x))", "(list a (cadr x))");
  lisp_compare("(lambda (a . x) (list a (cadr x)))", "(lambda (a . x) (list a (cadr x)))");
  lisp_compare("(setq f (lambda (a . x) (list a (cadr x))))", "(setq f (lambda (a . x) (list a (cadr x))))");
}

TEST_CASE("parser: (greaterp 1.0 \"b\")")
{
  lexer lexer{"(greaterp 1.0 \"b\")"};
  auto res = parser(lexer).parse();
  REQUIRE(type_of(res) == object::type::Cons);
  CHECK(car(res) == "greaterp"_a);
  CHECK(cadr(res)->as_double() == 1.0);
  CHECK(caddr(res)->as_string() == "b");
}

TEST_CASE("parser: nil")
{
  lexer r1{"()"};
  lexer r2{"nil"};
  auto res1 = parser(r1).parse();
  auto res2 = parser(r2).parse();
  CHECK(res1 == res2);
}

TEST_CASE("parser: macro")
{
  lexer lexer{"$HOME"};
  lexer.macro('$', "rmgetenv"_l);
  auto r = parser(lexer).parse();
  REQUIRE(r);
  REQUIRE(type_of(r) == object::type::String);
  CHECK(r->as_string().starts_with("/"));
}

TEST_CASE("parser: splice")
{
  lexer lexer{"$HOME"};
  lexer.splice('$', "rmgetenv"_l);
  auto r = parser(lexer).parse();
  REQUIRE(r);
  REQUIRE(type_of(r) == object::type::String);
  CHECK(r->as_string().starts_with("/"));
}

TEST_CASE("parser: infix")
{
  lexer lexer{"$HOME"};
  lexer.infix('$', "rmgetenv"_l);
  auto r = parser(lexer).parse();
  REQUIRE(r);
  REQUIRE(type_of(r) == object::type::String);
  CHECK(r->as_string().starts_with("/"));
}

TEST_CASE("parser: syntax table")
{
  lexer lexer{"[quote]"};
  lexer.set('[', syntax::type::LEFT_PAREN);
  lexer.set(']', syntax::type::RIGHT_PAREN);
  auto t = parser(lexer).parse();
  REQUIRE(t);
  REQUIRE(type_of(t) == object::type::Cons);
  CHECK(t->car() == atoms::QUOTE);
}

TEST_CASE("parser: empty token")
{
  // For coverage
  lexer lexer{""};
  parser parser{lexer};
  token_t token;
  auto empty = parser.create(token);
  CHECK(empty == nil);
}

} // namespace lisp
