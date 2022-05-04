//
// Lips, lisp shell.
// Copyright 2022 Krister Joas
//
#include <string>
#include <sstream>
#include <catch2/catch.hpp>
#include <lisp/lexer.hh>

namespace
{
using string_lexer = lisp::lexer<std::string>;

void lexer_check(string_lexer& lexer, enum lisp::token_t::type type, const std::string& token)
{
  auto b = lexer.read();
  REQUIRE(b);
  CHECK(b.type == type);
  CHECK(b.token == token);
}

void lexer_check(std::string string, enum lisp::token_t::type type, const std::string& token)
{
  string_lexer lexer{string};
  auto b = lexer.read();
  REQUIRE(b);
  CHECK(b.type == type);
  CHECK(b.token == token);
}
}

namespace lisp
{

TEST_CASE("lexer: symbols")
{
  SECTION("Symbol a.")
  {
    lexer_check("a.", token_t::type::SYMBOL, "a.");
  }

  SECTION("Symbol .a")
  {
    lexer_check(".a", token_t::type::SYMBOL, ".a");
  }

  SECTION("Symbol a01")
  {
    lexer_check("a01", token_t::type::SYMBOL, "a01");
  }

  SECTION("Symbol sym\\(bol")
  {
    lexer_check("sym\\(bol", token_t::type::SYMBOL, "sym(bol");
  }
}

TEST_CASE("lexer: (a b c)")
{
  std::string s{"(a b c)"};
  string_lexer lexer{s};
  lexer_check(lexer, token_t::type::MACRO, "(");
  lexer_check(lexer, token_t::type::SYMBOL, "a");
  lexer_check(lexer, token_t::type::SYMBOL, "b");
  lexer_check(lexer, token_t::type::SYMBOL, "c");
  lexer_check(lexer, token_t::type::MACRO, ")");
  CHECK(!lexer.read());
}

TEST_CASE("lexer: ( a b c )")
{
  std::string s{"( a b c )"};
  string_lexer lexer{s};
  lexer_check(lexer, token_t::type::MACRO, "(");
  lexer_check(lexer, token_t::type::SYMBOL, "a");
  lexer_check(lexer, token_t::type::SYMBOL, "b");
  lexer_check(lexer, token_t::type::SYMBOL, "c");
  lexer_check(lexer, token_t::type::MACRO, ")");
  CHECK(!lexer.read());
}

TEST_CASE("lexer: +")
{
  SECTION("+")
  {
    std::string s{"+"};
    string_lexer lexer{s};
    lexer_check(lexer, token_t::type::SYMBOL, "+");
  }

  SECTION("(+)")
  {
    std::string s{"(+)"};
    string_lexer lexer{s};
    lexer_check(lexer, token_t::type::MACRO, "(");
    lexer_check(lexer, token_t::type::SYMBOL, "+");
    lexer_check(lexer, token_t::type::MACRO, ")");
  }

  SECTION("(+ 1 2)")
  {
    std::string s{"(+ 1 2)"};
    string_lexer lexer{s};
    lexer_check(lexer, token_t::type::MACRO, "(");
    lexer_check(lexer, token_t::type::SYMBOL, "+");
    lexer_check(lexer, token_t::type::INT, "1");
    lexer_check(lexer, token_t::type::INT, "2");
    lexer_check(lexer, token_t::type::MACRO, ")");
  }
}

TEST_CASE("lexer: strings")
{
  SECTION("\"string\"")
  {
    lexer_check("\"string\"", token_t::type::STRING, "string");
  }

  SECTION("\"string\" hello")
  {
    std::string s{"\"string\" hello"};
    string_lexer lexer{s};
    lexer_check(lexer, token_t::type::STRING, "string");
    lexer_check(lexer, token_t::type::SYMBOL, "hello");
  }

  SECTION("\"st\\\"ring\"")
  {
    lexer_check("\"st\\\"ring\"", token_t::type::STRING, "st\"ring");
  }

  SECTION("\"st\\\\ring\"")
  {
    lexer_check("\"st\\\\ring\"", token_t::type::STRING, "st\\ring");
  }

  SECTION("\"st\\\\\\\"ring\"")
  {
    lexer_check("\"st\\\\\\\"ring\"", token_t::type::STRING, "st\\\"ring");
  }

  SECTION("\"(hello)\"")
  {
    lexer_check("\"(hello)\"", token_t::type::STRING, "(hello)");
  }
}

TEST_CASE("lexer: integers")
{
  lexer_check("123", token_t::type::INT, "123");
  lexer_check("123abc", token_t::type::SYMBOL, "123abc");
  lexer_check("0123456789", token_t::type::INT, "0123456789");
  lexer_check("+0123456789", token_t::type::INT, "+0123456789");
  lexer_check("-0123456789", token_t::type::INT, "-0123456789");
}

TEST_CASE("lexer: floats")
{
  lexer_check("1.0 ", token_t::type::FLOAT, "1.0");
  lexer_check("1.0)", token_t::type::FLOAT, "1.0");
  lexer_check("1.0(", token_t::type::FLOAT, "1.0");
  lexer_check("1.234", token_t::type::FLOAT, "1.234");
  lexer_check("-1.234", token_t::type::FLOAT, "-1.234");
  lexer_check("1.23e-2", token_t::type::FLOAT, "1.23e-2");
  lexer_check("1.23e+2", token_t::type::FLOAT, "1.23e+2");
  lexer_check("1.23E-2", token_t::type::FLOAT, "1.23E-2");
  lexer_check("+1.234", token_t::type::FLOAT, "+1.234");
  lexer_check("123e-2", token_t::type::FLOAT, "123e-2");
  lexer_check(".1.234", token_t::type::SYMBOL, ".1.234");
  lexer_check(".1,234", token_t::type::SYMBOL, ".1,234");
  lexer_check("1.23e--2", token_t::type::SYMBOL, "1.23e--2");
  lexer_check("12e2abc", token_t::type::SYMBOL, "12e2abc");
}

TEST_CASE("lexer: unread")
{
  std::string s{"()"};
  string_lexer lexer{s};
  auto token = lexer.read();
  REQUIRE(token);
  CHECK(token.token == "(");
  lexer.unread(token);
  token = lexer.read();
  REQUIRE(token);
  CHECK(token.token == "(");
  token = lexer.read();
  REQUIRE(token);
  CHECK(token.token == ")");
}

TEST_CASE("lexer: comments")
{
  SECTION("# at start of line")
  {
    std::string s{"# comment\nhello\n# another comment\nworld"};
    string_lexer lexer{s};
    auto token = lexer.read();
    REQUIRE(token);
    CHECK(token.type == token_t::type::SYMBOL);
    CHECK(token.token == "hello");
    token = lexer.read();
    REQUIRE(token);
    CHECK(token.type == token_t::type::SYMBOL);
    CHECK(token.token == "world");
  }

  SECTION("# not at start of line")
  {
    std::string s{" # comment\nhello\n"};
    string_lexer lexer{s};
    auto token = lexer.read();
    REQUIRE(token);
    CHECK(token.type == token_t::type::SYMBOL);
    CHECK(token.token == "#");
    token = lexer.read();
    REQUIRE(token);
    CHECK(token.type == token_t::type::SYMBOL);
    CHECK(token.token == "comment");
    token = lexer.read();
    REQUIRE(token);
    CHECK(token.type == token_t::type::SYMBOL);
    CHECK(token.token == "hello");
  }

  SECTION("#' token")
  {
    std::string s{" #'plus\n"};
    string_lexer lexer{s};
    auto token = lexer.read();
    REQUIRE(token);
    CHECK(token.type == token_t::type::SYMBOL);
    CHECK(token.token == "#'");
    token = lexer.read();
    REQUIRE(token);
    CHECK(token.type == token_t::type::SYMBOL);
    CHECK(token.token == "plus");
  }

  SECTION("inside s-expr")
  {
    std::string s{"(;comment\n)"};
    string_lexer lexer{s};
    auto token = lexer.read();
    REQUIRE(token);
    CHECK(token.is_macro('('));
    token = lexer.read();
    REQUIRE(token);
    CHECK(token.is_macro(')'));
  }

  SECTION("inside s-expr with int")
  {
    std::string s{"(100;comment\n200)"};
    string_lexer lexer{s};
    auto token = lexer.read();
    REQUIRE(token);
    CHECK(token.is_macro('('));
    token = lexer.read();
    REQUIRE(token);
    CHECK(token.token == "100");
    token = lexer.read();
    REQUIRE(token);
    CHECK(token.token == "200");
    token = lexer.read();
    REQUIRE(token);
    CHECK(token.is_macro(')'));
  }

  SECTION("inside s-expr with int")
  {
    std::string s{"(symbol;comment\n100)"};
    string_lexer lexer{s};
    auto token = lexer.read();
    REQUIRE(token);
    CHECK(token.is_macro('('));
    token = lexer.read();
    REQUIRE(token);
    token_t symbol{token_t::type::SYMBOL, "symbol"};
    CHECK(token == symbol);
    token = lexer.read();
    REQUIRE(token);
    CHECK(token.token == "100");
    token = lexer.read();
    REQUIRE(token);
    CHECK(token.is_macro(')'));
  }

  SECTION("inside s-expr with float")
  {
    std::string s{"(1.23;comment\n100)"};
    string_lexer lexer{s};
    auto token = lexer.read();
    REQUIRE(token);
    CHECK(token.is_macro('('));
    token = lexer.read();
    REQUIRE(token);
    token_t f0{token_t::type::FLOAT, "1.23"};
    CHECK(token == f0);
    token = lexer.read();
    REQUIRE(token);
    CHECK(token.token == "100");
    token = lexer.read();
    REQUIRE(token);
    CHECK(token.is_macro(')'));
  }

  SECTION("inside s-expr with exponent")
  {
    std::string s{"(1.23e1;comment\n100)"};
    string_lexer lexer{s};
    auto token = lexer.read();
    REQUIRE(token);
    CHECK(token.is_macro('('));
    token = lexer.read();
    REQUIRE(token);
    token_t f0{token_t::type::FLOAT, "1.23e1"};
    CHECK(token == f0);
    token = lexer.read();
    REQUIRE(token);
    CHECK(token.token == "100");
    token = lexer.read();
    REQUIRE(token);
    CHECK(token.is_macro(')'));
  }
}

TEST_CASE("lexer: operator<<")
{
  SECTION("EMPTY")
  {
    std::ostringstream os;
    token_t token{token_t::type::EMPTY};
    os << token;
    CHECK(os.str() == "?:");
  }

  SECTION("MACRO")
  {
    std::ostringstream os;
    token_t token{token_t::type::MACRO, ")"};
    os << token;
    CHECK(os.str() == "MACRO:)");
  }

  SECTION("STRING")
  {
    std::ostringstream os;
    token_t token{token_t::type::STRING, "string"};
    os << token;
    CHECK(os.str() == "STRING:string");
  }

  SECTION("SYMBOL")
  {
    std::ostringstream os;
    token_t token{token_t::type::SYMBOL, "symbol"};
    os << token;
    CHECK(os.str() == "SYMBOL:symbol");
  }

  SECTION("INT")
  {
    std::ostringstream os;
    token_t token{token_t::type::INT, "123"};
    os << token;
    CHECK(os.str() == "INT:123");
  }

  SECTION("FLOAT")
  {
    std::ostringstream os;
    token_t token{token_t::type::FLOAT, "1.234"};
    os << token;
    CHECK(os.str() == "FLOAT:1.234");
  }
}

TEST_CASE("lexer: operator==")
{
  SECTION("EMPTY")
  {
    token_t token0;
    token_t token1{token_t::type::EMPTY};
    CHECK(token0 == token1);
  }

  SECTION("MACRO")
  {
    token_t token0{token_t::type::MACRO, ")"};
    token_t token1{token_t::type::MACRO, ")"};
    CHECK(token0 == token1);
  }

  SECTION("STRING")
  {
    token_t token0{token_t::type::STRING, "string"};
    token_t token1{token_t::type::STRING, "string"};
    CHECK(token0 == token1);
  }

  SECTION("SYMBOL")
  {
    token_t token0{token_t::type::SYMBOL, "symbol"};
    token_t token1{token_t::type::SYMBOL, "symbol"};
    CHECK(token0 == token1);
  }

  SECTION("INT")
  {
    token_t token0{token_t::type::INT, "123"};
    token_t token1{token_t::type::INT, "123"};
    CHECK(token0 == token1);
  }

  SECTION("FLOAT")
  {
    token_t token0{token_t::type::FLOAT, "1.234"};
    token_t token1{token_t::type::FLOAT, "1.234"};
    CHECK(token0 == token1);
  }
}

}
