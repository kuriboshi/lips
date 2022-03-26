//
// Lips, lisp shell.
//
// Copyright 2022 Krister Joas
//

#include <catch2/catch.hpp>
#include <string>
#include <sstream>
#include "reader.hh"

using namespace std::literals;

namespace
{
using string_reader = lisp::reader<std::string>;

void reader_check(string_reader& reader, enum lisp::token_t::type type, const std::string& token)
{
  auto b = reader.read();
  REQUIRE(b);
  CHECK(b.type == type);
  CHECK(b.token == token);
}

void reader_check(std::string string, enum lisp::token_t::type type, const std::string& token)
{
  string_reader reader{string};
  auto b = reader.read();
  REQUIRE(b);
  CHECK(b.type == type);
  CHECK(b.token == token);
}
}

namespace lisp
{

TEST_CASE("reader: symbols")
{
  SECTION("Symbol a.")
  {
    reader_check("a.", token_t::type::SYMBOL, "a.");
  }

  SECTION("Symbol .a")
  {
    reader_check(".a", token_t::type::SYMBOL, ".a");
  }

  SECTION("Symbol a01")
  {
    reader_check("a01", token_t::type::SYMBOL, "a01");
  }

  SECTION("Symbol sym\\(bol")
  {
    reader_check("sym\\(bol", token_t::type::SYMBOL, "sym(bol");
  }
}

TEST_CASE("reader: (a b c)")
{
  std::string s{"(a b c)"};
  string_reader reader{s};
  reader_check(reader, token_t::type::MACRO, "(");
  reader_check(reader, token_t::type::SYMBOL, "a");
  reader_check(reader, token_t::type::SYMBOL, "b");
  reader_check(reader, token_t::type::SYMBOL, "c");
  reader_check(reader, token_t::type::MACRO, ")");
  CHECK(!reader.read());
}

TEST_CASE("reader: ( a b c )")
{
  std::string s{"( a b c )"};
  string_reader reader{s};
  reader_check(reader, token_t::type::MACRO, "(");
  reader_check(reader, token_t::type::SYMBOL, "a");
  reader_check(reader, token_t::type::SYMBOL, "b");
  reader_check(reader, token_t::type::SYMBOL, "c");
  reader_check(reader, token_t::type::MACRO, ")");
  CHECK(!reader.read());
}

TEST_CASE("reader: +")
{
  SECTION("+")
  {
    std::string s{"+"};
    string_reader reader{s};
    reader_check(reader, token_t::type::SYMBOL, "+");
  }

  SECTION("(+)")
  {
    std::string s{"(+)"};
    string_reader reader{s};
    reader_check(reader, token_t::type::MACRO, "(");
    reader_check(reader, token_t::type::SYMBOL, "+");
    reader_check(reader, token_t::type::MACRO, ")");
  }

  SECTION("(+ 1 2)")
  {
    std::string s{"(+ 1 2)"};
    string_reader reader{s};
    reader_check(reader, token_t::type::MACRO, "(");
    reader_check(reader, token_t::type::SYMBOL, "+");
    reader_check(reader, token_t::type::INT, "1");
    reader_check(reader, token_t::type::INT, "2");
    reader_check(reader, token_t::type::MACRO, ")");
  }
}

TEST_CASE("reader: strings")
{
  SECTION("\"string\"")
  {
    reader_check("\"string\"", token_t::type::STRING, "string");
  }

  SECTION("\"string\" hello")
  {
    std::string s{"\"string\" hello"};
    string_reader reader{s};
    reader_check(reader, token_t::type::STRING, "string");
    reader_check(reader, token_t::type::SYMBOL, "hello");
  }

  SECTION("\"st\\\"ring\"")
  {
    reader_check("\"st\\\"ring\"", token_t::type::STRING, "st\"ring");
  }

  SECTION("\"st\\\\ring\"")
  {
    reader_check("\"st\\\\ring\"", token_t::type::STRING, "st\\ring");
  }

  SECTION("\"st\\\\\\\"ring\"")
  {
    reader_check("\"st\\\\\\\"ring\"", token_t::type::STRING, "st\\\"ring");
  }

  SECTION("\"(hello)\"")
  {
    reader_check("\"(hello)\"", token_t::type::STRING, "(hello)");
  }
}

TEST_CASE("reader: integers")
{
  reader_check("123", token_t::type::INT, "123");
  reader_check("123abc", token_t::type::SYMBOL, "123abc");
  reader_check("0123456789", token_t::type::INT, "0123456789");
  reader_check("+0123456789", token_t::type::INT, "+0123456789");
  reader_check("-0123456789", token_t::type::INT, "-0123456789");
}

TEST_CASE("reader: floats")
{
  reader_check("1.0 ", token_t::type::FLOAT, "1.0");
  reader_check("1.0)", token_t::type::FLOAT, "1.0");
  reader_check("1.0(", token_t::type::FLOAT, "1.0");
  reader_check("1.234", token_t::type::FLOAT, "1.234");
  reader_check("-1.234", token_t::type::FLOAT, "-1.234");
  reader_check("1.23e-2", token_t::type::FLOAT, "1.23e-2");
  reader_check("1.23e+2", token_t::type::FLOAT, "1.23e+2");
  reader_check("1.23E-2", token_t::type::FLOAT, "1.23E-2");
  reader_check("+1.234", token_t::type::FLOAT, "+1.234");
  reader_check("123e-2", token_t::type::FLOAT, "123e-2");
  reader_check(".1.234", token_t::type::SYMBOL, ".1.234");
  reader_check(".1,234", token_t::type::SYMBOL, ".1,234");
  reader_check("1.23e--2", token_t::type::SYMBOL, "1.23e--2");
  reader_check("12e2abc", token_t::type::SYMBOL, "12e2abc");
}

TEST_CASE("reader: unread")
{
  std::string s{"()"};
  string_reader reader{s};
  auto token = reader.read();
  REQUIRE(token);
  CHECK(token.token == "(");
  reader.unread(token);
  token = reader.read();
  REQUIRE(token);
  CHECK(token.token == "(");
  token = reader.read();
  REQUIRE(token);
  CHECK(token.token == ")");
}

TEST_CASE("reader: comments")
{
  SECTION("inside s-expr")
  {
    std::string s{"(#comment\n)"};
    string_reader reader{s};
    auto token = reader.read();
    REQUIRE(token);
    CHECK(token.is_macro('('));
    token = reader.read();
    REQUIRE(token);
    CHECK(token.is_macro(')'));
  }

  SECTION("inside s-expr with int")
  {
    std::string s{"(100#comment\n200)"};
    string_reader reader{s};
    auto token = reader.read();
    REQUIRE(token);
    CHECK(token.is_macro('('));
    token = reader.read();
    REQUIRE(token);
    CHECK(token.token == "100");
    token = reader.read();
    REQUIRE(token);
    CHECK(token.token == "200");
    token = reader.read();
    REQUIRE(token);
    CHECK(token.is_macro(')'));
  }

  SECTION("inside s-expr with int")
  {
    std::string s{"(symbol#comment\n100)"};
    string_reader reader{s};
    auto token = reader.read();
    REQUIRE(token);
    CHECK(token.is_macro('('));
    token = reader.read();
    REQUIRE(token);
    token_t symbol{token_t::type::SYMBOL, "symbol"};
    CHECK(token == symbol);
    token = reader.read();
    REQUIRE(token);
    CHECK(token.token == "100");
    token = reader.read();
    REQUIRE(token);
    CHECK(token.is_macro(')'));
  }

  SECTION("inside s-expr with float")
  {
    std::string s{"(1.23#comment\n100)"};
    string_reader reader{s};
    auto token = reader.read();
    REQUIRE(token);
    CHECK(token.is_macro('('));
    token = reader.read();
    REQUIRE(token);
    token_t f0{token_t::type::FLOAT, "1.23"};
    CHECK(token == f0);
    token = reader.read();
    REQUIRE(token);
    CHECK(token.token == "100");
    token = reader.read();
    REQUIRE(token);
    CHECK(token.is_macro(')'));
  }

  SECTION("inside s-expr with exponent")
  {
    std::string s{"(1.23e1#comment\n100)"};
    string_reader reader{s};
    auto token = reader.read();
    REQUIRE(token);
    CHECK(token.is_macro('('));
    token = reader.read();
    REQUIRE(token);
    token_t f0{token_t::type::FLOAT, "1.23e1"};
    CHECK(token == f0);
    token = reader.read();
    REQUIRE(token);
    CHECK(token.token == "100");
    token = reader.read();
    REQUIRE(token);
    CHECK(token.is_macro(')'));
  }
}

TEST_CASE("reader: operator<<")
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

TEST_CASE("reader: operator==")
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
