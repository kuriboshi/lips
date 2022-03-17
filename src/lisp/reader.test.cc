//
// Lisp reader -- reads an s-expression from the input source.
//
// Copyright 2022 Krister Joas
//

#include <catch2/catch.hpp>
#include <string>
#include "reader.hh"

using namespace std::literals;

namespace
{
using StringReader = lisp::Reader<std::string>;

void reader_check(StringReader& reader, enum lisp::token_t::type type, const std::string& token)
{
  auto b = reader.read();
  REQUIRE(b);
  CHECK(b->type == type);
  CHECK(b->token == token);
}

void reader_check(std::string string, enum lisp::token_t::type type, const std::string& token)
{
  StringReader reader{string};
  auto b = reader.read();
  REQUIRE(b);
  CHECK(b->type == type);
  CHECK(b->token == token);
}
}

namespace lisp
{

TEST_CASE("Reader symbols")
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
}

TEST_CASE("Reader (a b c)")
{
  std::string s{"(a b c)"};
  StringReader reader{s};
  reader_check(reader, token_t::type::MACRO, "(");
  reader_check(reader, token_t::type::SYMBOL, "a");
  reader_check(reader, token_t::type::SYMBOL, "b");
  reader_check(reader, token_t::type::SYMBOL, "c");
  reader_check(reader, token_t::type::MACRO, ")");
  CHECK(!reader.read());
}

TEST_CASE("Reader ( a b c )")
{
  std::string s{"( a b c )"};
  StringReader reader{s};
  reader_check(reader, token_t::type::MACRO, "(");
  reader_check(reader, token_t::type::SYMBOL, "a");
  reader_check(reader, token_t::type::SYMBOL, "b");
  reader_check(reader, token_t::type::SYMBOL, "c");
  reader_check(reader, token_t::type::MACRO, ")");
  CHECK(!reader.read());
}

TEST_CASE("Reader +")
{
  SECTION("+")
  {
    std::string s{"+"};
    StringReader reader{s};
    reader_check(reader, token_t::type::SYMBOL, "+");
  }
  SECTION("(+)")
  {
    std::string s{"(+)"};
    StringReader reader{s};
    reader_check(reader, token_t::type::MACRO, "(");
    reader_check(reader, token_t::type::SYMBOL, "+");
    reader_check(reader, token_t::type::MACRO, ")");
  }
  SECTION("(+ 1 2)")
  {
    std::string s{"(+ 1 2)"};
    StringReader reader{s};
    reader_check(reader, token_t::type::MACRO, "(");
    reader_check(reader, token_t::type::SYMBOL, "+");
    reader_check(reader, token_t::type::INT, "1");
    reader_check(reader, token_t::type::INT, "2");
    reader_check(reader, token_t::type::MACRO, ")");
  }
}

TEST_CASE("Reader strings")
{
  SECTION("\"string\"")
  {
    reader_check("\"string\"", token_t::type::STRING, "string");
  }
  SECTION("\"string\" hello")
  {
    std::string s{"\"string\" hello"};
    StringReader reader{s};
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

TEST_CASE("Reader integers")
{
  reader_check("123", token_t::type::INT, "123");
  // A bit weird so this may change in the future.
  reader_check("123abc", token_t::type::INT, "123");
  reader_check("0123456789", token_t::type::INT, "0123456789");
}

TEST_CASE("Reader floats")
{
  reader_check("1.234", token_t::type::FLOAT, "1.234");
  reader_check("-1.234", token_t::type::FLOAT, "-1.234");
  reader_check("1.23e-2", token_t::type::FLOAT, "1.23e-2");
  reader_check("1.23e+2", token_t::type::FLOAT, "1.23e+2");
  reader_check("1.23E-2", token_t::type::FLOAT, "1.23E-2");
  reader_check("+1.234", token_t::type::FLOAT, "+1.234");
  reader_check(".1.234", token_t::type::SYMBOL, ".1.234");
  reader_check(".1,234", token_t::type::SYMBOL, ".1,234");
  reader_check("1.23e--2", token_t::type::SYMBOL, "1.23e--2");
}

TEST_CASE("Reader unread")
{
  std::string s{"()"};
  StringReader reader{s};
  auto token = reader.read();
  CHECK(token->token == "(");
  reader.unread(*token);
  token = reader.read();
  CHECK(token->token == "(");
  token = reader.read();
  CHECK(token->token == ")");
}

}
