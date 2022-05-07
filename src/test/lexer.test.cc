//
// Lips, lisp shell.
// Copyright 2022 Krister Joas
//
#include <string>
#include <sstream>
#include <catch2/catch.hpp>
#include <lisp/lexer.hh>
#include <lisp/alloc.hh>

namespace
{
void lexer_check(lisp::lexer& lexer, enum lisp::token_t::type type, const std::string& token)
{
  auto b = lexer.read();
  REQUIRE(b);
  CHECK(b.type == type);
  CHECK(b.token == token);
}

void lexer_check(std::string string, enum lisp::token_t::type type, const std::string& token)
{
  lisp::lexer lexer{string};
  auto b = lexer.read();
  REQUIRE(b);
  CHECK(b.type == type);
  CHECK(b.token == token);
}
} // namespace

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
  lexer lexer{"(a b c)"};
  lexer_check(lexer, token_t::type::SPECIAL, "(");
  lexer_check(lexer, token_t::type::SYMBOL, "a");
  lexer_check(lexer, token_t::type::SYMBOL, "b");
  lexer_check(lexer, token_t::type::SYMBOL, "c");
  lexer_check(lexer, token_t::type::SPECIAL, ")");
  CHECK(!lexer.read());
}

TEST_CASE("lexer: ( a b c )")
{
  lexer lexer{"( a b c )"};
  lexer_check(lexer, token_t::type::SPECIAL, "(");
  lexer_check(lexer, token_t::type::SYMBOL, "a");
  lexer_check(lexer, token_t::type::SYMBOL, "b");
  lexer_check(lexer, token_t::type::SYMBOL, "c");
  lexer_check(lexer, token_t::type::SPECIAL, ")");
  CHECK(!lexer.read());
}

TEST_CASE("lexer: +")
{
  SECTION("+")
  {
    lexer lexer{"+"};
    lexer_check(lexer, token_t::type::SYMBOL, "+");
  }

  SECTION("(+)")
  {
    lexer lexer{"(+)"};
    lexer_check(lexer, token_t::type::SPECIAL, "(");
    lexer_check(lexer, token_t::type::SYMBOL, "+");
    lexer_check(lexer, token_t::type::SPECIAL, ")");
  }

  SECTION("(+ 1 2)")
  {
    lexer lexer{"(+ 1 2)"};
    lexer_check(lexer, token_t::type::SPECIAL, "(");
    lexer_check(lexer, token_t::type::SYMBOL, "+");
    lexer_check(lexer, token_t::type::INT, "1");
    lexer_check(lexer, token_t::type::INT, "2");
    lexer_check(lexer, token_t::type::SPECIAL, ")");
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
    lexer lexer{"\"string\" hello"};
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
  lexer lexer{"()"};
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
    lexer lexer{"# comment\nhello\n# another comment\nworld"};
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
    lexer lexer{" # comment\nhello\n"};
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
    lexer lexer{" #'plus\n"};
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
    lexer lexer{"(;comment\n)"};
    auto token = lexer.read();
    REQUIRE(token);
    CHECK(token.is_special('('));
    token = lexer.read();
    REQUIRE(token);
    CHECK(token.is_special(')'));
  }

  SECTION("inside s-expr with int")
  {
    lexer lexer{"(100;comment\n200)"};
    auto token = lexer.read();
    REQUIRE(token);
    CHECK(token.is_special('('));
    token = lexer.read();
    REQUIRE(token);
    CHECK(token.token == "100");
    token = lexer.read();
    REQUIRE(token);
    CHECK(token.token == "200");
    token = lexer.read();
    REQUIRE(token);
    CHECK(token.is_special(')'));
  }

  SECTION("inside s-expr with int")
  {
    lexer lexer{"(symbol;comment\n100)"};
    auto token = lexer.read();
    REQUIRE(token);
    CHECK(token.is_special('('));
    token = lexer.read();
    REQUIRE(token);
    token_t symbol{token_t::type::SYMBOL, "symbol"};
    CHECK(token == symbol);
    token = lexer.read();
    REQUIRE(token);
    CHECK(token.token == "100");
    token = lexer.read();
    REQUIRE(token);
    CHECK(token.is_special(')'));
  }

  SECTION("inside s-expr with float")
  {
    lexer lexer{"(1.23;comment\n100)"};
    auto token = lexer.read();
    REQUIRE(token);
    CHECK(token.is_special('('));
    token = lexer.read();
    REQUIRE(token);
    token_t f0{token_t::type::FLOAT, "1.23"};
    CHECK(token == f0);
    token = lexer.read();
    REQUIRE(token);
    CHECK(token.token == "100");
    token = lexer.read();
    REQUIRE(token);
    CHECK(token.is_special(')'));
  }

  SECTION("inside s-expr with exponent")
  {
    lexer lexer{"(1.23e1;comment\n100)"};
    auto token = lexer.read();
    REQUIRE(token);
    CHECK(token.is_special('('));
    token = lexer.read();
    REQUIRE(token);
    token_t f0{token_t::type::FLOAT, "1.23e1"};
    CHECK(token == f0);
    token = lexer.read();
    REQUIRE(token);
    CHECK(token.token == "100");
    token = lexer.read();
    REQUIRE(token);
    CHECK(token.is_special(')'));
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

  SECTION("SPECIAL")
  {
    std::ostringstream os;
    token_t token{token_t::type::SPECIAL, ")"};
    os << token;
    CHECK(os.str() == "SPECIAL:)");
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

  SECTION("SPECIAL")
  {
    token_t token0{token_t::type::SPECIAL, ")"};
    token_t token1{token_t::type::SPECIAL, ")"};
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

TEST_CASE("lexer: macro")
{
  lexer lexer{"$HOME"};
  lexer.set('$', "rmgetenv"_l);
  auto t = lexer.read();
  REQUIRE(t);
  CHECK(t.token == "$");
}

}
